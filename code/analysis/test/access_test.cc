/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include <gtest/gtest.h>
#include <limits>
#include <algorithm>

#include "insieme/utils/logging.h"
#include "insieme/analysis/access/access.h"
#include "insieme/analysis/access/access_mgr.h"
#include "insieme/analysis/access/unified_address.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/analysis/cfg.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::access;

TEST(UnifiedAddress, IRAddress) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	{
		auto addr = builder.parseAddresses("int<4> a = $10+20$;");

		EXPECT_EQ(1u, addr.size());

		EXPECT_EQ(
			addr[0].getAddressedNode(), 
			UnifiedAddress(addr[0]).getAddressedNode()
		);
	}
	
	{
		auto addr = builder.parseAddresses("$int<4> a = $10+20$;$");

		EXPECT_EQ(2u, addr.size());
		CFGPtr cfg = CFG::buildCFG<MultiStmtPerBasicBlock>(addr[0]);

		{ 
			auto cfgAddr = cfg->find(addr[1]);
			EXPECT_EQ("<2:0:0-1>", toString(cfgAddr));
		}
		{ 
			auto cfgAddr = cfg->find(addr[0]);
			EXPECT_EQ("<2:0:0>", toString(cfgAddr));
		}

	}

}

TEST(Access, Scalars) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	{
		auto code = builder.parseAddresses(
			"{ "
			"	ref<int<4>> a;"
			"	$a$;"
			"}"
		);

		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto access = getImmediateAccess( mgr, accessAddr );

		EXPECT_EQ(AccessType::AT_BASE, access->getType());
		EXPECT_TRUE(access->isBaseAccess());
		EXPECT_TRUE(access->isReference());

		auto access2 = getImmediateAccess( mgr, accessAddr );
		EXPECT_EQ(AccessType::AT_BASE, access2->getType());
		EXPECT_EQ(*access, *access2);

		EXPECT_TRUE(equalPath(access, access2));

		AccessManager mgr;
		auto cl1 = mgr.getClassFor(access);
		auto cl2 = mgr.getClassFor(access2);

		EXPECT_EQ(1u, mgr.size());
		EXPECT_EQ(cl1, cl2);
	}

	{
		auto code = builder.parseAddresses(
			"{"
			"	int<4> a;"
			"	$a$;"
			"}"
		);

		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess( mgr, accessAddr );
		EXPECT_EQ(AccessType::AT_BASE, access->getType());
		EXPECT_FALSE(access->isReference());

		AccessManager mgr;
		auto accClassSet1 = mgr.getClassFor(access);
		EXPECT_EQ(1u, accClassSet1.size());
		EXPECT_EQ(0u, (*accClassSet1.begin())->getUID());

		auto accClassSet2 = mgr.getClassFor(access);
		EXPECT_EQ(1u, accClassSet2.size());
		EXPECT_EQ(0u, (*accClassSet2.begin())->getUID());

		EXPECT_EQ(accClassSet1, accClassSet2);
	}

	{
		auto code = builder.parseAddresses(
			"{"
			"	ref<int<4>> a;"
			"	$*$a$$;"
			"}"
		);
		
		EXPECT_EQ(2u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto varAddr = code[1].as<VariableAddress>();

		auto access = getImmediateAccess(  mgr, accessAddr );
		EXPECT_FALSE(access->isReference());
		EXPECT_FALSE(access->isBaseAccess());
	}


	{
		auto code = builder.parseAddresses(
			"{"
			"	ref<int<4>> a;"
			"	$var(*a)$;"
			"}"
		);
		
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess(  mgr, accessAddr );
		EXPECT_TRUE(access->isReference());
		EXPECT_FALSE(access->isBaseAccess());

		EXPECT_TRUE(getRoot(access)->isReference());
	}


	{
		auto code = builder.parseAddresses(
			"{"
			"	ref<struct{int<4> a; int<4> b;}> s;"
			"	$s$;"
			"}"
		);

		EXPECT_EQ(1u, code.size());
		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess( mgr, accessAddr );
		EXPECT_TRUE(access->isReference());
		EXPECT_TRUE(access->isBaseAccess());

		
	}

}

TEST(Access, ScalarAliasedAccess) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	{
		auto code = builder.parseAddresses(
			"{"
			"	ref<int<4>> a;"
			"	$*$a$$;"
			"	$ref<int<4>> b = a;$"
			"   $b$;"
			"}"
		);
		
		EXPECT_EQ(4u, code.size());

		auto accessAddr1 = code[0].as<ExpressionAddress>();
		auto accessAddr2= code[3].as<ExpressionAddress>();
		auto declAddr = code[2].as<DeclarationStmtAddress>();
		auto varAddr = code[1].as<VariableAddress>();
		
		// Create an alias for the expression c = b+a;
		TmpVarMap map;
		map.storeTmpVar( declAddr->getInitialization(), declAddr->getVariable().getAddressedNode() );

		auto access1 = getImmediateAccess( mgr, accessAddr1 );
		auto access2 = getImmediateAccess( mgr, accessAddr2 );

		AccessManager mgr(nullptr, map);
		auto accClassSet1 = mgr.getClassFor(access1);
		EXPECT_EQ(1u, accClassSet1.size());
		EXPECT_EQ(0u, (*accClassSet1.begin())->getUID());

		auto accClassSet2 = mgr.getClassFor(access2);
		EXPECT_EQ(1u, accClassSet2.size());
		EXPECT_EQ(0u, (*accClassSet2.begin())->getUID());

		EXPECT_EQ(accClassSet1, accClassSet2);
		EXPECT_EQ(accClassSet1, accClassSet2);

		EXPECT_EQ(3u, (*accClassSet1.begin())->size());
	}


}

TEST(Access, MemberAccess) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	{
		auto code = builder.parseAddresses(
			"{ "
			"	ref<struct{ int<4> a; int<4> b; }> s;"
			"	$$s$.a$;"
			"}"
		);

		EXPECT_EQ(2u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto varAddr = code[1].as<VariableAddress>();

		auto access = getImmediateAccess( mgr, accessAddr );
		
		EXPECT_TRUE(access->isReference());
		EXPECT_FALSE(access->isBaseAccess());

		auto memberAccess = cast<Member>(access);
		EXPECT_TRUE(!!memberAccess);

		EXPECT_EQ("a", toString(*memberAccess->getMember()));
	}

	{
		auto code = builder.parseAddresses(
			"{ "
			"	struct{ int<4> a; int<4> b; } s;"
			"	$$s$.a$;"
			"	$s.b$;"
			"	$s.a$;"
			"}"
		);
		EXPECT_EQ(4u, code.size());

		auto accessAddr1 = code[0].as<ExpressionAddress>();
		auto accessAddr2 = code[2].as<ExpressionAddress>();
		auto accessAddr3 = code[3].as<ExpressionAddress>();
		auto varAddr = code[1].as<VariableAddress>();

		auto access1 = getImmediateAccess(mgr, accessAddr1);
		auto access2 = getImmediateAccess(mgr, accessAddr2);
		auto access3 = getImmediateAccess(mgr, accessAddr3);

		EXPECT_EQ(UnifiedAddress(accessAddr1), access1->getAddress());
		EXPECT_EQ(AccessType::AT_MEMBER, access1->getType());

		EXPECT_EQ(UnifiedAddress(accessAddr2), access2->getAddress());
		EXPECT_EQ(AccessType::AT_MEMBER, access2->getType());
		EXPECT_NE(access1, access2);
		
		EXPECT_EQ(UnifiedAddress(accessAddr3), access3->getAddress());
		EXPECT_EQ(AccessType::AT_MEMBER, access3->getType());
		EXPECT_NE(access2, access3);
		EXPECT_NE(access1, access3);

		EXPECT_FALSE(equalPath(access1, access2));
		EXPECT_FALSE(equalPath(access3, access2));
		EXPECT_TRUE(equalPath(access1, access3));
	}
}

TEST(Access, CompoundMemberAccess) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	{
		auto code = builder.parseAddresses(
			"{ "
			"	ref<struct{ int<4> a; struct{ int<4> b; int<4> c;} b; }> s;"
			"	$ref<struct{int<4> b; int<4> c;}> t1 = s.b;$"
			"	$t1.b$;"
			"}"
		);

		EXPECT_EQ(2u, code.size());

		// Retrieve addresses for access 
		auto declAddr   = code[0].as<DeclarationStmtAddress>();
		auto accessAddr = code[1].as<ExpressionAddress>();

		// get access 
		auto access = getImmediateAccess(mgr, accessAddr);

		EXPECT_EQ(UnifiedAddress(accessAddr), access->getAddress());

		EXPECT_EQ(declAddr->getVariable().getAddressedNode(), getRoot(access)->getVariable());
		EXPECT_EQ(AccessType::AT_MEMBER, access->getType());

		EXPECT_TRUE(access->isReference());

		TmpVarMap tmpVarMap;
		tmpVarMap.storeTmpVar(declAddr->getInitialization(), declAddr->getVariable().getAddressedNode());

		AccessManager mgr(NULL, tmpVarMap);
		auto clSet1 = mgr.getClassFor(access);

		EXPECT_EQ(1u, clSet1.size());
		auto cl1 = *clSet1.begin();
		EXPECT_EQ(1u, cl1->size());
		EXPECT_EQ(2u, cl1->getUID());

		EXPECT_EQ(3u, mgr.size());

		auto& cl2 = mgr[0];

		EXPECT_EQ(1u, cl2.size());
		EXPECT_NE(*cl1, cl2);
	}

}

TEST(Access, CompoundMemberAccess2) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	{
		auto code = builder.parseAddresses(
			"{ "
			"	ref<struct{ int<4> a; struct{ int<4> b; int<4> c;} b; }> s;"
			"	$ref<struct{int<4> b; int<4> c;}> t1 = s.b;$"
			"	$t1.b$;"
			"	$ref<struct{int<4> b; int<4> c;}> t2 = s.b;$"
			"	$t2.b$;"
			"}"
		);

		EXPECT_EQ(4u, code.size());

		// extract addresses for the first access 
		auto declAddr1 = code[0].as<DeclarationStmtAddress>();
		auto accessAddr1 = code[1].as<ExpressionAddress>();

		// extract addresses for the second access 
		auto declAddr2 = code[2].as<DeclarationStmtAddress>();
		auto accessAddr2 = code[3].as<ExpressionAddress>();

		auto access1 = getImmediateAccess(mgr, accessAddr1);
		auto access2 = getImmediateAccess(mgr, accessAddr2);

		EXPECT_EQ(UnifiedAddress(accessAddr1), access1->getAddress());
		EXPECT_EQ(AccessType::AT_MEMBER, access1->getType());

		EXPECT_EQ(UnifiedAddress(accessAddr2), access2->getAddress());
		EXPECT_EQ(AccessType::AT_MEMBER, access2->getType());
		EXPECT_NE(access1, access2);
		
		// store the alias mappings manually 
		TmpVarMap tmpVarMap;
		tmpVarMap.storeTmpVar(declAddr1->getInitialization(), declAddr1->getVariable().getAddressedNode());
		tmpVarMap.storeTmpVar(declAddr2->getInitialization(), declAddr2->getVariable().getAddressedNode());

		AccessManager mgr(NULL, tmpVarMap);
		auto clSet1 = mgr.getClassFor(access1);
		auto clSet2 = mgr.getClassFor(access2);

		EXPECT_EQ(1u, clSet1.size());
		EXPECT_EQ(1u, clSet2.size());

		auto cl1 = *clSet1.begin();
		auto cl2 = *clSet2.begin();

		EXPECT_EQ(3u, mgr.size());
		EXPECT_EQ(2u, cl1->getUID());

		EXPECT_EQ(clSet1, clSet2);
	}
}


TEST(Access, ArrayAccess) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	{	
		auto code = builder.parseAddresses(
			"{ "
			"	ref<array<int<4>,1>> v;"
			"	$$v$[2u]$;"
			"}"
		);
		EXPECT_EQ(2u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto access = getImmediateAccess( mgr, accessAddr );
		auto varAddr = code[1].as<ExpressionAddress>();
		
		EXPECT_EQ(UnifiedAddress(accessAddr), access->getAddress());
		EXPECT_EQ(AccessType::AT_SUBSCRIPT, access->getType());
		EXPECT_EQ(varAddr.getAddressedNode(), getRoot(access)->getVariable());
		EXPECT_TRUE(access->isReference());
		EXPECT_FALSE(access->isContextDependent());

		AccessManager mgr;
		auto clSet1 = mgr.getClassFor(access);

		EXPECT_EQ(3u, mgr.size());
		EXPECT_EQ(1u, clSet1.size());
		auto cl1 = *clSet1.begin();

		EXPECT_EQ(1u, cl1->getUID());
	}

	{	
		auto code = builder.parseAddresses(
			"{ "
			"	ref<array<int<4>,1>> v;"
			"	$*$v$[2u]$;"
			"}"
		);
		EXPECT_EQ(2u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto access = getImmediateAccess( mgr, accessAddr );
		auto varAddr = code[1].as<ExpressionAddress>();
		
		EXPECT_EQ(UnifiedAddress(accessAddr), access->getAddress());
		EXPECT_EQ(AccessType::AT_DEREF, access->getType());
		EXPECT_EQ(varAddr.getAddressedNode(), getRoot(access)->getVariable());
		EXPECT_FALSE(access->isReference());
		EXPECT_FALSE(access->isContextDependent());

		AccessManager mgr;
		auto clSet1 = mgr.getClassFor(access);

		EXPECT_EQ(3u, mgr.size());
		EXPECT_EQ(1u, clSet1.size());
		
		auto& cl1 = *clSet1.begin();
		EXPECT_EQ(1u, cl1->getUID());
	}

	{
		auto code = builder.parseAddresses(
			"{ "
			"	ref<vector<int<4>,4>> v;"
			"	$v[3u-1u]$;"
			"}"
		);
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto access = getImmediateAccess( mgr,accessAddr );

		EXPECT_EQ(AccessType::AT_SUBSCRIPT, access->getType());
		EXPECT_TRUE(access->isReference());
		EXPECT_FALSE(access->isContextDependent());

		auto subscript = cast<Subscript>(access);
		EXPECT_EQ("(v4294967295 + -2 == 0)", toString(*subscript->getRange()));
	}

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["b"] = builder.variable(builder.getLangBasic().getUInt4());
	symbols["a"] = builder.variable(builder.getLangBasic().getUInt4());

	{
		auto code = builder.parseAddresses(
			"{ $v[b]$; }", symbols
		);
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess( mgr, accessAddr );
		EXPECT_EQ(AccessType::AT_SUBSCRIPT, access->getType());
		EXPECT_TRUE(access->isReference());
		EXPECT_FALSE(access->isContextDependent());

		auto subscript = cast<Subscript>(access);
		EXPECT_FALSE(subscript->getRange());

	// 	std::cout << access << std::endl;
		
	}
//	{
//		auto address = builder.parseAddresses(
//			"{"
//			"	$if( b > 10u ) {"
//			"		$v[b]$;"
//			"	}$"
//			"}", symbols
//		);
//
//		EXPECT_EQ(2u, address.size());
//
//		auto rootNode = address[1].getRootNode();
//		auto accessNode = address[1];
//
//		// perform the polyhedral analysis 
//		auto scops = polyhedral::scop::mark(rootNode);
//
//		auto access = getImmediateAccess( accessNode.as<ExpressionAddress>() );
//
//		EXPECT_EQ(VarType::ARRAY, access.getType());
//		EXPECT_TRUE(access.isRef());
//
//		EXPECT_TRUE(!!access.getAccessedRange());
//		EXPECT_EQ("((v8 + -11 >= 0) ^ (v4294967295 + -v8 == 0))", toString(*access.getAccessedRange()));
//
//		EXPECT_EQ(address[0], access.getContext().getAddressedNode()); 
//		EXPECT_TRUE(access.isContextDependent());
//	}
//
//	symbols["a"] = builder.variable(builder.getLangBasic().getUInt4());
////	{
////		auto address = builder.parseAddresses(
////			"$if (b>0u && a<20u) {"
////			"	$uint<4> c = a+b;$"
////			"	$v[c]$; "
////			"}$", symbols
////		);
////
////		EXPECT_EQ(3u, address.size());
////
////		auto rootNode = address[0];
////		auto declNode = address[1].as<DeclarationStmtAddress>();
////		auto accessNode = address[2];
////
////		// perform the polyhedral analysis 
////		auto scops = polyhedral::scop::mark(rootNode);
////
////		// Create an alias for the expression c = b+a;
////		TmpVarMap map;
////		map.storeTmpVar( declNode->getInitialization(), declNode->getVariable().getAddressedNode() );
////
////		auto access = getImmediateAccess( accessNode.as<ExpressionAddress>(), {nullptr, 0}, map );
////
////		EXPECT_EQ(VarType::ARRAY, access.getType());
////		EXPECT_TRUE(access.isRef());
////		EXPECT_FALSE(access.isContextDependent());
////	}
////
////	{
////		auto address = builder.parseAddresses(
////			"$if (b>0u && a<20u) {"
////			"	$v[a*b]$; "
////			"}$", symbols
////		);
////
////		EXPECT_EQ(2u, address.size());
////
////		auto rootNode = address[0];
////		auto accessNode = address[1].as<ExpressionAddress>();
////
////		// perform the polyhedral analysis 
////		polyhedral::scop::mark(rootNode);
////
////		auto access = getImmediateAccess( accessNode );
////
////		EXPECT_EQ(VarType::ARRAY, access.getType());
////		EXPECT_TRUE(access.isRef());
////
////		EXPECT_FALSE( access.getContext() ); 
////		EXPECT_FALSE(access.isContextDependent());
////	}
////
	{
		auto address = builder.parseAddresses(
			"${"
			"	if (b>0u && a<20u) {"
			"		$$v$[a+b]$; "
			"	}"
			"}$", symbols
		);

		EXPECT_EQ(3u, address.size());

		auto rootNode = core::analysis::normalize(address[0]);
		auto accessNode = address[1].switchRoot(rootNode).as<ExpressionAddress>();

		// perform the polyhedral analysis 
		polyhedral::scop::mark(rootNode.getAddressedNode());

		auto access = getImmediateAccess( mgr, accessNode );
		EXPECT_EQ(AccessType::AT_SUBSCRIPT, access->getType());
		EXPECT_EQ(address[2].switchRoot(rootNode).getAddressedNode(), getRoot(access)->getVariable());

		EXPECT_TRUE(access->isReference());
		EXPECT_TRUE(access->isContextDependent());

		auto subscript = cast<Subscript>(access);
		EXPECT_TRUE(subscript->getContext());

		EXPECT_EQ("(((-v26 + 19 >= 0) ^ (v25 + -1 >= 0)) ^ (v4294967295 + -v25 + -v26 == 0))",
				  toString(*subscript->getRange()));

		auto ctx = polyhedral::makeCtx();
		auto set = polyhedral::makeSet(ctx, polyhedral::IterationDomain(subscript->getRange()));
		EXPECT_EQ("[v25, v26] -> { [v25 + v26] : v26 <= 19 and v25 >= 1 }", toString(*set));
	}
}

TEST(Access, ArrayAlias) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	{	
		auto code = builder.parseAddresses(
			"{ "
			"	ref<array<int<4>,1>> v;"
			"	$ref<int<4>> e = v[2u];$"
			"	$e$;"
			"}"
		);

		EXPECT_EQ(2u, code.size());

		auto declAddr = code[0].as<DeclarationStmtAddress>();
		auto accessAddr = code[1].as<ExpressionAddress>();

		auto access = getImmediateAccess( mgr, accessAddr );

		TmpVarMap tmpVarMap;
		tmpVarMap.storeTmpVar(declAddr->getInitialization(), declAddr->getVariable().getAddressedNode());

		EXPECT_EQ(UnifiedAddress(accessAddr), access->getAddress());
		EXPECT_TRUE(access->isReference());
		EXPECT_FALSE(access->isContextDependent());

		AccessManager mgr(nullptr, tmpVarMap);
		auto clSet1 = mgr.getClassFor(access);
	
		EXPECT_EQ(3u, mgr.size());

		EXPECT_EQ(1u, clSet1.size());
		auto cl1 = *clSet1.begin();

		EXPECT_EQ(1u, cl1->getUID());
		EXPECT_EQ(2u, cl1->size());
	}
}


////TEST(Access, SameAccess) {
////
////	NodeManager mgr;
////	IRBuilder builder(mgr);
////
////	std::map<string, NodePtr> symbols;
////	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
////
////	auto address = builder.parseAddresses(
////		"$for (uint<4> i = 0u .. 10 : 1) {"
////		"	$v[i]$; "
////		"	$v[i]$; "
////		"}$", symbols
////	);
////
////	EXPECT_EQ(3u, address.size());
////
////	auto rootNode = address[0];
////
////	// perform the polyhedral analysis 
////	polyhedral::scop::mark(rootNode.getAddressedNode());
////
////	auto accessNode1 = address[1].as<ExpressionAddress>();
////	auto accessNode2 = address[2].as<ExpressionAddress>();
////
////	auto access1 = getImmediateAccess( accessNode1 );
////	auto access2 = getImmediateAccess( accessNode2 );
////
////	EXPECT_TRUE(access1.getContext());
////	EXPECT_FALSE(access1.isContextDependent());
////	EXPECT_EQ(access1.getContext(), rootNode);
////	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access1.getAccessedRange()));
////
////	EXPECT_TRUE(access2.getContext());
////	EXPECT_FALSE(access2.isContextDependent());
////	EXPECT_EQ(access2.getContext(), rootNode);
////	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access2.getAccessedRange()));
////}
////
////TEST(Access, DifferentAccess) {
////	
////	NodeManager mgr;
////	IRBuilder builder(mgr);
////
////	std::map<string, NodePtr> symbols;
////	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
////
////	auto address = builder.parseAddresses(
////		"$for (uint<4> i = 0u .. 10 : 1) {"
////		"	$v[i]$; "
////		"	$v[i+1u]$; "
////		"}$", symbols
////	);
////
////	EXPECT_EQ(3u, address.size());
////
////	auto rootNode = address[0];
////
////	// perform the polyhedral analysis 
////	polyhedral::scop::mark(rootNode.getAddressedNode());
////
////	auto accessNode1 = address[1].as<ExpressionAddress>();
////	auto accessNode2 = address[2].as<ExpressionAddress>();
////
////	auto access1 = getImmediateAccess( accessNode1 );
////	auto access2 = getImmediateAccess( accessNode2 );
////
////	EXPECT_TRUE(access1.getContext());
////	EXPECT_FALSE(access1.isContextDependent());
////	EXPECT_EQ(access1.getContext(), rootNode);
////	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access1.getAccessedRange()));
////
////
////	EXPECT_TRUE(access2.getContext());
////	EXPECT_FALSE(access2.isContextDependent());
////	EXPECT_EQ(access2.getContext(), rootNode);
////	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 + -1 == 0))", toString(*access2.getAccessedRange()));
////
////	auto ctx = polyhedral::makeCtx();
////	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access1.getAccessedRange()));
////	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 9 and v4294967295 >= 0 }", toString(*set1));
////}

TEST(Access, CommonSubset) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"${ "
		"	for (uint<4> i1 = 1u .. 11u : 1) {"
		"		$v[i1]$; "
		"	}"
		"	for (uint<4> i2 = 0u .. 10u : 1) {"
		"		$v[i2+1u]$;"
		"	}"
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( mgr, accessNode1 );
	auto access2 = getImmediateAccess( mgr, accessNode2 );

	EXPECT_FALSE(access1->isContextDependent());
	EXPECT_FALSE(access2->isContextDependent());

	auto rAccess1 = cast<Subscript>(access1);
	EXPECT_TRUE(rAccess1->getContext());
	EXPECT_EQ(rAccess1->getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 10 >= 0) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", 
			toString(*rAccess1->getRange()));

	auto rAccess2 = cast<Subscript>(access2);
	EXPECT_TRUE(rAccess2->getContext());
	EXPECT_EQ(rAccess2->getContext(), rootNode);
	EXPECT_EQ("(((-v6 + 9 >= 0) ^ (v6 >= 0)) ^ (-v6 + v4294967295 + -1 == 0))",
			toString(*rAccess2->getRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess1->getRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess2->getRange()));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 10 and v4294967295 >= 1 }", toString(*set1));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 10 and v4294967295 >= 1 }", toString(*set2));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 10 and v4294967295 >= 1 }", toString(*(set1 * set2)));

	// the two sets are equal
	EXPECT_EQ(*set1, *set2);
	EXPECT_FALSE((set1*set2)->empty());
	EXPECT_EQ(*set1, *(set1*set2));

	// Set difference is empty 
	EXPECT_TRUE((set1-set2)->empty());



	// Add the accesses to the class manager 
	AccessManager aMgr;
	auto clSet1 = aMgr.getClassFor(access1);
	EXPECT_EQ(1u, clSet1.size());
	
	auto cl1 = *clSet1.begin();
	EXPECT_EQ(1u, cl1->getUID());

	
	auto clSet2 = aMgr.getClassFor(access2);
	EXPECT_EQ(1u, clSet2.size());

	auto cl2 = *clSet2.begin();

	EXPECT_EQ(cl1, cl2);
	EXPECT_EQ(*cl1, *cl2);

	EXPECT_EQ(3u, aMgr.size());
}


TEST(Access, EmptySubset) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"${ "
		"	for (uint<4> i1 = 1u .. 5u : 1) {"
		"		$v[i1]$; "
		"	}"
		"	for (uint<4> i2 = 4u .. 9u : 1) {"
		"		$v[i2+1u]$;"
		"	}"
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( mgr, accessNode1 );
	auto access2 = getImmediateAccess( mgr, accessNode2 );

	EXPECT_FALSE(access1->isContextDependent());
	EXPECT_FALSE(access2->isContextDependent());

	auto rAccess1 = cast<Subscript>(access1);
	EXPECT_TRUE(rAccess1->getContext());
	EXPECT_EQ(rAccess1->getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 4 >= 0) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", 
			toString(*rAccess1->getRange()));

	auto rAccess2 = cast<Subscript>(access2);
	EXPECT_TRUE(rAccess2->getContext());
	EXPECT_EQ(rAccess2->getContext(), rootNode);
	EXPECT_EQ("(((-v6 + 8 >= 0) ^ (v6 + -4 >= 0)) ^ (-v6 + v4294967295 + -1 == 0))",
			toString(*rAccess2->getRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess1->getRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess2->getRange()));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 4 and v4294967295 >= 1 }", toString(*set1));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 9 and v4294967295 >= 5 }", toString(*set2));

	EXPECT_TRUE((set1 * set2)->empty());


	AccessManager aMgr;
	auto clSet1 = aMgr.getClassFor(rAccess1);
	EXPECT_EQ(3u, aMgr.size());

	EXPECT_EQ(1u, clSet1.size());
	auto cl1 = *clSet1.begin();
	EXPECT_EQ(1u, cl1->getUID());

	auto clSet2 = aMgr.getClassFor(rAccess2);

	EXPECT_EQ(4u, aMgr.size());
	EXPECT_EQ(1u, clSet2.size());

	auto cl2 = *clSet2.begin();
	EXPECT_EQ(3u, cl2->getUID());

}

TEST(Access, CommonSubSubset) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"${ "
		"	for (uint<4> i1 = 1u .. 5u : 1) {"
		"		$v[i1]$; "
		"	}"
		"	for (uint<4> i2 = 1u .. 3u : 1) {"
		"		$v[i2]$;"
		"	}"
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( mgr, accessNode1 );
	auto access2 = getImmediateAccess( mgr, accessNode2 );

	EXPECT_FALSE(access1->isContextDependent());
	EXPECT_FALSE(access2->isContextDependent());

	auto rAccess1 = cast<Subscript>(access1);
	EXPECT_TRUE(rAccess1->getContext());
	EXPECT_EQ(rAccess1->getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 4 >= 0) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", 
			toString(*rAccess1->getRange()));

	auto rAccess2 = cast<Subscript>(access2);
	EXPECT_TRUE(rAccess2->getContext());
	EXPECT_EQ(rAccess2->getContext(), rootNode);
	EXPECT_EQ("(((-v6 + 2 >= 0) ^ (v6 + -1 >= 0)) ^ (-v6 + v4294967295 == 0))",
			toString(*rAccess2->getRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess1->getRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess2->getRange()));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 4 and v4294967295 >= 1 }", toString(*set1));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 2 and v4294967295 >= 1 }", toString(*set2));

	EXPECT_FALSE((set1 * set2)->empty());

	{ 
		AccessManager aMgr;
		auto clSet1 = aMgr.getClassFor(rAccess2);
		EXPECT_EQ(3u, aMgr.size());
		EXPECT_EQ(1u, clSet1.size());

		auto cl1 = *clSet1.begin();
		EXPECT_EQ(1u, cl1->getUID());
		EXPECT_EQ(*cl1->getParentClass(), aMgr[0]);


		auto clSet2 = aMgr.getClassFor(rAccess1);
		EXPECT_EQ(4u, aMgr.size());
		EXPECT_EQ(2u, clSet2.size());

		auto cl2_1 = *clSet2.begin();
		EXPECT_EQ(1u, cl2_1->getUID());
		EXPECT_EQ(*cl2_1->getParentClass(), aMgr[0]);

		auto cl2_2 = *(++clSet2.begin());
		EXPECT_EQ(3u, cl2_2->getUID());
		EXPECT_EQ(*cl2_2->getParentClass(), aMgr[0]);
	}

	{ 
		AccessManager aMgr;
		auto clSet1 = aMgr.getClassFor(rAccess1);
		EXPECT_EQ(3u, aMgr.size());
		EXPECT_EQ(1u, clSet1.size());

		auto cl1 = *clSet1.begin();
		EXPECT_EQ(1u, cl1->getUID());
		EXPECT_EQ(*cl1->getParentClass(), aMgr[0]);

		auto clSet2 = aMgr.getClassFor(rAccess2);
		EXPECT_EQ(4u, aMgr.size());
	
		EXPECT_EQ(1u, clSet2.size());

		auto cl2 = *clSet2.begin();
		EXPECT_EQ(3u, cl2->getUID());
		EXPECT_EQ(*cl2->getParentClass(), *cl1->getParentClass());

		clSet1 = aMgr.getClassFor(rAccess1);
		EXPECT_EQ(4u, aMgr.size());

		// The access now has been split into 2 classes 
		EXPECT_EQ(2u, clSet1.size());

		EXPECT_EQ(1u, (*clSet1.begin())->getUID());
		EXPECT_EQ(3u, (*(++clSet1.begin()))->getUID());
		
	}
}

TEST(Access, CommonInnerSubset) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"${ "
		"	for (uint<4> i1 = 1u .. 6u : 1) {"
		"		$v[i1]$; "
		"	}"
		"	for (uint<4> i2 = 4u .. 8u : 1) {"
		"		$v[i2]$;"
		"	}"
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( mgr, accessNode1 );
	auto access2 = getImmediateAccess( mgr, accessNode2 );

	EXPECT_FALSE(access1->isContextDependent());
	EXPECT_FALSE(access2->isContextDependent());

	auto rAccess1 = cast<Subscript>(access1);
	EXPECT_TRUE(rAccess1->getContext());
	EXPECT_EQ(rAccess1->getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 5 >= 0) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", 
			toString(*rAccess1->getRange()));

	auto rAccess2 = cast<Subscript>(access2);
	EXPECT_TRUE(rAccess2->getContext());
	EXPECT_EQ(rAccess2->getContext(), rootNode);
	EXPECT_EQ("(((-v6 + 7 >= 0) ^ (v6 + -4 >= 0)) ^ (-v6 + v4294967295 == 0))",
			toString(*rAccess2->getRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess1->getRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess2->getRange()));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 5 and v4294967295 >= 1 }", toString(*set1));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 7 and v4294967295 >= 4 }", toString(*set2));

	EXPECT_FALSE((set1 * set2)->empty());

	{ 
		AccessManager aMgr;
		auto clSet1 = aMgr.getClassFor(rAccess2);
		EXPECT_EQ(3u, aMgr.size());
		EXPECT_EQ(1u, clSet1.size());

		auto cl1 = *clSet1.begin();
		EXPECT_EQ(1u, cl1->getUID());
		EXPECT_EQ(*cl1->getParentClass(), aMgr[0]);


		auto clSet2 = aMgr.getClassFor(rAccess1);
		EXPECT_EQ(5u, aMgr.size());
		EXPECT_EQ(2u, clSet2.size());

		auto cl2_1 = *clSet2.begin();
		EXPECT_EQ(3u, cl2_1->getUID());
		EXPECT_EQ(*cl2_1->getParentClass(), aMgr[0]);

		auto cl2_2 = *(++clSet2.begin());
		EXPECT_EQ(4u, cl2_2->getUID());
		EXPECT_EQ(*cl2_2->getParentClass(), aMgr[0]);
	}

	{ 
		AccessManager aMgr;
		auto clSet1 = aMgr.getClassFor(rAccess1);
		EXPECT_EQ(3u, aMgr.size());
		EXPECT_EQ(1u, clSet1.size());

		auto cl1 = *clSet1.begin();
		EXPECT_EQ(1u, cl1->getUID());
		EXPECT_EQ(*cl1->getParentClass(), aMgr[0]);

		auto clSet2 = aMgr.getClassFor(rAccess2);
		EXPECT_EQ(5u, aMgr.size());
	
		EXPECT_EQ(2u, clSet2.size());

		auto cl2 = *clSet2.begin();
		EXPECT_EQ(3u, cl2->getUID());
		EXPECT_EQ(*cl2->getParentClass(), *cl1->getParentClass());

		clSet1 = aMgr.getClassFor(rAccess1);
		EXPECT_EQ(5u, aMgr.size());

		// The access now has been split into 2 classes 
		EXPECT_EQ(2u, clSet1.size());

		EXPECT_EQ(1u, (*clSet1.begin())->getUID());
		EXPECT_EQ(3u, (*(++clSet1.begin()))->getUID());
		
	}
}

TEST(Access, StridedSubset) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"${ "
		"	for (uint<4> i1 = 1u .. 5u : 2) {"
		"		$v[i1]$; "
		"	}"
		"	for (uint<4> i2 = 1u .. 9u : 2) {"
		"		$v[i2+1u]$;"
		"	}"
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( mgr, accessNode1 );
	auto access2 = getImmediateAccess( mgr, accessNode2 );

	EXPECT_FALSE(access1->isContextDependent());
	EXPECT_FALSE(access2->isContextDependent());

	auto rAccess1 = cast<Subscript>(access1);
	EXPECT_TRUE(rAccess1->getContext());
	EXPECT_EQ(rAccess1->getContext(), rootNode);
	EXPECT_EQ("((((-v2 + 4 >= 0) ^ (v2 + -2*v36 + -1 == 0)) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))",
			toString(*rAccess1->getRange()));

	auto rAccess2 = cast<Subscript>(access2);
	EXPECT_TRUE(rAccess2->getContext());
	EXPECT_EQ(rAccess2->getContext(), rootNode);
	EXPECT_EQ("((((-v6 + 8 >= 0) ^ (v6 + -2*v42 + -1 == 0)) ^ (v6 + -1 >= 0)) ^ (-v6 + v4294967295 + -1 == 0))",
			toString(*rAccess2->getRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess1->getRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(rAccess2->getRange()));

	EXPECT_EQ("{ [v4294967295] : exists (e0 = [(-1 + v4294967295)/2]: 2e0 = -1 + v4294967295 and v4294967295 <= 3 and v4294967295 >= 1) }", 
			toString(*set1));
	EXPECT_EQ("{ [v4294967295] : exists (e0 = [(v4294967295)/2]: 2e0 = v4294967295 and v4294967295 <= 8 and v4294967295 >= 2) }", 
			toString(*set2));

	EXPECT_TRUE((set1 * set2)->empty());
}

TEST(Access, MultipleAccessesSimple) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(
		"${"
		"	int<4> a=2; "
		"	int<4> b=3; "
		"	$int<4> c = $a$ + $b$;$ "
		"}$");

	EXPECT_EQ(4u, addresses.size());

	auto accesses = getAccesses(  mgr, UnifiedAddress(addresses[1]) );

	EXPECT_EQ(3u, accesses.size());

	EXPECT_EQ(addresses[1].as<DeclarationStmtAddress>()->getVariable(), accesses[0]->getAddress());
	EXPECT_FALSE(accesses[0]->isReference());

	EXPECT_EQ(addresses[2], accesses[1]->getAddress());
	EXPECT_FALSE(accesses[1]->isReference());

	EXPECT_EQ(addresses[3], accesses[2]->getAddress());
	EXPECT_FALSE(accesses[2]->isReference());
}

TEST(Access, MultipleAccessesVector) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(
		"${"
		"	vector<uint<4>,4> a; "
		"	ref<uint<4>> b=3; "
		"	ref<uint<4>> c; "
		"	$c = $a[$b$]$ + $b$;$ "
		"}$");

	EXPECT_EQ(5u, addresses.size());

	auto accesses = getAccesses( mgr, UnifiedAddress(addresses[1]) );

	EXPECT_EQ(4u, accesses.size());

	EXPECT_EQ(addresses[1].as<CallExprAddress>()->getArgument(0), accesses[0]->getAddress());
	EXPECT_TRUE(accesses[0]->isReference());

	EXPECT_EQ(addresses[2], accesses[1]->getAddress());
	EXPECT_FALSE(accesses[1]->isReference());

	EXPECT_EQ(addresses[2], accesses[2]->getAddress());
	EXPECT_FALSE(accesses[1]->isReference());

	EXPECT_EQ(addresses[4], accesses[3]->getAddress());
	EXPECT_FALSE(accesses[2]->isReference());
}

TEST(Access, MultipleAccessesVector2) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(
		"${"
		"	vector<uint<4>,4> a; "
		"	ref<uint<4>> b=3; "
		"	$uint<4> c = $a[$b$]$ + 4u;$"
		"}$");

	EXPECT_EQ(4u, addresses.size());

	auto accesses = getAccesses( mgr, UnifiedAddress(addresses[1]) );

	EXPECT_EQ(3u, accesses.size());

	EXPECT_EQ(addresses[1].as<DeclarationStmtAddress>()->getVariable(), accesses[0]->getAddress());
	EXPECT_EQ(addresses[2], accesses[1]->getAddress());
	EXPECT_EQ(addresses[3], accesses[2]->getAddress());
}

