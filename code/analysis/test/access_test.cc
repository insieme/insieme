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
#include "insieme/analysis/access.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

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
		auto access = getImmediateAccess( accessAddr );

		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_TRUE(access.isRef());

		AccessManager mgr;
		auto& assClass = mgr.getClassFor(access);
		EXPECT_EQ(0u, assClass.getUID());

		auto& assClass2 = mgr.getClassFor(access);
		EXPECT_EQ(0u, assClass2.getUID());

		EXPECT_EQ(assClass, assClass2);
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

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_FALSE(access.isRef());
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

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_EQ(varAddr.getAddressedNode(), access.getAccessedVariable());
		EXPECT_FALSE(access.isRef());
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

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_TRUE(access.isRef());
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

		auto access = getImmediateAccess(accessAddr);

		EXPECT_EQ(accessAddr, access.getAccessExpression());
		EXPECT_EQ(varAddr.getAddressedNode(), access.getAccessedVariable());
		EXPECT_EQ(VarType::MEMBER, access.getType());
		EXPECT_TRUE(access.isRef());
	}

	{
		auto code = builder.parseAddresses(
			"{ "
			"	struct{ int<4> a; int<4> b; } s;"
			"	$$s$.a$;"
			"}"
		);
		EXPECT_EQ(2u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto varAddr = code[1].as<VariableAddress>();

		auto access = getImmediateAccess(accessAddr);

		EXPECT_EQ(accessAddr, access.getAccessExpression());
		EXPECT_EQ(varAddr.getAddressedNode(), access.getAccessedVariable());
		EXPECT_EQ(VarType::MEMBER, access.getType());

		EXPECT_FALSE(access.isRef());
	}

}

// Wait for new parser 
TEST(Access, ArrayAccess) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	{	
		auto code = builder.parseAddresses(
			"{ "
			"	ref<array<int<4>,1>> v;"
			"	$v[2u]$;"
			"}"
		);
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();
		auto access = getImmediateAccess( accessAddr );

		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		EXPECT_EQ("(v4294967295 + -2 == 0)", toString(*access.getAccessedRange()));
		EXPECT_FALSE(access.getContext());
		EXPECT_FALSE(access.isContextDependent());
	}

	{	
		auto code = builder.parseAddresses(
			"{ "
			"	array<int<4>,1> v;"
			"	$v[2u]$;"
			"}"
		);
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_FALSE(access.isRef());
		EXPECT_EQ("(v4294967295 + -2 == 0)", toString(*access.getAccessedRange()));
		EXPECT_FALSE(access.getContext());
		EXPECT_FALSE(access.isContextDependent());
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

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		EXPECT_EQ("(v4294967295 + -2 == 0)", toString(*access.getAccessedRange()));
		EXPECT_FALSE(access.getContext());
		EXPECT_FALSE(access.isContextDependent());
	}

	{
		auto code = builder.parseAddresses(
			"{ "
			"	vector<int<4>,4> v;"
			"	$v[2u]$;"
			"}"
		);
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_FALSE(access.isRef());
		EXPECT_EQ("(v4294967295 + -2 == 0)", toString(*access.getAccessedRange()));
		EXPECT_FALSE(access.getContext());
		EXPECT_FALSE(access.isContextDependent());
	}

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["b"] = builder.variable(builder.getLangBasic().getUInt4());

	{
		auto code = builder.parseAddresses(
			"{ "
			"	$v[b]$;"
			"}", symbols
		);
		EXPECT_EQ(1u, code.size());

		auto accessAddr = code[0].as<ExpressionAddress>();

		auto access = getImmediateAccess( accessAddr );
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
	}

	{
		auto address = builder.parseAddresses(
			"{"
			"	$if( b > 10u ) {"
			"		$v[b]$;"
			"	}$"
			"}", symbols
		);

		EXPECT_EQ(2u, address.size());

		auto rootNode = address[1].getRootNode();
		auto accessNode = address[1];

		// perform the polyhedral analysis 
		auto scops = polyhedral::scop::mark(rootNode);

		auto access = getImmediateAccess( accessNode.as<ExpressionAddress>() );

		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());

		EXPECT_TRUE(!!access.getAccessedRange());
		EXPECT_EQ("((v10 + -11 >= 0) ^ (v4294967295 + -v10 == 0))", toString(*access.getAccessedRange()));

		EXPECT_EQ(address[0], access.getContext().getAddressedNode()); 
		EXPECT_TRUE(access.isContextDependent());
	}

	symbols["a"] = builder.variable(builder.getLangBasic().getUInt4());
	{
		auto address = builder.parseAddresses(
			"$if (b>0u && a<20u) {"
			"	$uint<4> c = a+b;$"
			"	$v[c]$; "
			"}$", symbols
		);

		EXPECT_EQ(3u, address.size());

		auto rootNode = address[0];
		auto declNode = address[1].as<DeclarationStmtAddress>();
		auto accessNode = address[2];

		// perform the polyhedral analysis 
		auto scops = polyhedral::scop::mark(rootNode);

		// Create an alias for the expression c = b+a;
		TmpVarMap map;
		map.storeTmpVar( declNode->getInitialization(), declNode->getVariable().getAddressedNode() );

		auto access = getImmediateAccess( accessNode.as<ExpressionAddress>(), map );

		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		EXPECT_FALSE(access.isContextDependent());
	}

	{
		auto address = builder.parseAddresses(
			"$if (b>0u && a<20u) {"
			"	$v[a*b]$; "
			"}$", symbols
		);

		EXPECT_EQ(2u, address.size());

		auto rootNode = address[0];
		auto accessNode = address[1].as<ExpressionAddress>();

		// perform the polyhedral analysis 
		polyhedral::scop::mark(rootNode);

		auto access = getImmediateAccess( accessNode );

		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());

		EXPECT_FALSE( access.getContext() ); 
		EXPECT_FALSE(access.isContextDependent());
	}

	{
		auto address = builder.parseAddresses(
			"${"
			"	if (b>0u && a<20u) {"
			"		$v[a+b]$; "
			"	}"
			"}$", symbols
		);

		EXPECT_EQ(2u, address.size());

		auto rootNode = address[0];
		auto accessNode = address[1].as<ExpressionAddress>();

		// perform the polyhedral analysis 
		polyhedral::scop::mark(rootNode.getAddressedNode());

		auto access = getImmediateAccess( accessNode );

		EXPECT_EQ(VarType::ARRAY, access.getType());

		EXPECT_TRUE(access.isRef());
		EXPECT_TRUE(access.getContext());
		EXPECT_TRUE(access.isContextDependent());

		EXPECT_EQ("(((-v14 + 19 >= 0) ^ (v10 + -1 >= 0)) ^ (v4294967295 + -v10 + -v14 == 0))", 
				  toString(*access.getAccessedRange())
				 );

		auto ctx = polyhedral::makeCtx();
		auto set = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access.getAccessedRange()));
		EXPECT_EQ("[v10, v14] -> { [v10 + v14] : v14 <= 19 and v10 >= 1 }", toString(*set));
	}

}

TEST(Access, SameAccess) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"$for (uint<4> i = 0u .. 10 : 1) {"
		"	$v[i]$; "
		"	$v[i]$; "
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( accessNode1 );
	auto access2 = getImmediateAccess( accessNode2 );

	EXPECT_TRUE(access1.getContext());
	EXPECT_FALSE(access1.isContextDependent());
	EXPECT_EQ(access1.getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access1.getAccessedRange()));

	EXPECT_TRUE(access2.getContext());
	EXPECT_FALSE(access2.isContextDependent());
	EXPECT_EQ(access2.getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access2.getAccessedRange()));
}

TEST(Access, DifferentAccess) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

	auto address = builder.parseAddresses(
		"$for (uint<4> i = 0u .. 10 : 1) {"
		"	$v[i]$; "
		"	$v[i+1u]$; "
		"}$", symbols
	);

	EXPECT_EQ(3u, address.size());

	auto rootNode = address[0];

	// perform the polyhedral analysis 
	polyhedral::scop::mark(rootNode.getAddressedNode());

	auto accessNode1 = address[1].as<ExpressionAddress>();
	auto accessNode2 = address[2].as<ExpressionAddress>();

	auto access1 = getImmediateAccess( accessNode1 );
	auto access2 = getImmediateAccess( accessNode2 );

	EXPECT_TRUE(access1.getContext());
	EXPECT_FALSE(access1.isContextDependent());
	EXPECT_EQ(access1.getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access1.getAccessedRange()));


	EXPECT_TRUE(access2.getContext());
	EXPECT_FALSE(access2.isContextDependent());
	EXPECT_EQ(access2.getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 9 >= 0) ^ (v2 >= 0)) ^ (-v2 + v4294967295 + -1 == 0))", toString(*access2.getAccessedRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access1.getAccessedRange()));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 9 and v4294967295 >= 0 }", toString(*set1));
}

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

	auto access1 = getImmediateAccess( accessNode1 );
	auto access2 = getImmediateAccess( accessNode2 );

	EXPECT_TRUE(access1.getContext());
	EXPECT_FALSE(access1.isContextDependent());
	EXPECT_EQ(access1.getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 10 >= 0) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access1.getAccessedRange()));

	EXPECT_TRUE(access2.getContext());
	EXPECT_FALSE(access2.isContextDependent());
	EXPECT_EQ(access2.getContext(), rootNode);
	EXPECT_EQ("(((-v4 + 9 >= 0) ^ (v4 >= 0)) ^ (-v4 + v4294967295 + -1 == 0))", toString(*access2.getAccessedRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access1.getAccessedRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access2.getAccessedRange()));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 10 and v4294967295 >= 1 }", toString(*set1));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 10 and v4294967295 >= 1 }", toString(*set2));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 10 and v4294967295 >= 1 }", toString(*(set1 * set2)));

	// the two sets are equal
	EXPECT_EQ(*set1, *set2);
	EXPECT_FALSE((set1*set2)->empty());
	EXPECT_EQ(*set1, *(set1*set2));

	// Set difference is empty 
	EXPECT_TRUE((set1-set2)->empty());
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

	auto access1 = getImmediateAccess( accessNode1 );
	auto access2 = getImmediateAccess( accessNode2 );

	EXPECT_TRUE(access1.getContext());
	EXPECT_FALSE(access1.isContextDependent());
	EXPECT_EQ(access1.getContext(), rootNode);
	EXPECT_EQ("(((-v2 + 4 >= 0) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", toString(*access1.getAccessedRange()));

	EXPECT_TRUE(access2.getContext());
	EXPECT_FALSE(access2.isContextDependent());
	EXPECT_EQ(access2.getContext(), rootNode);
	EXPECT_EQ("(((-v4 + 8 >= 0) ^ (v4 + -4 >= 0)) ^ (-v4 + v4294967295 + -1 == 0))", toString(*access2.getAccessedRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access1.getAccessedRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access2.getAccessedRange()));

	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 4 and v4294967295 >= 1 }", toString(*set1));
	EXPECT_EQ("{ [v4294967295] : v4294967295 <= 9 and v4294967295 >= 5 }", toString(*set2));

	EXPECT_TRUE((set1 * set2)->empty());
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

	auto access1 = getImmediateAccess( accessNode1 );
	auto access2 = getImmediateAccess( accessNode2 );

	EXPECT_TRUE(access1.getContext());
	EXPECT_FALSE(access1.isContextDependent());
	EXPECT_EQ(access1.getContext(), rootNode);
	EXPECT_EQ("((((-v2 + 4 >= 0) ^ (v2 + -2*v7 + -1 == 0)) ^ (v2 + -1 >= 0)) ^ (-v2 + v4294967295 == 0))", 
			toString(*access1.getAccessedRange()));

	EXPECT_TRUE(access2.getContext());
	EXPECT_FALSE(access2.isContextDependent());
	EXPECT_EQ(access2.getContext(), rootNode);
	EXPECT_EQ("((((-v4 + 8 >= 0) ^ (v4 + -2*v8 + -1 == 0)) ^ (v4 + -1 >= 0)) ^ (-v4 + v4294967295 + -1 == 0))", 
			toString(*access2.getAccessedRange()));

	auto ctx = polyhedral::makeCtx();
	auto set1 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access1.getAccessedRange()));
	auto set2 = polyhedral::makeSet(ctx, polyhedral::IterationDomain(access2.getAccessedRange()));

	EXPECT_EQ("{ [v4294967295] : exists (e0 = [(-1 + v4294967295)/2]: 2e0 = -1 + v4294967295 and v4294967295 <= 3 and v4294967295 >= 1) }", 
			toString(*set1));
	EXPECT_EQ("{ [v4294967295] : exists (e0 = [(v4294967295)/2]: 2e0 = v4294967295 and v4294967295 <= 8 and v4294967295 >= 2) }", 
			toString(*set2));

	EXPECT_TRUE((set1 * set2)->empty());
}

