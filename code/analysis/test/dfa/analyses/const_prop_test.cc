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

#include "insieme/analysis/dfa/solver.h"
#include "insieme/analysis/dfa/analyses/const_prop.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

typedef dfa::analyses::ConstantPropagation::value_type AnalysisData;

typename AnalysisData::value_type find_constant_value(const AccessClassSet& classes, const AnalysisData& in) {

	for (const auto& cl : classes) {

		auto fit = std::find_if(in.begin(), in.end(), 
				[&](const typename AnalysisData::value_type& cur) { 
					return *std::get<0>(cur) == *cl; 
				});
		
		if (fit!=in.end()) { return *fit; }
	}
	
	assert(false && "Big problem");
}


TEST(ConstantPropagation, PropagateConstantNoControl) {
 
 	NodeManager mgr;
 	IRBuilder builder(mgr);
 
    auto addresses = builder.parseAddresses(
		"${"
		"	ref<int<4>> a = 1;"
		"	int<4> c = *$a$;"
		"}$"
    );
 
	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
 
 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	// Finds the CFG block containing the address of variable a
 	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());
 
	auto accPtr = getImmediateAccess(mgr, addresses[1]);

	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );

	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( builder.intLit(1), std::get<1>(cons).value() );
}

TEST(ConstantPropagation, PropagateConstant) {
 
 	NodeManager mgr;
 	IRBuilder builder(mgr);
 
    auto addresses = builder.parseAddresses(
		"${"
		"	ref<int<4>> a = 1;"
		"	if ( a <= 0 ) { "
		"		a = 1; "
		"	} "
		"	int<4> c = *$a$;"
		"}$"
    );
 
	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
 
 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	// Finds the CFG block containing the address of variable a
 	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());

	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor( accPtr );
	assert( !accClasses.empty() );

	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( builder.intLit(1), std::get<1>(cons).value() );
}


TEST(ConstantPropagation, PropagateNotConstant) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	ref<int<4>> a = 1;"
		"	if ( a <= 0 ) { "
		"		a = 2; "
		"	}"
		"	int<4> c = *$a$;"
		"}$"
    );

  	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
 
 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	// Finds the CFG block containing the address of variable a
 	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());
 
	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );

	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( dfa::bottom, std::get<1>(cons) );
}

TEST(ConstantPropagation, PropagateArrayElementConstant) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
			builder.parseType("ref<vector<int<4>,10>>")
		);

    auto addresses = builder.parseAddresses(
		"${"
		"	v[3u] = 2; "
		"	int<4> c = *$v[3u]$;"
		"}$", symbols
    );

  	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
 
 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	// Finds the CFG block containing the address of variable a
 	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(3u, addr.getBlockPtr()->getBlockID());
 
	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );

	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( builder.intLit(2), std::get<1>(cons).value() );
}

TEST(ConstantPropagation, PropagateArrayElementLoop) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
			builder.parseType("ref<vector<int<4>,10>>")
		);

    auto addresses = builder.parseAddresses(
		"${"
		"	v[3u] = 4; "
		" 	for( uint<4> i = 2u .. 10u : 2u) {"
		"		v[i] = 4; "
		"	} "
		"	int<4> c = *$v[3u]$;"
		"}$", symbols
    );

  	EXPECT_EQ(2u, addresses.size());

	// mark for polyhedral 
	polyhedral::scop::mark(addresses[0]);

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
 
 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	// Finds the CFG block containing the address of variable a
 	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(3u, addr.getBlockPtr()->getBlockID());
 
	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );
	
	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( builder.intLit(4), std::get<1>(cons).value() );

}

TEST(ConstantPropagation, Formulas) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	ref<int<4>> a = 10; "
		"	int<4> b = a+2; "
		"	a = b+a;"
		"	int<4> c = $a$;"
		"}$"
    );

  	EXPECT_EQ(2u, addresses.size());

	// mark for polyhedral 
	polyhedral::scop::mark(addresses[0]);

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
 
 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	// Finds the CFG block containing the address of variable a
 	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());
 
	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );
	
	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( builder.intLit(22), std::get<1>(cons).value() );

}

TEST(ConstantPropagation, TransitivePropagation) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	ref<int<4>> a = 1;"
		"	int<4> b = (10+a);"
		"	if ( a <= 0 ) { "
		"		a = 2; "
		"	}"
		"	int<4> c = $b$;"
		"}$"
    );

  	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0]);

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& ret = s.solve();
	
	// Finds the CFG block containing the address of variable b
	auto addr = cfg->find( addresses[1] );
 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());

	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );
	
	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	auto cons = find_constant_value(accClasses, consts);

	EXPECT_EQ( builder.intLit(11), std::get<1>(cons).value() );

}

// TEST(ConstantPropagation, Aliasing) {
// 
// 	NodeManager mgr;
// 	parse::IRParser parser(mgr);
// 	IRBuilder builder(mgr);
// 
//     auto code = parser.parseStatement(
// 		"{"
// 		"	decl ref<struct<a:int<4>, b:int<4>>>:s=0;"
// 		"	((op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>,int>)) = 5);"
// 		"	decl ref<int<4>>:c = (op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>, int>));"
// 		"	decl int<4>:d = (op<ref.deref>(c));"
// 		"}"
//     );
// 
//     EXPECT_TRUE(code);
// 	CFGPtr cfg = CFG::buildCFG(code);
// 
// 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
// 	auto&& ret = s.solve();
// 
// 	// lookup address of variable b in the last stmt
// 	NodeAddress aRef = NodeAddress(code).getAddressOfChild(3).getAddressOfChild(1).getAddressOfChild(2);
// 	
// 	// Finds the CFG block containing the address of variable b
// 	const cfg::BlockPtr& b = cfg->find(aRef);
// 	EXPECT_EQ(2u, b->getBlockID());
// 
// 	auto access = getImmediateAccess(aRef.as<ExpressionAddress>());
// 
// 	unsigned occurrences=0;
// 	for( auto def : ret[b->getBlockID()] ) {
// 		if ( *std::get<0>(def).getAccessedVariable() == *aRef ) {
// 			EXPECT_EQ(std::get<1>(def), builder.intLit(5));
// 			occurrences++;
// 		}
// 	}
// 	EXPECT_EQ(1u, occurrences);
// 
// }
// 
// 
// TEST(ConstantPropagation, ArrayAlias) {
// 
// 	NodeManager mgr;
// 	parse::IRParser parser(mgr);
// 	IRBuilder builder(mgr);
// 
//     auto code = parser.parseStatement(
// 		"{"
// 		"	decl ref<vector<int<4>,2>>:v=0;"
// 		"	((op<vector.ref.elem>(v, lit<uint<8>,2>)) = 5);"
// 		"	decl ref<int<4>>:c = ((op<vector.ref.elem>(v, lit<uint<8>,2>))+1);"
// 		"	decl int<4>:d = (op<ref.deref>(c));"
// 		"}"
//     );
// 
//     EXPECT_TRUE(code);
// 	CFGPtr cfg = CFG::buildCFG(code);
// 
// 	std::cout << *cfg << std::endl;
// 
// 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
// 	auto&& ret = s.solve();
// 
// 	// lookup address of variable b in the last stmt
// 	NodeAddress aRef = NodeAddress(code).getAddressOfChild(3).getAddressOfChild(1).getAddressOfChild(2);
// 	
// 	// Finds the CFG block containing the address of variable b
// 	const cfg::BlockPtr& b = cfg->find(aRef);
// 	EXPECT_EQ(2u, b->getBlockID());
// 
// 	auto access = getImmediateAccess(aRef.as<ExpressionAddress>());
// 
// 	unsigned occurrences=0;
// 	for( auto def : ret[b->getBlockID()] ) {
// 		if ( *std::get<0>(def).getAccessedVariable() == *aRef ) {
// 			EXPECT_EQ(std::get<1>(def), builder.intLit(6));
// 			occurrences++;
// 		}
// 	}
// 	EXPECT_EQ(1u, occurrences);
// }
// 
// TEST(ConstantPropagation, ArrayAliasReasign) {
// 
// 	NodeManager mgr;
// 	parse::IRParser parser(mgr);
// 	IRBuilder builder(mgr);
// 
//     auto code = parser.parseStatement(
// 		"{"
// 		"	decl ref<vector<int<4>,2>>:v=0;"
// 		"	((op<vector.ref.elem>(v, lit<uint<8>,2>)) = 5);"
// 		"	((op<vector.ref.elem>(v, lit<uint<8>,2>)) = ((op<vector.ref.elem>(v, lit<uint<8>,2>))+1));"
// 		"	decl int<4>:d = (op<ref.deref>((op<vector.ref.elem>(v, lit<uint<8>,2>))));"
// 		"}"
//     );
// 
//     EXPECT_TRUE(code);
// 	CFGPtr cfg = CFG::buildCFG(code);
// 
// 	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
// 	auto ret = s.solve();
// 
// 	// lookup address of variable b in the last stmt
// 	NodeAddress aRef = NodeAddress(code).getAddressOfChild(3).getAddressOfChild(1).getAddressOfChild(2);
// 	
// 	// Finds the CFG block containing the address of variable b
// 	const cfg::BlockPtr& b = cfg->find(aRef);
// 	EXPECT_EQ(3u, b->getBlockID());
// 
// 	auto access = getImmediateAccess(aRef.as<ExpressionAddress>());
// 
// 	unsigned occurrences=0;
// 	//for( auto def : ret[b->getBlockID()] ) {
// 		//if ( isConflicting(std::get<0>(def), access ) ) {
// 			//LOG(INFO) << def << "    " << access;
// 			//EXPECT_EQ(std::get<1>(def), builder.intLit(6));
// 			//occurrences++;
// 		//}
// 	//}
// 	//EXPECT_EQ(1u, occurrences);
// 
// }
