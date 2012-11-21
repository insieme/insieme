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

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/solver.h"
#include "insieme/analysis/cfg.h"

#include "insieme/analysis/dfa/lattice.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;


typedef std::set<ExpressionAddress> ExprAddrSet;

//ExprAddrSet cleanup(const ExprAddrSet& addrSet) {
//	ExprAddrSet ret;
//	
//	if (addrSet.empty()) { return ret; }
//
//	auto it = addrSet.begin(), end = addrSet.end();
//
//	while (it != end) {
//		ret.insert(*it);
//		decltype(it) refIt;
//		while( (refIt = it) != end && (++it != end) && isChildOf(*refIt, *it) ) ; 
//	}
//	return ret;
//}

ExprAddrSet lookup_accesses(const AccessClassSet& classes, const CFGPtr& cfg) {
	ExprAddrSet ret;
	for (const auto& cl : classes) {
		auto addrs = extractRealAddresses(*cl, cfg->getTmpVarMap());
		std::copy(addrs.begin(), addrs.end(), std::inserter(ret, ret.begin()));
	}
	return ret;
}


ExprAddrSet getDefinitions(
		const Solver<dfa::analyses::ReachingDefinitions>::CFGBlockMap& ret, 
		const CFGPtr& 			cfg, 
		const NodeAddress& 		use) 
{
	
	Solver<dfa::analyses::ReachingDefinitions>::printDataflowData(std::cout, ret);;
	
	cfg::Address addr = cfg->find(use);
	auto blockID = addr.getBlock().getBlockID();

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	auto fit = ret.find(blockID);
	EXPECT_NE(fit, ret.end());
	 
	insieme::analysis::dfa::analyses::definitionsToAccesses(fit->second, aMgr);

	auto thisAccess = getImmediateAccess(use->getNodeManager(), use);

	auto classes = aMgr.getClassFor(thisAccess);
	LOG(INFO) << aMgr;
	// LOG(INFO) << "From access: " << classes;

	// now get all the conflicting accesses 
	auto confClasses = getConflicting(classes);
	std::copy(classes.begin(), classes.end(), std::inserter(confClasses, confClasses.begin()));

	// LOG(INFO) << "Conficting: " << confClasses;

	ExprAddrSet addrSet = lookup_accesses(confClasses, cfg);

	// aMgr.printDotGraph(std::cout);
	// remove the address of the use 
	addrSet.erase(use.as<ExpressionAddress>());

	// LOG(INFO) << addrSet; 
	return addrSet;
}

//=============================================================================
// Scalars 
//=============================================================================
TEST(ReachingDefinition, ScalarNoControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$ref<int<4>> a = 0;$ "
		"	$a$ = i+b; "
		"	int<4> c = *$a$;"
		"}$"
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	core::VariableAddress aRef = addresses[3].as<VariableAddress>();
		
	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
}

TEST(ReachingDefinition, ScalarWithControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$ref<int<4>> a = 0;$ "
		"	if ( a <= 0 ) { "
		"		$a$ = i+b; "
		"	}"
		"	int<4> c = *$a$;"
		"}$"
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	core::VariableAddress aRef = addresses[3].as<VariableAddress>();
	
	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1].as<DeclarationStmtAddress>()->getVariable(), *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}

TEST(ReachingDefinition, ScalarWithControl2) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	ref<int<4>> a = 0; "
		"	if ( a <= 0 ) { "
		"		$a$ = i+b; "
		"	} else {"
		"		$a$ = 3; "	
		"	}"
		"	int<4> c = *$a$;"
		"}$"
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	VariableAddress aRef = addresses[3].as<VariableAddress>();
	
	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}

TEST(ReachingDefinition, ScalarWithControl3) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	ref<int<4>> a = 0; "
		"	$a$ = 2; "
		"	while ( a <= 0 ) { "
		"		a = i+b; "
		"		$a$ = 3; "	
		"	}"
		"	int<4> c = *$a$;"
		"}$"
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	VariableAddress aRef = addresses[3].as<VariableAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
	
	EXPECT_EQ(++addrIt, addrSet.end());
}

TEST(ReachingDefinition, ScalarWithLoop) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	ref<int<4>> b = 3; "
		"	$ref<int<4>> a = 0;$ "
		"	while ( a <= 0 ) { "
		"		b = $a$;   "
		"		$a$ = i+b; "
		"	}"
		"	int<4> c = *$a$;"
		"}$"
    );
    EXPECT_EQ(5u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	VariableAddress aRef = addresses[2].as<VariableAddress>();
	
	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1].as<DeclarationStmtAddress>()->getVariable(), *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[3], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}

//=============================================================================
// STRUCTS 
//=============================================================================
TEST(ReachingDefinitions, StructMemberNoControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["s"] = builder.variable(builder.parseType("ref<struct{ int<4> a; int<4> b; }>"));

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$s.a$ = i+b;  "
		"	int<4> c = *$s.a$; "
		"}$", symbols
    );
	EXPECT_EQ(3u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(1u, addrSet.size());

	EXPECT_EQ(addresses[0].getRootNode(), addrSet.begin()->getRootNode());
	EXPECT_EQ(addresses[1], *addrSet.begin());
}

//TEST(ReachingDefinitions, StructMemberInitialization) {
//
//	NodeManager mgr;
//	IRBuilder builder(mgr);
//
//    auto addresses = builder.parseAddresses(
//		"${"
//		"	int<4> i = 2; "
//		"	int<4> b = 3; "
//		"	$ref<struct { int<4> a; }> s;$"
//		"	int<4> c = *$s.a$;"
//		"}$"
//    );
//	EXPECT_EQ(3u, addresses.size());
//
//	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
//	std::cout << *cfg << std::endl;
//
//	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
//	auto ret = s.solve();
//
//	// lookup address of variable A
//	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();
//
//	auto addrSet = getDefinitions(ret, cfg, aRef);
//	EXPECT_EQ(1u, addrSet.size());
//
//	EXPECT_EQ(addresses[0].getRootNode(), addrSet.begin()->getRootNode());
//	EXPECT_EQ(addresses[1], *addrSet.begin());
//}

//TEST(ReachingDefinitions, StructMemberWithControl) {
//
//	NodeManager mgr;
//	IRBuilder builder(mgr);
//
//    auto addresses = builder.parseAddresses(
//		"${"
//		"	ref<struct{ int<4> a; int<4> b; }> s; "
//		"	int<4> i = 2; "
//		"	int<4> b = 3; "
//		"	$s.a$ = 3; "
//		"	if ( s.a <= 0 ) { "
//		"		$s.a$ = i+b; "
//		"	}"
//		"	int<4> c = *$s.a$;"
//		"}$"
//    );
//    EXPECT_EQ(4u, addresses.size());
//
//	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
//	
//	// std::cout << *cfg << std::endl;
//	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
//	auto ret = s.solve();
//
//	// lookup address of variable A
//	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
//	
//	auto addrSet = getDefinitions(ret, cfg, aRef);
//	EXPECT_EQ(2u, addrSet.size());
//	auto addrIt = addrSet.begin();
//
//	// Makes sure the computed addresses have the same root node 
//	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
//	EXPECT_EQ(addresses[1], *addrIt);
//
//	EXPECT_NE(++addrIt, addrSet.end());
//
//	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
//	EXPECT_EQ(addresses[2], *addrIt);
//
//	EXPECT_EQ(++addrIt, addrSet.end());
//}

TEST(ReachingDefinitions, StructMemberNested) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["s"] = builder.variable(builder.parseType("ref<struct{ int<4> a; struct{ int<4> b; } b; }>"));

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$s.b.b$ = 3; "
		"	if ( s.a <= 0 ) { "
		"		$s.a$ = i+b; "
		"	}"
		"	int<4> c = *$s.b.b$;"
		"}$", symbols
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
	
	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);

	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}

TEST(ReachingDefinitions, StructMemberNested2) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["s"] = builder.variable(builder.parseType(
			"ref<struct{ int<4> a; struct{ int<4> a; struct { int<4> b; } b; } b; }>"
		));

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$s.b.b$ = s.b.b; "
		"	s.a = 5; "
		"	if ( s.a <= 0 ) { "
		"		$s.b.b.b$ = i+b; "
		"	}"
		"	int<4> c = *$s.b.b.b$;"
		"}$", symbols
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
	// std::cout << *cfg << std::endl;
	
	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size());
	
	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}

//=============================================================================
// ARRAYS 
//=============================================================================
//TEST(ReachingDefinitions, VectorsNoControl) {
//
//	NodeManager mgr;
//	IRBuilder builder(mgr);
//
//    auto addresses = builder.parseAddresses(
//		"${"
//		"	ref<vector<int<4>, 10>> v; "
//		"	int<4> i = 2; "
//		"	v[2u] = i; "
//		"	int<4> b = 3; "
//		"	$v[2u]$ = i+b; "
//		"	int<4> c = *$v[2u]$;"
//		"}$"
//    );
//	EXPECT_EQ(3u, addresses.size());
//
//	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
//
//	LOG(INFO) << *cfg;
//
//	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
//	auto ret = s.solve();
//
//	// lookup address of variable A
//	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();
//	
//	auto addrSet = getDefinitions(ret, cfg, aRef);
//	EXPECT_EQ(1u, addrSet.size());
//
//	EXPECT_EQ(addresses[0].getRootNode(), addrSet.begin()->getRootNode());
//	EXPECT_EQ(addresses[1], *addrSet.begin());
//}

TEST(ReachingDefinitions, VectorsWithControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<struct{int<4> a; int<2> b;}, 10>>"));

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$v[2u].a$ = i+b; "
		"	while (i<b) { "
		"		$v[2u].a$ = i+b; "
		"	}"
		"	int<4> c = *$v[2u].a$;"
		"}$", symbols
    );
	EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size()); 

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}


TEST(ReachingDefinitions, VectorsWithControl2) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
			builder.parseType("ref<vector<struct{int<4> a; int<2> b;}, 10>>")
		);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$v[2u].a$ = i+b; "
		"	while (i<b) { "
		"		$v[2u].b$ = i+b; "
		"	}"
		"	int<4> c = *$v[2u].a$;"
		"}$", symbols
    );
	EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);
}

TEST(ReachingDefinitions, VectorsWithControl3) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
			builder.parseType("ref<vector<struct{int<4> a; int<2> b;}, 10>>")
		);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$v[2u].a$ = i+b; "
		"	while (i<b) { "
		"		$v[3u].a$ = i+b; "
		"	}"
		"	int<4> c = *$v[2u].a$;"
		"}$", symbols
    );
	EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);
}

//TEST(ReachingDefinitions, VectorsWithControl4) {
//
//	NodeManager mgr;
//	IRBuilder builder(mgr);
//
//	std::map<std::string, core::NodePtr> symbols;
//	symbols["v"] = builder.variable(
//		builder.parseType("ref<vector<uint<4>,10>>")
//	);
//
//	auto addresses = builder.parseAddresses(
//	"${"
//	"	uint<4> a = 2u; "
//	"	uint<4> b = 3u; "
//	"	$v[1u]$ = a+b; "
//	"	for (uint<4> i=0u..10u : 2u) { "
//	"		$v[i]$ = a+b; "
//	"	}"
//	"	uint<4> c = *$v[1u]$;"
//	"	uint<4> d = *$v[2u]$;"
//	"}$", symbols
//	);
//	EXPECT_EQ(5u, addresses.size());
//
//	// mark for polyhedral 
//	polyhedral::scop::mark(addresses[0]);
//
//	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
//
//	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
//	auto ret = s.solve();
//   
//	{
//		// lookup address of variable A
//		ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
//
//		auto addrSet = getDefinitions(ret, cfg, aRef);
//		EXPECT_EQ(1u, addrSet.size());
//
//		auto addrIt = addrSet.begin();
//		EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
//		EXPECT_EQ(addresses[1], *addrIt);
//	}
//
//	{
//		// lookup address of variable A
//		ExpressionAddress aRef = addresses[4].as<ExpressionAddress>();
//
//		auto addrSet = getDefinitions(ret, cfg, aRef);
//		EXPECT_EQ(1u, addrSet.size());
//
//		auto addrIt = addrSet.begin();
//		EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
//		EXPECT_EQ(addresses[2], *addrIt);
//	}
//
//}

TEST(ReachingDefinitions, Vectors2DWithControl5) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
		builder.parseType("ref<vector<vector<uint<4>,10>,10>>")
	);

	auto addresses = builder.parseAddresses(
	"${"
	"	uint<4> a = 2u; "
	"	uint<4> b = 3u; "
	"	$v[1u][2u]$ = a+b; "
	"	for (uint<4> i=0u..10u : 2u) { "
	"		$v[1u][i]$ = a+b; "
	"	}"
	"	uint<4> c = *$v[1u][2u]$;"
	"}$", symbols
	);
	EXPECT_EQ(4u, addresses.size());

	// mark for polyhedral 
	polyhedral::scop::mark(addresses[0]);

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();
   
	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());

}


TEST(ReachingDefinitions, Vectors2DWithControl6) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	auto addresses = builder.parseAddresses(
	"${"
	"	ref<vector<uint<4>,10>> v; "
	"	uint<4> a = 2u; "
	"	uint<4> b = 3u; "
	" 	{ "
	"		$v[2]$ = 3; "
	"		for (int<4> i=0..10) { "
	"			$v[i]$ = a+b; "
	"		}"
	"		*$v[2u]$;"
	"	} "
	"}$"
	);
	EXPECT_EQ(4u, addresses.size());

	// mark for polyhedral 
	polyhedral::scop::mark(addresses[0]);

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();
   
	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(3u, addrSet.size());

	auto addrIt = addrSet.begin();
	++addrIt; // skip the declaration of vector 

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrSet.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}

//TEST(ReachingDefinitions, Vectors2DWithControl6) {
//
//	NodeManager mgr;
//	IRBuilder builder(mgr);
//
//	std::map<std::string, core::NodePtr> symbols;
//	symbols["v"] = builder.variable(
//		builder.parseType("ref<vector<vector<vector<uint<4>,10>,10>,10>>")
//	);
//
//	auto addresses = builder.parseAddresses(
//	"${"
//	"	uint<4> a = 2u; "
//	"	uint<4> b = 3u; "
//	"	$v[1u][2u][2u]$ = a+b; "
//	"	for (uint<4> i=0u..10u : 2u) { "
//	"		$v[1u][i][2u]$ = a+b; "
//	"	}"
//	"	v[1u][1u][2u] = a+b; "
//	"	uint<4> c = *$v[1u][2u][2u]$;"
//	"}$", symbols
//	);
//	EXPECT_EQ(4u, addresses.size());
//
//	// mark for polyhedral 
//	polyhedral::scop::mark(addresses[0]);
//
//	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
//
//	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
//	auto ret = s.solve();
//   
//	// lookup address of variable A
//	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
//
//	auto addrSet = getDefinitions(ret, cfg, aRef);
//	EXPECT_EQ(2u, addrSet.size());
//
//	auto addrIt = addrSet.begin();
//	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
//	EXPECT_EQ(addresses[1], *addrIt);
//
//	++addrIt;
//	EXPECT_EQ(addresses[2], *addrIt);
//
//	EXPECT_EQ(++addrIt, addrSet.end());
//}


TEST(ReachingDefinitions, Vectors2DWithControl7) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
		builder.parseType("ref<vector<vector<vector<uint<4>,10>,10>,10>>")
	);

	auto addresses = builder.parseAddresses(
	"${"
	"	uint<4> a = 2u; "
	"	uint<4> b = 3u; "
	"	$v[1u][2u][2u]$ = a+b; "
	"	for (uint<4> i=0u..10u : 2u) { "
	"		$v[0u][i][2u]$ = a+b; "
	"	}"
	"	v[1u][1u][2u] = a+b; "
	"	uint<4> c = *$v[1u][2u][2u]$;"
	"}$", symbols
	);
	EXPECT_EQ(4u, addresses.size());

	// mark for polyhedral 
	polyhedral::scop::mark(addresses[0]);

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();
   
	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());

}

TEST(ReachingDefinitions, Vectors2DWithControl8) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
		builder.parseType("ref<vector<vector<vector<uint<4>,10>,10>,10>>")
	);

	auto addresses = builder.parseAddresses(
	"${"
	"	uint<4> a = 2u; "
	"	uint<4> b = 3u; "
	"	v[1u][1u][2u] = a+b; "
	"	for (uint<4> i=0u..10u : 2u) { "
	"		v[1u][i][2u] = a+b; "
	"	}"
	"	$v[1u][1u][2u]$ = a+b; "
	"	uint<4> c = *$v[1u][1u][2u]$;"
	"}$", symbols
	);
	EXPECT_EQ(3u, addresses.size());

	// mark for polyhedral 
	polyhedral::scop::mark(addresses[0]);

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();
   
	// lookup address of variable A
	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();

	auto addrSet = getDefinitions(ret, cfg, aRef);
	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_EQ(++addrIt, addrSet.end());
}


