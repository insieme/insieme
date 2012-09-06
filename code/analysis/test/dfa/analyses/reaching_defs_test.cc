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
#include "insieme/analysis/dfa/analyses/def_use.h"

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/solver.h"
#include "insieme/analysis/cfg.h"

#include "insieme/analysis/dfa/lattice.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

typedef std::set<ExpressionAddress> ExprAddrSet;


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
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(2u, b.first->getBlockID());
	
	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
	
	auto thisAccess = getImmediateAccess(aRef);
	auto addrSet = extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());

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

	// std::cout << *cfg << std::endl;
	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	core::VariableAddress aRef = addresses[3].as<VariableAddress>();
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(2u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
	
	auto thisAccess = getImmediateAccess(aRef);
	auto addrSet = extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1].as<DeclarationStmtAddress>()->getVariable(), *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
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
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(2u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
	
	auto thisAccess = getImmediateAccess(aRef);
	auto addrSet = extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
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
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(2u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
	
	auto thisAccess = getImmediateAccess(aRef);
	auto addrSet = extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
}

TEST(ReachingDefinition, ScalarWithLoop) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
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
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(6u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
	
	auto thisAccess = getImmediateAccess(aRef);
	auto addrSet = extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	EXPECT_EQ(2u, addrSet.size());

	auto addrIt = addrSet.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1].as<DeclarationStmtAddress>()->getVariable(), *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[3], *addrIt);
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
		"	$s.a$ = i+b; "
		"	int<4> c = *$s.a$;"
		"}$", symbols
    );
	EXPECT_EQ(3u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
	//std::cout << *cfg << std::endl;

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getImmediateAccess(aRef);
	auto cl = aMgr.getClassFor(thisAccess);

	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(1u, addrList.size());

	EXPECT_EQ(addresses[0].getRootNode(), addrList.begin()->getRootNode());
	EXPECT_EQ(addresses[1], *addrList.begin());
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
//	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
//	EXPECT_EQ(3u, b.first->getBlockID());
//
//	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
//	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
//
//	auto thisAccess = getImmediateAccess(aRef);
//	auto cl = aMgr.getClassFor(thisAccess);
//
//	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());
//
//	EXPECT_EQ(1u, addrList.size());
//
//	EXPECT_EQ(addresses[0].getRootNode(), addrList.begin()->getRootNode());
//	EXPECT_EQ(addresses[1], *addrList.begin());
//}


TEST(ReachingDefinitions, StructMemberWithControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["s"] = builder.variable(builder.parseType("ref<struct{ int<4> a; int<4> b; }>"));

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$s.a$ = 3; "
		"	if ( s.a <= 0 ) { "
		"		$s.a$ = i+b; "
		"	}"
		"	int<4> c = *$s.a$;"
		"}$", symbols
    );
    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
	
	// std::cout << *cfg << std::endl;
	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getImmediateAccess(aRef);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(2u, addrList.size());
	auto addrIt = addrList.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
}

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
	// std::cout << *cfg << std::endl;
	
	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getCFGBasedAccess(aRef,cfg);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(2u, addrList.size());

	auto addrIt = addrList.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[3], *addrIt);

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
		"	$s.b.b.b$ = 3; "
		"	if ( s.a <= 0 ) { "
		"		$s.a$ = i+b; "
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
	
	std::pair<cfg::BlockPtr,size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getCFGBasedAccess(aRef,cfg);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(2u, addrList.size());

	auto addrIt = addrList.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[3], *addrIt);
}

//=============================================================================
// ARRAYS 
//=============================================================================
TEST(ReachingDefinitions, VectorsNoControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>, 10>>"));

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$v[2u]$ = i+b; "
		"	int<4> c = *$v[2u]$;"
		"}$", symbols
    );
	EXPECT_EQ(3u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
	//std::cout << *cfg << std::endl;

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getImmediateAccess(aRef);
	auto cl = aMgr.getClassFor(thisAccess);

	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(1u, addrList.size());

	EXPECT_EQ(addresses[0].getRootNode(), addrList.begin()->getRootNode());
	EXPECT_EQ(addresses[1], *addrList.begin());
}

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
	//std::cout << *cfg << std::endl;

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getCFGBasedAccess(aRef, cfg);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(3u, addrList.size()); //FIXME

	auto addrIt = addrList.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	++addrIt;

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
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
	//std::cout << *cfg << std::endl;

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getCFGBasedAccess(aRef, cfg);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(2u, addrList.size()); //FIXME

	auto addrIt = addrList.begin();
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
	//std::cout << *cfg << std::endl;

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
	
	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
	EXPECT_EQ(3u, b.first->getBlockID());

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);

	auto thisAccess = getCFGBasedAccess(aRef, cfg);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());

	EXPECT_EQ(3u, addrList.size()); //FIXME

	auto addrIt = addrList.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);
}

//TEST(Problem, ReachingDefinitions2) {
//
//	NodeManager mgr;
//	parse::IRParser parser(mgr);
//
//    auto code = parser.parseStatement(
//		"{"
//		"	decl ref<int<4>>:a = 0;"
//		"	if ( (a<=0) ) { "
//		"		(a = (int<4>:i+int<4>:b)); "
//		"	};"
//		"	decl int<4>:c = (op<ref.deref>(a));"
//		"}"
//    );
//
//    EXPECT_TRUE(code);
//
//	analyses::DefUse du(code);
//
//	VariableAddress aRef = core::static_address_cast<const core::Variable>( 
//			NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2)
//		);
//	
//	EXPECT_EQ(2u, std::distance(du.defs_begin(aRef), du.defs_end(aRef)));
//	
//	auto comp = [](const ExpressionAddress& lhs, const ExpressionAddress& rhs) -> bool { 
//		assert(lhs.getRootNode() == rhs.getRootNode());
//		return lhs.getPath() < rhs.getPath();
//	};
//
//	std::set<ExpressionAddress, decltype(comp)> definitions(du.defs_begin(aRef), du.defs_end(aRef), comp);
//	
//	auto it = definitions.begin(), end = definitions.end();
//
//	{
//		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
//			NodeAddress(code).getAddressOfChild(0).getAddressOfChild(0)
//		);
//
//		EXPECT_EQ(trgRef, *it);
//		EXPECT_EQ(*trgRef, **it);
//	}
//	++it;
//	{
//		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
//			NodeAddress(code).getAddressOfChild(1).getAddressOfChild(1).getAddressOfChild(0).getAddressOfChild(2)
//		);
//	
//		EXPECT_EQ(trgRef, *it);
//		EXPECT_EQ(*trgRef, **it);
//	}
//	++it;
//
//	EXPECT_EQ(end,it);
//}
//
//
//TEST(Problem, ReachingDefinitions3) {
//
//	NodeManager mgr;
//	parse::IRParser parser(mgr);
//
//    auto code = parser.parseStatement(
//		"{"
//		"	decl ref<int<4>>:a = 0;"
//		"	if ( (a<=0) ) { "
//		"		(a = (int<4>:i+int<4>:b)); "
//		"	} else {"
//		"		(a = int<4>:b);"
//		"   };"
//		"	decl int<4>:c = (op<ref.deref>(a));"
//		"}"
//    );
//
//    EXPECT_TRUE(code);
//
//	analyses::DefUse du(code);
//
//	VariableAddress aRef = core::static_address_cast<const core::Variable>( 
//			NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2)
//		);
//	
//	EXPECT_EQ(2u, std::distance(du.defs_begin(aRef), du.defs_end(aRef)));
//
//	analyses::DefUse::iterator it = du.defs_begin(aRef), end = du.defs_end(aRef);
//
//	std::vector<NodeAddress> l { 
//		NodeAddress(code).getAddressOfChild(1).getAddressOfChild(1).getAddressOfChild(0).getAddressOfChild(2),
//		NodeAddress(code).getAddressOfChild(1).getAddressOfChild(2).getAddressOfChild(0).getAddressOfChild(2)
//	};
//
//	auto fit1 = std::find(it, end, l[0]);
//	EXPECT_NE(end, fit1);
//
//	auto fit2 = std::find(it, end, l[1]);
//	EXPECT_NE(end, fit2);
//}
//
//
//TEST(Problem, ReachingDefinitionsMember) {
//
//	NodeManager mgr;
//	parse::IRParser parser(mgr);
//
//    auto code = parser.parseStatement(
//		"{"
//		"	decl ref<struct<a:int<4>, b:int<4>>>:s=0;"
//	//	"	if ( (int<4>:b <= 0) ) { "
//		"	((op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>,int>)) = 3);"
//	//	"   };"
//		"	decl ref<int<4>>:c = (op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>, int>));"
//		"}"
//    );
//
//    EXPECT_TRUE(code);
//
//	analyses::DefUse du(code);
//
//	ExpressionAddress aRef = core::static_address_cast<const core::Expression>( 
//			NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1)
//		);
//	
//	auto var = getImmediateAccess(aRef);
//
//	EXPECT_EQ(2u, std::distance(du.defs_begin(aRef), du.defs_end(aRef)));
//
//	auto comp = [](const ExpressionAddress& lhs, const ExpressionAddress& rhs) -> bool { 
//		assert(lhs.getRootNode() == rhs.getRootNode());
//		return lhs.getPath() < rhs.getPath();
//	};
//
//	std::set<ExpressionAddress, decltype(comp)> definitions(du.defs_begin(aRef), du.defs_end(aRef), comp);
//	
//	auto it = definitions.begin(), end = definitions.end();
//
//	{
//		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
//			NodeAddress(code).getAddressOfChild(0).getAddressOfChild(0)
//		);
//
//		EXPECT_EQ(trgRef, *it);
//		EXPECT_EQ(*trgRef, **it);
//	}
//	++it;
//	{
//		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
//			NodeAddress(code).getAddressOfChild(1).getAddressOfChild(2)
//		);
//	
//		EXPECT_EQ(trgRef, *it);
//		EXPECT_EQ(*trgRef, **it);
//	}
//	++it;
//
//	EXPECT_EQ(end,it);
//
//}
