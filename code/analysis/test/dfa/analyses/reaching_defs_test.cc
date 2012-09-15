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

std::set<core::NodeAddress> extractRealAddresses(const AccessClass& cl, const TmpVarMap& map) {

	std::set<core::NodeAddress> ret;
	for( const auto& access : cl) {
		ret.insert( access->getAddress().getAbsoluteAddress(map) );
	}
	
	return ret;
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
	
	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(2u, blockID);
	
	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);

	auto addrSet = ::extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	// std::cout << addrSet << std::endl;

	EXPECT_EQ(2u, addrSet.size());

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
	
	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(2u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);
	
	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto addrSet = ::extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	addrSet.erase(aRef);
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
	
	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(2u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);
	
	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto addrSet = ::extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	addrSet.erase(aRef);
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

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(2u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);
	
	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto addrSet = ::extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	addrSet.erase(aRef);
	EXPECT_EQ(2u, addrSet.size());
	// std::cout << addrSet << std::endl;

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
	
	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(6u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);
	
	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto addrSet = ::extractRealAddresses(*aMgr.getClassFor(thisAccess), cfg->getTmpVarMap());
	addrSet.erase(aRef);
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
		"	$s.a$ = i+b; "
		"	int<4> c = *$s.a$;"
		"}$", symbols
    );
	EXPECT_EQ(3u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto cl = aMgr.getClassFor(thisAccess);

	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);
	EXPECT_EQ(1u, addrList.size());

	EXPECT_EQ(addresses[0].getRootNode(), addrList.begin()->getRootNode());
	EXPECT_EQ(addresses[1], *addrList.begin());
}

////TEST(ReachingDefinitions, StructMemberInitialization) {
////
////	NodeManager mgr;
////	IRBuilder builder(mgr);
////
////    auto addresses = builder.parseAddresses(
////		"${"
////		"	int<4> i = 2; "
////		"	int<4> b = 3; "
////		"	$ref<struct { int<4> a; }> s;$"
////		"	int<4> c = *$s.a$;"
////		"}$"
////    );
////	EXPECT_EQ(3u, addresses.size());
////
////	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());
////	std::cout << *cfg << std::endl;
////
////	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
////	auto ret = s.solve();
////
////	// lookup address of variable A
////	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();
////	
////	std::pair<cfg::BlockPtr, size_t> b = cfg->find(aRef);
////	EXPECT_EQ(3u, b.first->getBlockID());
////
////	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
////	definitionsToAccesses(ret[b.first->getBlockID()], aMgr);
////
////	auto thisAccess = getImmediateAccess(aRef);
////	auto cl = aMgr.getClassFor(thisAccess);
////
////	auto addrList = extractRealAddresses(*cl, cfg->getTmpVarMap());
////
////	EXPECT_EQ(1u, addrList.size());
////
////	EXPECT_EQ(addresses[0].getRootNode(), addrList.begin()->getRootNode());
////	EXPECT_EQ(addresses[1], *addrList.begin());
////}


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
	
	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());

	addrList.erase(aRef);

	EXPECT_EQ(2u, addrList.size());
	auto addrIt = addrList.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrList.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrList.end());
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
	
	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);

	EXPECT_EQ(1u, addrList.size());

	auto addrIt = addrList.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_EQ(++addrIt, addrList.end());
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

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);

	EXPECT_EQ(2u, addrList.size());
	
	auto addrIt = addrList.begin();

	// Makes sure the computed addresses have the same root node 
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrList.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrList.end());
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

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[2].as<ExpressionAddress>();
	
	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);
	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);
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

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);
	EXPECT_EQ(2u, addrList.size()); 

	auto addrIt = addrList.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);

	EXPECT_NE(++addrIt, addrList.end());

	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);

	EXPECT_EQ(++addrIt, addrList.end());
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

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);

	EXPECT_EQ(1u, addrList.size());

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

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);

	EXPECT_EQ(1u, addrList.size());

	auto addrIt = addrList.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);
}

TEST(ReachingDefinitions, VectorsWithControl4) {

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
		"	$v[1u].a$ = i+b; "
		"	for (int<4> i=0..10 : 2) { "
		"		$v[i].a$ = i+b; "
		"	}"
		"	int<4> c = *$v[1u].a$;"
		"}$", symbols
    );
	EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();

	// lookup address of variable A
	ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();

	cfg::Address addr = cfg->find(aRef);
	auto blockID = addr.getBlock().getBlockID();
	EXPECT_EQ(3u, blockID);

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	definitionsToAccesses(ret[blockID], aMgr);

	auto thisAccess = getImmediateAccess(mgr, aRef);

	auto cl = aMgr.getClassFor(thisAccess);
	auto addrList = ::extractRealAddresses(*cl, cfg->getTmpVarMap());
	addrList.erase(aRef);

	EXPECT_EQ(1u, addrList.size());

	auto addrIt = addrList.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[1], *addrIt);
}

