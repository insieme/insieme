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

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

TEST(ConstantPropagation, PropagateConstant) {

	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 1;"
		"	if ( (a<=0) ) { "
		"		(a = 1); "
		"	};"
		"	decl int<4>:c = (op<ref.deref>(a));"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& ret = s.solve();

	// lookup address of variable A
	NodeAddress aRef = NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2);
	
	// Finds the CFG block containing the address of variable a
	const cfg::BlockPtr& b = cfg->find(aRef);
	EXPECT_EQ(2u, b->getBlockID());

	auto access = makeAccess(aRef.as<ExpressionAddress>());

	unsigned occurrences=0;
	for( auto def : ret[b->getBlockID()] ) {
		if (isConflicting(std::get<0>(def),access)) {
			EXPECT_EQ(std::get<1>(def), builder.intLit(1));
			occurrences++;
		}
	}
	EXPECT_EQ(1u, occurrences);

}

TEST(ConstantPropagation, PropagateNotConstant) {

	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 1;"
		"	if ( (a<=0) ) { "
		"		(a = 2); "
		"	};"
		"	decl int<4>:c = (op<ref.deref>(a));"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& ret = s.solve();
	
	// lookup address of variable a
	NodeAddress aRef = NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2);
	
	// Finds the CFG block containing the address of variable a
	const cfg::BlockPtr& b = cfg->find(aRef);
	EXPECT_EQ(2u, b->getBlockID());

	unsigned occurrences=0;
	for( auto def : ret[b->getBlockID()] ) {
		if (std::get<0>(def) == makeAccess(aRef.as<ExpressionAddress>())) {
			EXPECT_EQ(std::get<1>(def), dfa::bottom);
			occurrences++;
		}
	}
	EXPECT_EQ(1u, occurrences);

}

TEST(ConstantPropagation, TransitivePropagation) {

	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 1;"
		"	decl int<4>:b = (10+a);"
		"	if ( (a<=0) ) { "
		"		(a = 2); "
		"	};"
		"	decl int<4>:c = (op<ref.deref>(b));"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& ret = s.solve();
	
	// lookup address of variable b in the last stmt
	NodeAddress aRef = NodeAddress(code).getAddressOfChild(3).getAddressOfChild(1).getAddressOfChild(2);
	
	// Finds the CFG block containing the address of variable b
	const cfg::BlockPtr& b = cfg->find(aRef);
	EXPECT_EQ(2u, b->getBlockID());

	auto access = makeAccess(aRef.as<ExpressionAddress>());

	unsigned occurrences=0;
	for( auto def : ret[b->getBlockID()] ) {
		if ( isConflicting(std::get<0>(def), access) ) {
			EXPECT_EQ(std::get<1>(def), builder.intLit(11));
			occurrences++;
		}
	}
	EXPECT_EQ(1u, occurrences);

}

TEST(ConstantPropagation, Aliasing) {

	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<struct<a:int<4>, b:int<4>>>:s=0;"
		"	((op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>,int>)) = 5);"
		"	decl ref<int<4>>:c = (op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>, int>));"
		"	decl int<4>:d = (op<ref.deref>(c));"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& ret = s.solve();

	// lookup address of variable b in the last stmt
	NodeAddress aRef = NodeAddress(code).getAddressOfChild(3).getAddressOfChild(1).getAddressOfChild(2);
	
	// Finds the CFG block containing the address of variable b
	const cfg::BlockPtr& b = cfg->find(aRef);
	EXPECT_EQ(2u, b->getBlockID());

	auto access = makeAccess(aRef.as<ExpressionAddress>());

	unsigned occurrences=0;
	for( auto def : ret[b->getBlockID()] ) {
		if ( isConflicting(std::get<0>(def), access) ) {
			EXPECT_EQ(std::get<1>(def), builder.intLit(5));
			occurrences++;
		}
	}
	EXPECT_EQ(1u, occurrences);

}

