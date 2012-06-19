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

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/problem.h"
#include "insieme/analysis/cfg.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/set_utils.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

TEST(CreateEntity, AtomicEntity) {

	auto e = makeAtomicEntity<VariablePtr>("variables");
	EXPECT_EQ(1u,e.arity());


}

TEST(CreateEntity, CompoundEntity) {

	typedef enum { DEF, USE } DefUse;

	auto e = makeCompoundEntity<VariablePtr, DefUse>("defuse");
	EXPECT_EQ(2u, e.arity());

}


TEST(EntityExtract, VariableExtractor) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"for(decl int<4>:i = 10 .. 50 : 1) { "
		"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"}"
    );

    EXPECT_TRUE(code);

	CFGPtr cfg = CFG::buildCFG(code);

	// Extract VariablePtr
	{ 
		auto dom = extract(dfa::Entity<elem<VariablePtr>>(), *cfg);
		EXPECT_EQ(3u, dom.size());
	}

	// Extract VariableAddress
	{ 
		auto dom = extract(dfa::Entity<elem<VariableAddress>>(), *cfg);
		EXPECT_EQ(4u, dom.size());
	}
}

TEST(EntityExtract, ExpressionExtractor) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"for(decl int<4>:i = 10 .. 50 : 1) { "
		"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"}"
    );

    EXPECT_TRUE(code);

	CFGPtr cfg = CFG::buildCFG(code);

	auto dom = extract(dfa::Entity<elem<ExpressionPtr>>(), *cfg);

	EXPECT_EQ(10u, dom.size());
	EXPECT_TRUE(dfa::isBounded(dom));

	// filter out all builtin literal
	auto twin = filterIterator(dom.begin(), dom.end(), [&mgr](const ExpressionPtr& cur) -> bool { 
			return mgr.getLangBasic().isBuiltIn(cur); 
		} );

	EXPECT_EQ(8u, std::distance(twin.first, twin.second));
}

TEST(EntityExtract, GenLiteralExtractor) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"for(decl int<4>:i = 10 .. 50 : 1) { "
		"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"}"
    );

    EXPECT_TRUE(code);

	CFGPtr cfg = CFG::buildCFG(code);
	auto d = extract(dfa::Entity< dom< dfa::Value<LiteralPtr> > >(), *cfg);

	EXPECT_FALSE( d.bounded() );
	EXPECT_TRUE( d.contains( top ) );
}


TEST(EntityExtract, TypeExtractor) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"for(decl int<4>:i = 10 .. 50 : 1) { "
		"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"}"
    );

    EXPECT_TRUE(code);

	CFGPtr cfg = CFG::buildCFG(code);

	auto dom = extract(dfa::Entity<elem<TypePtr>>(), *cfg);
	EXPECT_EQ(12u, dom.size());

	std::cout << dom << std::endl;
}


TEST(CompoundEntityExtract, VariableTypeExtractor) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"for(decl int<4>:i = 10 .. 50 : 1) { "
		"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"}"
    );

    EXPECT_TRUE(code);

	CFGPtr cfg = CFG::buildCFG(code);

	auto dom1 = extract(dfa::Entity<elem<VariablePtr>>(), *cfg);
	EXPECT_FALSE(dom1.empty());

	VariablePtr v = *dom1.begin();

	auto d = extract(dfa::Entity<elem<VariablePtr>,dom<dfa::Value<TypePtr>>>(), *cfg);

	EXPECT_FALSE( dfa::isBounded(d) );

	EXPECT_TRUE( 
		d.contains( std::make_tuple(v, mgr.getLangBasic().getBool()) ) 
	);

	VariablePtr v1 = IRBuilder(mgr).variable( mgr.getLangBasic().getInt4() );

	EXPECT_FALSE( 
		d.contains( std::make_tuple(v1,mgr.getLangBasic().getBool()) ) 
	);
	EXPECT_TRUE( 
		d.contains( std::make_tuple(v,bottom) ) 
	);

}

TEST(SingleEntity, ValueType) {

	typedef dfa::Entity< dom<int> > e;

	//typename dfa::entity_type_traits<e>::type v = 10;
	//EXPECT_EQ(1u, e::arity());

	//typename dfa::entity_type_traits<e>::type b = dfa::bottom;

	//EXPECT_EQ(typeid(Element<int>), typeid(v));

}

TEST(CompoundEntity, ValueType) {

	NodeManager mgr;

	typedef dfa::Entity< dom<int>, elem<VariablePtr>, dom<double> > e;

	//typename dfa::entity_type_traits<e>::type v = 
	//	std::make_tuple(10, IRBuilder(mgr).variable(mgr.getLangBasic().getBool()), 20.2F);

	//EXPECT_EQ(3u, e::arity());

	//EXPECT_EQ(typeid(std::tuple<int,VariablePtr,double>), typeid(v));

}

