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

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

TEST(ScopRegion, CompoundStmt) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));

    auto compStmt = builder.parseStmt(
		"{ "
		"	int<4> b = 20; "
		"	v[a+b]; "
		"}", symbols
	);
	// Mark scops in this code snippet
	scop::mark(compStmt);

	EXPECT_TRUE(compStmt->hasAnnotation(scop::ScopRegion::KEY));
}

TEST(ScopRegion, IfStmt) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["c"] = builder.variable(builder.parseType("int<4>"));
	symbols["d"] = builder.variable(builder.parseType("int<4>"));

    auto ifStmt = analysis::normalize(builder.parseStmt(
		"if( c <= d ){ "
		"	v[a-b]; "
		"} else { "
		"	v[a+b]; "
		"}", symbols)).as<IfStmtPtr>();

	EXPECT_TRUE(ifStmt);

	// Mark scops in this code snippet
	scop::mark(ifStmt);
	EXPECT_TRUE(ifStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annIf = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
	IterationVector iterVec = annIf.getIterationVector();
	EXPECT_EQ(5u, iterVec.size());

	EXPECT_EQ(0u, iterVec.getIteratorNum());
	EXPECT_EQ(4u, iterVec.getParameterNum());
	EXPECT_EQ("(|v4,v5,v2,v3|1)", toString(iterVec));

	EXPECT_FALSE( annIf.getDomainConstraints().empty() );


	EXPECT_TRUE(ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annThen = *ifStmt->getThenBody()->getAnnotation(scop::ScopRegion::KEY);
	iterVec = annThen.getIterationVector();
	EXPECT_EQ(3u, iterVec.size());

	EXPECT_EQ(0u, iterVec.getIteratorNum());
	EXPECT_EQ(2u, iterVec.getParameterNum());
	EXPECT_EQ("(|v2,v3|1)", toString(iterVec));

	EXPECT_FALSE( annThen.getDomainConstraints().empty() );

	EXPECT_TRUE(ifStmt->getElseBody()->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annElse = *ifStmt->getElseBody()->getAnnotation(scop::ScopRegion::KEY);
	iterVec = annElse.getIterationVector();

	EXPECT_EQ(3u, iterVec.size());

	EXPECT_EQ(0u, iterVec.getIteratorNum());
	EXPECT_EQ(2u, iterVec.getParameterNum());
	EXPECT_EQ("(|v2,v3|1)", toString(iterVec));

	EXPECT_FALSE( annElse.getDomainConstraints().empty() );
}

TEST(ScopRegion, SimpleForStmt) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for(int<4> i = 10 .. 50 : 1) { "
		"	v[i+b]; "
		"}", symbols)).as<ForStmtPtr>();

	EXPECT_TRUE(forStmt);
	
	scop::mark(forStmt);

	EXPECT_TRUE(forStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);

	EXPECT_EQ(1u, ann.getDirectRegionStmts().size());
	IterationVector iterVec = ann.getIterationVector();
	EXPECT_EQ(3u, iterVec.size()) << iterVec;
	EXPECT_EQ(1u, iterVec.getIteratorNum()) << iterVec;
	EXPECT_EQ(1u, iterVec.getParameterNum()) << iterVec;

	EXPECT_EQ("(v0|v2|1)", toString(iterVec));
	EXPECT_EQ("((v0 + -10 >= 0) ^ (v0 + -50 < 0))", toString(ann.getDomainConstraints()));
	EXPECT_TRUE(forStmt->getBody()->hasAnnotation(scop::ScopRegion::KEY));
}

TEST(ScopRegion, ForStmt) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["n"] = builder.variable(builder.parseType("int<4>"));
	symbols["h"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for(int<4> i = 10 .. 50 : 1) { "
		"	v[i*b]; "
		"	if (i > 25) { "
		"		h = v[n+i-1]; "
		"	}"
		"}", symbols)).as<ForStmtPtr>();

	EXPECT_TRUE(forStmt);

	scop::mark(forStmt);

	EXPECT_FALSE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );
	IfStmtPtr ifStmt = forStmt->getBody().as<CompoundStmtPtr>()->getStatements().back().as<IfStmtPtr>();

	EXPECT_TRUE(ifStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_TRUE(ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY));

	// check the then body
	scop::ScopRegion& ann = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(3u, iterVec.size());
	EXPECT_EQ(0u, iterVec.getIteratorNum());
	EXPECT_EQ(2u, iterVec.getParameterNum());
	EXPECT_EQ("(|v0,v3|1)", toString(iterVec));

}

TEST(ScopRegion, ForStmt2) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["lb"] = builder.variable(builder.parseType("int<4>"));
	symbols["ub"] = builder.variable(builder.parseType("int<4>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for(int<4> i = lb .. ub : 1) { "
		"	v[i+b]; "
		"}", symbols)).as<ForStmtPtr>();

	EXPECT_TRUE(forStmt);

	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(5u, iterVec.size());
	EXPECT_EQ(1u, iterVec.getIteratorNum());
	EXPECT_EQ(3u, iterVec.getParameterNum());
	EXPECT_EQ("(v0|v2,v3,v4|1)", toString(iterVec));
	EXPECT_EQ("((v0 + -v3 >= 0) ^ (v0 + -v4 < 0))",  toString(ann.getDomainConstraints()));
}

TEST(ScopRegion, ForStmt3) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["lb"] = builder.variable(builder.parseType("int<4>"));
	symbols["ub"] = builder.variable(builder.parseType("int<4>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for(int<4> i = lb .. ub : 5) { "
		"	v[i+b]; "
		"}", symbols)).as<ForStmtPtr>();

	EXPECT_TRUE(forStmt);

	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(6u, iterVec.size());
	EXPECT_EQ(2u, iterVec.getIteratorNum());
	EXPECT_EQ(3u, iterVec.getParameterNum());
	
	EXPECT_EQ(Element::ITER, iterVec[1].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[1]).isExistential());
	EXPECT_EQ("(v0,v11|v2,v3,v4|1)", toString(iterVec));
	EXPECT_EQ("(((v0 + -v3 >= 0) ^ (v0 + -v4 < 0)) ^ (v0 + -5*v11 + -v3 == 0))", toString(ann.getDomainConstraints()));
}

TEST(ScopRegion, ForStmt4) {
	using namespace insieme::core::arithmetic;

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for( int<4> i = cloog.floor(5, 2) .. 20 : 5) { "
		"	v[i+b]; "
		"}", symbols)).as<ForStmtPtr>();

	EXPECT_TRUE(forStmt);

	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(6u, iterVec.size());
	EXPECT_EQ(4u, iterVec.getIteratorNum());
	EXPECT_EQ(1u, iterVec.getParameterNum());
	
	EXPECT_EQ(Element::ITER, iterVec[1].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[1]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[2].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[2]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[3].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[3]).isExistential());

	EXPECT_EQ("(v0,v9,v10,v11|v2|1)",toString(iterVec));
	EXPECT_EQ("((((((-2*v9 + -v10 + 5 == 0) ^ (v10 + -2 < 0)) ^ (v10 >= 0)) ^ (v0 + -v9 >= 0)) ^ (v0 + -20 < 0)) ^ (v0 + -v9 + -5*v11 == 0))", toString(ann.getDomainConstraints()));
	
	// we solve the system and we make sure that the domain of the if statement contains exactly 4 elements 
	Piecewise pw = cardinality(mgr,  ann.getDomainConstraints());
	EXPECT_TRUE(pw.isFormula());
	EXPECT_EQ(Formula(4), pw.toFormula());
	EXPECT_EQ("4 -> if true", toString(pw));
}

TEST(ScopRegion, ForStmt5) {
	using namespace insieme::core::arithmetic;

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["ub"] = builder.variable(builder.parseType("int<4>"));
	symbols["lb"] = builder.variable(builder.parseType("int<4>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for(int<4> i = cloog.ceil(lb, 3) .. ub : 5) { "
		"	v[i+b]; "
		"}", symbols)).as<ForStmtPtr>();

	EXPECT_TRUE(forStmt);

	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(8u, iterVec.size());
	EXPECT_EQ(4u, iterVec.getIteratorNum());
	EXPECT_EQ(3u, iterVec.getParameterNum());
	
	EXPECT_EQ(Element::ITER, iterVec[1].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[1]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[2].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[2]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[3].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[3]).isExistential());

	EXPECT_EQ("(v0,v11,v12,v13|v2,v4,v3|1)", toString(iterVec));
	EXPECT_EQ("((((((-3*v11 + v12 + v4 == 0) ^ (v12 + -3 < 0)) ^ (v12 >= 0)) ^ (v0 + -v11 >= 0)) ^ (v0 + -v3 < 0)) ^ (v0 + -v11 + -5*v13 == 0))", toString(ann.getDomainConstraints()));
}

/*
TEST(ScopRegion, SwitchStmt) {
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto compStmt = static_pointer_cast<const CompoundStmt>(
		parser.parseStatement("\
			{ \
			int<4>:i; \
			int<4>:b; \
			switch(i) { \
				case 0: \
					{ (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i-b))); } \
				case 1: \
					{ (int<4>:h = (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, ((int<4>:n+i)-1)))); }\
				default: \
					{ (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+b))); } \
				}; \
			}")
		);
	// std::cout << "Parsed Stmt: " << compStmt << std::endl;
	scop::mark(compStmt);

	EXPECT_TRUE( compStmt->hasAnnotation(scop::ScopRegion::KEY) );
	EXPECT_EQ( NT_SwitchStmt, (*compStmt)[2]->getNodeType() );

	const SwitchStmtPtr& switchStmt = static_pointer_cast<const SwitchStmt>((*compStmt)[2]);
	EXPECT_TRUE( switchStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *switchStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(4u, iterVec.size()) << iterVec;
	EXPECT_EQ(0u, iterVec.getIteratorNum()) << iterVec;
	EXPECT_EQ(3u, iterVec.getParameterNum()) << iterVec;

	EXPECT_EQ("(|v1,v5,v2|1)", toString(iterVec));
	scop::mark(compStmt);
}
*/

TEST(ScopRegion, WhileStmt) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["N"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto forStmt = analysis::normalize(builder.parseStmt(
		"for( int<4> i = 10 .. 20 : 1) { "
		"	N = v[i-b]; "
		"}", symbols)).as<ForStmtPtr>();

	VariablePtr cond = IRBuilder(mgr).variable( mgr.getLangBasic().getBool() );
	WhileStmtPtr whileStmt = builder.whileStmt(cond, forStmt);

	scop::AddressList scops = scop::mark(whileStmt);

	EXPECT_FALSE(whileStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_EQ(1u, scops.size());
	EXPECT_TRUE(forStmt->hasAnnotation(scop::ScopRegion::KEY));
}

TEST(ScopRegion, NotAScopForStmt) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["N"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto compStmt = analysis::normalize(builder.parseStmt(
		"{ "
		"	for(int<4> i = 10 .. N : 1) { "
		"		N = v[i*b]; "
		"	} "
		"} ", symbols)).as<CompoundStmtPtr>();

	EXPECT_TRUE(compStmt);

	scop::AddressList scops = scop::mark(compStmt);

	EXPECT_FALSE(compStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_EQ(0u, scops.size());
}


TEST(ScopRegion, ForStmtToIR) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto code = analysis::normalize(builder.parseStmt(
		"for(int<4> i = 10 .. 50 : 1) { "
		"	v[i+b]; "
		"} ", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not A SCoP";

	// convert back into IR
	NodePtr res = scop->toIR(mgr);
	res = analysis::normalize(res);
	EXPECT_EQ("for(int<4> v0 = 10 .. int.add(49, 1) : 1) {"
				"vector.ref.elem(v1, cast<uint<8>>(int.add(v0, v2)));"
			  "}", 
			  toString(*res)
			 );
}

TEST(ScopRegion, ForStmtToIR2) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["y"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = analysis::normalize(builder.parseStmt(
    	"{"
    	"	y = 0;"
		"	for(int<4> i = 10 .. 50 : 1) { "
		"		v[i+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	// convert back into IR
	NodePtr res = scop->toIR(mgr);
	res = analysis::normalize(res);
	EXPECT_EQ(
		"{"
			"ref.assign(v3, 0); "
			"for(int<4> v0 = 10 .. int.add(49, 1) : 1) {"
				"vector.ref.elem(v1, cast<uint<8>>(int.add(v0, v2)));"
			"};"
		"}", toString(*res));

}

TEST(ScopRegion, IfStmtSelect) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["y"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = analysis::normalize(builder.parseStmt(
		"{"
    	"	y = 0;"
		"	if( select(a,b, int.lt) == 5 ) { "
		"		v[a+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);

	EXPECT_TRUE(scop) << "Not a SCoP";

	// convert back into IR
	NodeManager mgr1;
	NodePtr res = scop->toIR(mgr1);
	EXPECT_EQ(
		"{ref.assign(v4, 0); if(bool.and(int.eq(v2, 5), bind(){rec v0.{v0=fun(int<4> v1) {return int.ge(v1, 6);}}(v3)})) {vector.ref.elem(v1, cast<uint<8>>(int.add(v2, v3)));} else {}; if(bool.and(int.ge(v2, 5), bind(){rec v0.{v0=fun(int<4> v4) {return int.eq(v4, 5);}}(v3)})) {vector.ref.elem(v1, cast<uint<8>>(int.add(v2, v3)));} else {};}", toString(*res));

	auto scop2 = polyhedral::scop::ScopRegion::toScop(res);
	EXPECT_TRUE(scop2);

	NodeManager mgr2;
	NodePtr res2 = scop2->toIR(mgr2);

	EXPECT_EQ(*res2,*res);
}

TEST(ScopRegion, IfStmtPiecewise) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["y"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = analysis::normalize(builder.parseStmt(
    	"{"
    	"	y = 0;"
		"	if( cloog.floor(a,3) == 3 ) { "
		"		v[a+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	NodeManager mgr1;
	// convert back into IR
	NodePtr res = scop->toIR(mgr1);
	EXPECT_EQ("{ref.assign(v4, 0); if(bool.and(int.ge(v2, 9), bind(){rec v0.{v0=fun(int<4> v1) {return int.le(v1, 11);}}(v2)})) {vector.ref.elem(v1, cast<uint<8>>(int.add(v2, v3)));} else {};}", toString(*res));

	auto scop2 = polyhedral::scop::ScopRegion::toScop(res);
	EXPECT_TRUE(scop2);

	NodeManager mgr2;
	NodePtr res2 = scop2->toIR(mgr2);

	EXPECT_EQ(*res2,*res);
}

TEST(ScopRegion, ForStmtToIR3) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto code = analysis::normalize(builder.parseStmt(
		"{"
		"	for( int<4> i = 1 .. cloog.floor(a,3) : 1) { "
		"		v[i+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	NodeManager mgr1;
	// convert back into IR
	NodePtr res = scop->toIR(mgr1);

	EXPECT_EQ("if(int.ge(v2, 6)) {for(int<4> v1 = 1 .. int.add(cloog.floor(int.add(cast<int<4>>(v2), cast<int<4>>(-3)), 3), 1) : 1) {vector.ref.elem(v1, cast<uint<8>>(int.add(v1, v3)));};} else {}", toString(*res));
	
	auto scop2 = polyhedral::scop::ScopRegion::toScop(res);
	EXPECT_TRUE(scop2);

	NodeManager mgr2;
	NodePtr res2 = scop2->toIR(mgr2);
	
	EXPECT_EQ(*res2,*res);
}

TEST(ScopRegion, ForStmtSelectLB) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto code = analysis::normalize(builder.parseStmt(
		"{"
		"	for( int<4> i = select(a,b, int.lt) .. select(a,b, int.gt) : 1) { "
		"		v[i+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	//auto scop = polyhedral::scop::ScopRegion::toScop(code);
	//EXPECT_TRUE(scop) << "Not a SCoP";

	//NodeManager mgr1;
	// convert back into IR
	//NodePtr res = scop->toIR(mgr1);

	//EXPECT_EQ("{"
	//			"for(int<4> v1 = v3 .. int.add(int.add(cast<int<4>>(v2), cast<int<4>>(-1)), 1) : 1) {"
	//				"array.ref.elem.1D(v4, int.add(v1, v3));"
	//			"}; "
	//			"for(int<4> v4 = v2 .. int.add(int.add(cast<int<4>>(v3), cast<int<4>>(-1)), 1) : 1) {"
	//				"array.ref.elem.1D(v4, int.add(v4, v3));"
	//			"};"
	//		   "}", toString(*res));
	
	//auto scop2 = polyhedral::scop::ScopRegion::toScop(res);
	//EXPECT_TRUE(scop2);

	//NodeManager mgr2;
	//NodePtr res2 = scop2->toIR(mgr2);
	
	//EXPECT_EQ(toString(res2), toString(res));
}

TEST(ScopRegion, Mod) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto code = analysis::normalize(builder.parseStmt(
    	"{"
		"	for(int<4> i = int.mod(a,3) ..  10 : 1) { "
		"		v[i+b]; "
		"	} "
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	// LOG(ERROR) << *scop;

	// NodeManager mgr1;
	// // convert back into IR
	// NodePtr res = scop->toIR(mgr1);

	// //LOG(ERROR) << printer::PrettyPrinter(res);

	// EXPECT_EQ("for(int<4> v1 = int.add(cast<int<4>>(int.mul(cast<int<4>>(3), "
	// 								"cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v2))), "
	// 									"cast<int<4>>(2)), 3)))), cast<int<4>>(v2)) "
	// 							".. int.add(9, 1) : 1) {"
	// 			"array.ref.elem.1D(v3, int.add(v1, v4));"
	// 		  "}", toString(*res));
	// 
	// auto scop2 = polyhedral::scop::ScopRegion::toScop(res);
	// EXPECT_TRUE(scop2);
	
	//NodeManager mgr2;
	//NodePtr res2 = scop2->toIR(mgr2);
	
	//EXPECT_EQ(toString(res2), toString(res));
	// LOG(ERROR) << *scop2;
	// EXPECT_EQ(*(*scop2)[0].getDomain().getCard(), *(*scop)[0].getDomain().getCard());
}


TEST(ScopRegion, ForStmtSelectLBTile) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto code = analysis::normalize(builder.parseStmt(
    	"{"
		"	for(int<4> i = select(a,b,int.lt) .. 100 : 5) { "
		"		v[i+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
// 	auto scop = polyhedral::scop::ScopRegion::toScop(code);
// 	EXPECT_TRUE(scop);
// 
// 	NodeManager mgr1;
// 	// convert back into IR
// 	NodePtr res = scop->toIR(mgr1);

	//EXPECT_EQ("{if(int.le(v3, v2)) {for(int<4> v1 = int.add(cast<int<4>>(int.mul(cast<int<4>>(-5), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v3))), cast<int<4>>(v2)), 5)))), cast<int<4>>(v2)) .. int.add(99, 1) : 5) {array.ref.elem.1D(v4, int.add(v1, v3));};} else {}; if(int.ge(v3, int.add(cast<int<4>>(v2), cast<int<4>>(1)))) {for(int<4> v4 = int.add(cast<int<4>>(int.mul(cast<int<4>>(-5), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(v3), cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v2)))), 5)))), cast<int<4>>(v3)) .. int.add(99, 1) : 5) {array.ref.elem.1D(v4, int.add(v4, v3));};} else {};}", toString(*res));
	
	//auto scop2 = polyhedral::scop::ScopRegion::toScop(res);
	//EXPECT_TRUE(scop2);

	//NodeManager mgr2;
	//NodePtr res2 = scop2->toIR(mgr2);
	//EXPECT_EQ(toString(res2), toString(res));
}

TEST(ScopRegion, ForStmtToIR4) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto code = analysis::normalize(builder.parseStmt(
	   "{"
	   "	ref<int<4>> y=var(0);"
		"	for(int<4> i = 10 .. 50 : 1) { "
	    "   	ref<int<4>> u = var(0);"
		"		v[i+b]; "
		"	}"
    	"}", symbols));

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	// convert back into IR
	NodePtr res = scop->toIR(mgr);
	// normalize varnames
	res = analysis::normalize(res);
	EXPECT_EQ("{ref<int<4>> v0 = ref.var(0); ref<int<4>> v0 = ref.var(0); {ref.assign(v0, ref.deref(ref.var(0))); for(int<4> v2 = 10 .. int.add(49, 1) : 1) {ref.assign(v0, ref.deref(ref.var(0))); vector.ref.elem(v1, cast<uint<8>>(int.add(v2, v3)));};};}", toString(*res));

}


