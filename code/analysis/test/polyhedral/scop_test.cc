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

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

TEST(ScopRegion, CompoundStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto compStmt = parser.parseStatement(
		"{\
			decl int<4>:b = 20; \
			(op<array.subscript.1D>(ref<array<int<4>,1>>:v, (int<4>:a+b)));\
		}"
	);
	// Mark scops in this code snippet
	scop::mark(compStmt);

	EXPECT_TRUE(compStmt->hasAnnotation(scop::ScopRegion::KEY));
}

TEST(ScopRegion, IfStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto ifStmt = static_pointer_cast<const IfStmt>( parser.parseStatement("\
		if((int<4>:c <= int<4>:d)){ \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (int<4>:a-int<4>:b))); \
		} else { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (int<4>:a+int<4>:b))); \
		}") );
	// std::cout << *ifStmt << std::endl;
	// Mark scops in this code snippet
	scop::mark(ifStmt);
	EXPECT_TRUE(ifStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annIf = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
	IterationVector iterVec = annIf.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(5), iterVec.size());

	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(4), iterVec.getParameterNum());
	{	
		std::ostringstream ss;
		ss << annIf.getIterationVector();
		EXPECT_EQ("(|v1,v2,v4,v5|1)", ss.str());
	}

	EXPECT_FALSE( annIf.getDomainConstraints().empty() );

	EXPECT_TRUE(ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annThen = *ifStmt->getThenBody()->getAnnotation(scop::ScopRegion::KEY);
	iterVec = annThen.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());

	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getParameterNum());
	{	
		std::ostringstream ss;
		ss << annThen.getIterationVector();
		EXPECT_EQ("(|v4,v5|1)", ss.str());
	}
	EXPECT_FALSE( annThen.getDomainConstraints().empty() );

	EXPECT_TRUE(ifStmt->getElseBody()->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annElse = *ifStmt->getElseBody()->getAnnotation(scop::ScopRegion::KEY);
	iterVec = annElse.getIterationVector();

	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());

	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getParameterNum());
 	{	
		std::ostringstream ss;
		ss << annElse.getIterationVector();
		EXPECT_EQ("(|v4,v5|1)", ss.str());
	}
	EXPECT_FALSE( annElse.getDomainConstraints().empty() );

}

TEST(ScopRegion, SimpleForStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. 50 : 1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_TRUE(forStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);

	EXPECT_EQ(1, ann.getDirectRegionStmts().size());
	IterationVector iterVec = ann.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size()) << iterVec;
	EXPECT_EQ(static_cast<size_t>(1), iterVec.getIteratorNum()) << iterVec;
	EXPECT_EQ(static_cast<size_t>(1), iterVec.getParameterNum()) << iterVec;

	{	
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1|v3|1)", ss.str());
	}
	{ 
		std::ostringstream ss;
		ss << ann.getDomainConstraints();
		EXPECT_EQ("((v1 + -10*1 >= 0) ^ (v1 + -50*1 < 0))", ss.str());
	}
	EXPECT_TRUE(forStmt->getBody()->hasAnnotation(scop::ScopRegion::KEY));
}

TEST(ScopRegion, ForStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. 50 : 1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i*ref<int<4>>:b))); \
			if ((i > 25)) { \
				(int<4>:h = (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, ((int<4>:n+i)-1))));\
			};\
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_FALSE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );
	IfStmtPtr ifStmt = static_pointer_cast<const IfStmt>(
		static_pointer_cast<const CompoundStmt>(forStmt->getBody())->getStatements().back());

	EXPECT_TRUE(ifStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_TRUE(ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY));

	// check the then body
	scop::ScopRegion& ann = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getParameterNum());
	
	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(|v1,v5|1)", ss.str());
	}

}

TEST(ScopRegion, ForStmt2) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = int<4>:lb .. int<4>:ub : 1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+ref<int<4>>:b))); \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(static_cast<size_t>(5), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(1), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(3), iterVec.getParameterNum());
	
	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1|v5,v2,v3|1)", ss.str());
	}
	{
		std::ostringstream ss;
		ss << ann.getDomainConstraints();
		EXPECT_EQ("((v1 + -v2 >= 0) ^ (v1 + -v3 < 0))", ss.str());
	}
}

TEST(ScopRegion, ForStmt3) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = int<4>:lb .. int<4>:ub : 5) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+ref<int<4>>:b))); \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(static_cast<size_t>(6), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(3), iterVec.getParameterNum());
	
	EXPECT_EQ(Element::ITER, iterVec[1].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[1]).isExistential());
	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1,v6|v5,v2,v3|1)", ss.str());
	}
	{
		std::ostringstream ss;
		ss << ann.getDomainConstraints();
		EXPECT_EQ("(((v1 + -v2 >= 0) ^ (v1 + -v3 < 0)) ^ (v1 + -5*v6 + -v2 == 0))", ss.str());
	}
}

TEST(ScopRegion, ForStmt4) {
	using namespace insieme::core::arithmetic;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = (op<cloog.floor>(5, 2)) .. 20 : 5) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+ref<int<4>>:b))); \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(static_cast<size_t>(6), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(4), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(1), iterVec.getParameterNum());
	
	EXPECT_EQ(Element::ITER, iterVec[1].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[1]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[2].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[2]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[3].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[3]).isExistential());

	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1,v4,v5,v6|v3|1)", ss.str());
	}
	{
		std::ostringstream ss;
		ss << ann.getDomainConstraints();
		EXPECT_EQ("((((((-2*v4 + -v5 + 5*1 == 0) ^ (v5 + -2*1 < 0)) ^ (v5 >= 0)) ^ (v1 + -v4 >= 0)) "
				  "^ (v1 + -20*1 < 0)) ^ (v1 + -v4 + -5*v6 == 0))", ss.str());
	}
	
	// we solve the system and we make sure that the domain of the if statement contains exactly 4 elements 
	Piecewise pw = cardinality(mgr,  ann.getDomainConstraints());
	EXPECT_TRUE(pw.isFormula());
	EXPECT_EQ(Formula(4), pw.toFormula());
}

TEST(ScopRegion, ForStmt5) {
	using namespace insieme::core::arithmetic;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = (op<cloog.ceil>(int<4>:lb, 3)) .. int<4>:ub : 5) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+ref<int<4>>:b))); \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_TRUE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector();

	EXPECT_EQ(static_cast<size_t>(8), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(4), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(3), iterVec.getParameterNum());
	
	EXPECT_EQ(Element::ITER, iterVec[1].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[1]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[2].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[2]).isExistential());
	EXPECT_EQ(Element::ITER, iterVec[3].getType());
	EXPECT_TRUE(static_cast<const Iterator&>(iterVec[3]).isExistential());

	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1,v6,v7,v8|v5,v2,v3|1)", ss.str());
	}
	{
		std::ostringstream ss;
		ss << ann.getDomainConstraints();
		EXPECT_EQ("((((((-3*v6 + v7 + v2 == 0) ^ (v7 + -3*1 < 0)) ^ (v7 >= 0)) ^ "
				"(v1 + -v6 >= 0)) ^ (v1 + -v3 < 0)) ^ (v1 + -v6 + -5*v8 == 0))", ss.str());
	}
	
	// we solve the system and we make sure that the domain of the if statement contains exactly 4 elements 
	// Piecewise pw = cardinality(mgr,  ann.getDomainConstraints());
	// std::cout << pw << std::endl;
	// EXPECT_TRUE(pw.isFormula());
	// EXPECT_EQ(Formula(4), pw.toFormula());
}

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
	std::cout << "Parsed Stmt: " << compStmt << std::endl;
	scop::mark(compStmt);

	EXPECT_TRUE( compStmt->hasAnnotation(scop::ScopRegion::KEY) );
	EXPECT_EQ( NT_SwitchStmt, (*compStmt)[2]->getNodeType() );
	const SwitchStmtPtr& switchStmt = static_pointer_cast<const SwitchStmt>((*compStmt)[2]);
	
	EXPECT_TRUE( switchStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *switchStmt->getAnnotation(scop::ScopRegion::KEY);
	const IterationVector& iterVec = ann.getIterationVector(); 

	EXPECT_EQ(static_cast<size_t>(4), iterVec.size()) << iterVec;
	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum()) << iterVec;
	EXPECT_EQ(static_cast<size_t>(3), iterVec.getParameterNum()) << iterVec;
	
	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(|v1,v5,v2|1)", ss.str());
	}

	scop::mark(compStmt);
}

TEST(ScopRegion, WhileStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto compStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. int<4>:N : 1) { \
			(N = (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i-int<4>:b)))); \
		}") );

	VariablePtr cond = IRBuilder(mgr).variable( mgr.getLangBasic().getBool() );
	WhileStmtPtr whileStmt = IRBuilder(mgr).whileStmt(cond, compStmt);

	// std::cout << *forStmt << std::endl;
	scop::AddressList&& scops = scop::mark(whileStmt);

	EXPECT_FALSE(whileStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_EQ(1, scops.size());
	EXPECT_TRUE(compStmt->hasAnnotation(scop::ScopRegion::KEY));
}

TEST(ScopRegion, NotAScopForStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto compStmt = static_pointer_cast<const CompoundStmt>( parser.parseStatement("{\
		for(decl int<4>:i = 10 .. int<4>:N : 1) { \
			(N = (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i*int<4>:b)))); \
		}; \
	}") );
	// std::cout << *forStmt << std::endl;
	scop::AddressList&& scops = scop::mark(compStmt);

	EXPECT_FALSE(compStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_EQ(0, scops.size());
}


TEST(ScopRegion, ForStmtToIR) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"for(decl int<4>:i = 10 .. 50 : 1) { "
		"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"}"
    );

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	// convert back into IR
	NodePtr res = scop->toIR(mgr);
	EXPECT_EQ("for(int<4> v4 = 10 .. int.add(49, 1) : 1) {array.ref.elem.1D(v2, int.add(v4, v3));}", toString(*res));

}

TEST(ScopRegion, ForStmtToIR2) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

	// add some additional statements
    auto code = parser.parseStatement(
    	"{"
    	"	(ref<int<4>>:y = 0);"
		"	for(decl int<4>:i = 10 .. 50 : 1) { "
		"		(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
		"	};"
    	"}"
    );

    EXPECT_TRUE(code);

	// convert for-stmt into a SCoP
	auto scop = polyhedral::scop::ScopRegion::toScop(code);
	EXPECT_TRUE(scop) << "Not a SCoP";

	// convert back into IR
	NodePtr res = scop->toIR(mgr);
	EXPECT_EQ("{ref.assign(v1, 0); for(int<4> v5 = 10 .. int.add(49, 1) : 1) {array.ref.elem.1D(v3, int.add(v5, v4));};}", toString(*res));

}


//TEST(ScopRegion, ForStmtToIR3) {
//	Logger::setLevel(DEBUG, 1);
//
//	NodeManager mgr;
//	parse::IRParser parser(mgr);
//
//	// add some local declarations
//    auto code = parser.parseStatement(
//    	"{"
//    	"	decl ref<int<4>>:y = (op<ref.var>(0));"
//		"	for(decl int<4>:i = 10 .. 50 : 1) { "
//    	"   	decl ref<int<4>>:y = (op<ref.var>(0));"
//		"		(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); "
//		"	};"
//    	"}"
//    );
//
//    EXPECT_TRUE(code);
//
//	// convert for-stmt into a SCoP
//	auto scop = polyhedral::scop::ScopRegion::toScop(code);
//	EXPECT_TRUE(scop) << "Not a SCoP";
//
//	std::cout << "SCoP: \n" << *scop << "\n";
//
//	// convert back into IR
//	NodePtr res = scop->toIR(mgr);
//	std::cout << printer::PrettyPrinter(res) << std::endl;
//
//	EXPECT_EQ("{ref.assign(v1, 0); for(int<4> v5 = 10 .. int.add(49, 1) : 1) {array.ref.elem.1D(v3, int.add(v5, v4));};}", toString(*res));
//
//}


