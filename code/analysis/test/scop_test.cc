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

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/statements.h"

#include "insieme/core/parser/ir_parse.h"

using namespace insieme::core;
using namespace insieme::analysis;

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
	std::cout << *ifStmt << std::endl;
	// Mark scops in this code snippet
	scop::mark(ifStmt);
	EXPECT_TRUE(ifStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annIf = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
	poly::IterationVector iterVec = annIf.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(5), iterVec.size());

	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(4), iterVec.getParameterNum());
	{	
		std::ostringstream ss;
		ss << annIf.getIterationVector();
		EXPECT_EQ("(|v4,v5,v7,v8|1)", ss.str());
	}

	EXPECT_FALSE( annIf.getDomainConstraints().isEmpty() );

	EXPECT_TRUE(ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annThen = *ifStmt->getThenBody()->getAnnotation(scop::ScopRegion::KEY);
	iterVec = annThen.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());

	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getParameterNum());
	{	
		std::ostringstream ss;
		ss << annThen.getIterationVector();
		EXPECT_EQ("(|v7,v8|1)", ss.str());
	}
	EXPECT_FALSE( annThen.getDomainConstraints().isEmpty() );

	EXPECT_TRUE(ifStmt->getElseBody()->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& annElse = *ifStmt->getElseBody()->getAnnotation(scop::ScopRegion::KEY);
	iterVec = annElse.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());

	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getParameterNum());
 	{	
		std::ostringstream ss;
		ss << annElse.getIterationVector();
		EXPECT_EQ("(|v7,v8|1)", ss.str());
	}
	EXPECT_FALSE( annElse.getDomainConstraints().isEmpty() );

}

TEST(ScopRegion, SimpleForStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. 50 : -1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b))); \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_TRUE(forStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);

	EXPECT_EQ(1, ann.getDirectRegionStmts().size());
	poly::IterationVector iterVec = ann.getIterationVector();
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(1), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(1), iterVec.getParameterNum());

	{	
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v9|v11|1)", ss.str());
	}
	{ 
		std::ostringstream ss;
		ss << ann.getDomainConstraints();
		EXPECT_EQ("((1*v9 + -10*1 >= 0) AND (1*v9 + -50*1 < 0))", ss.str());
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
	std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	EXPECT_FALSE( forStmt->hasAnnotation(scop::ScopRegion::KEY) );
	IfStmtPtr ifStmt = static_pointer_cast<const IfStmt>(
		static_pointer_cast<const CompoundStmt>(forStmt->getBody())->getStatements().back());

	EXPECT_TRUE(ifStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_TRUE(ifStmt->getThenBody()->hasAnnotation(scop::ScopRegion::KEY));

	// check the then body
	scop::ScopRegion& ann = *ifStmt->getAnnotation(scop::ScopRegion::KEY);
	const poly::IterationVector& iterVec = ann.getIterationVector(); 

	EXPECT_EQ(static_cast<size_t>(3), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(2), iterVec.getParameterNum());
	
	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(|v12,v16|1)", ss.str());
	}

}

TEST(ScopRegion, SwitchStmt) {
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto compStmt = static_pointer_cast<const CompoundStmt>( 
		parser.parseStatement("\
			{ \
			int<4>:i; \
			ref<array<int<4>,1>>:v; \
			ref<int<4>>:b; \
			switch(i) { \
				case 0: \
					{ (op<array.ref.elem.1D>(v, (i-b))); } \
				case 1: \
					{ (int<4>:h = (op<array.ref.elem.1D>(v, ((int<4>:n+i)-1)))); }\
				default: \
					{ (op<array.ref.elem.1D>(v, (i+b))); } \
				}; \
			}") 
		);
	std::cout << "Parsed Stmt: " << compStmt << std::endl;
	scop::mark(compStmt);

	EXPECT_TRUE( compStmt->hasAnnotation(scop::ScopRegion::KEY) );
	EXPECT_EQ( NT_SwitchStmt, (*compStmt)[3]->getNodeType() );
	const SwitchStmtPtr& switchStmt = static_pointer_cast<const SwitchStmt>((*compStmt)[3]);
	
	EXPECT_TRUE( switchStmt->hasAnnotation(scop::ScopRegion::KEY) );

	// check the then body
	scop::ScopRegion& ann = *switchStmt->getAnnotation(scop::ScopRegion::KEY);
	const poly::IterationVector& iterVec = ann.getIterationVector(); 

	EXPECT_EQ(static_cast<size_t>(4), iterVec.size());
	EXPECT_EQ(static_cast<size_t>(0), iterVec.getIteratorNum());
	EXPECT_EQ(static_cast<size_t>(3), iterVec.getParameterNum());
	
	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(|v17,v21,v19|1)", ss.str());
	}

	scop::mark(compStmt);
}

TEST(ScopRegion, NotAScopForStmt) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto compStmt = static_pointer_cast<const CompoundStmt>( parser.parseStatement("{\
		ref<int<4>>:N; \
		for(decl int<4>:i = 10 .. N : -1) { \
			(N = (op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+int<4>:b)))); \
		}; \
	}") );
	// std::cout << *forStmt << std::endl;
	scop::AddressList&& scops = scop::mark(compStmt);

	EXPECT_FALSE(compStmt->hasAnnotation(scop::ScopRegion::KEY));
	EXPECT_EQ(0, scops.size());
}




