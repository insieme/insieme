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

#include "program.h"
#include "iostream"
#include "ast_visitor.h"

using namespace insieme::core;

class SimpleVisitor : public ASTVisitor<SimpleVisitor> {

public:
	int countGenericTypes;
	int countArrayTypes;
	int countExpressions;
	int countRefTypes;

	SimpleVisitor() : countGenericTypes(0), countArrayTypes(0), countExpressions(0), countRefTypes(0) {};

public:
	void visitGenericType(const GenericTypePtr& cur) {
		countGenericTypes++;
	}

	void visitExpression(const ExpressionPtr& cur) {
		countExpressions++;
	}

	void visitArrayType(const ArrayTypePtr& cur) {
		countArrayTypes++;
	}

	void visitRefType(const RefTypePtr& cur) {
		countRefTypes++;

		// forward processing
		visitGenericType(cur);
	}
};

TEST(ASTVisitor, DispatcherTest) {

	NodeManager manager;
	SimpleVisitor visitor;

	ProgramPtr program = Program::create();

	EXPECT_EQ ( 0, visitor.countArrayTypes );
	EXPECT_EQ ( 0, visitor.countExpressions );
	EXPECT_EQ ( 0, visitor.countGenericTypes );
	EXPECT_EQ ( 0, visitor.countRefTypes );

	visitor.visit(program);

	EXPECT_EQ ( 0, visitor.countArrayTypes );
	EXPECT_EQ ( 0, visitor.countExpressions );
	EXPECT_EQ ( 0, visitor.countGenericTypes );
	EXPECT_EQ ( 0, visitor.countRefTypes );


	GenericTypePtr type = GenericType::get(manager, "int");
	visitor.visit(type);

	EXPECT_EQ ( 0, visitor.countArrayTypes );
	EXPECT_EQ ( 0, visitor.countExpressions );
	EXPECT_EQ ( 1, visitor.countGenericTypes );
	EXPECT_EQ ( 0, visitor.countRefTypes );

	IntTypePtr intType = IntType::get(manager);
	visitor.visit(intType);

	EXPECT_EQ ( 0, visitor.countArrayTypes );
	EXPECT_EQ ( 0, visitor.countExpressions );
	EXPECT_EQ ( 2, visitor.countGenericTypes );
	EXPECT_EQ ( 0, visitor.countRefTypes );

	LiteralPtr literal = Literal::get(manager, "3", type);
	visitor.visit(literal);

	EXPECT_EQ ( 0, visitor.countArrayTypes );
	EXPECT_EQ ( 1, visitor.countExpressions );
	EXPECT_EQ ( 2, visitor.countGenericTypes );
	EXPECT_EQ ( 0, visitor.countRefTypes );

	ArrayTypePtr arrayType = ArrayType::get(manager, type);
	visitor.visit(arrayType);

	EXPECT_EQ ( 1, visitor.countArrayTypes );
	EXPECT_EQ ( 1, visitor.countExpressions );
	EXPECT_EQ ( 2, visitor.countGenericTypes );
	EXPECT_EQ ( 0, visitor.countRefTypes );

	RefTypePtr refType = RefType::get(manager, type);
	visitor.visit(refType);

	EXPECT_EQ ( 1, visitor.countArrayTypes );
	EXPECT_EQ ( 1, visitor.countExpressions );
	EXPECT_EQ ( 3, visitor.countGenericTypes );
	EXPECT_EQ ( 1, visitor.countRefTypes );
}


class CountingVisitor : public ASTVisitor<CountingVisitor, int> {
public:

	int counter;

	CountingVisitor() : counter(0) {};

	int visitNode(const NodePtr& node) {
		std::cout << *node << std::endl;
		return ++counter;
	};

	void reset() {
		counter = 0;
	}

};


TEST(ASTVisitor, RecursiveVisitorTest) {

	// TODO: run recursive visitor test

	NodeManager manager;
	CountingVisitor visitor;
	RecursiveProgramVisitor<int, CountingVisitor> recVisitor(visitor);

	ProgramPtr program = Program::create();

	visitor.reset();
	visitor.visit(program);
	EXPECT_EQ ( 1, visitor.counter );

	visitor.reset();
	recVisitor.visit(program);
	EXPECT_EQ ( 1, visitor.counter );


	GenericTypePtr type = GenericType::get(manager, "int");
	visitor.visit(type);

	visitor.reset();
	visitor.visit(program);
	EXPECT_EQ ( 1, visitor.counter );

	visitor.reset();
	recVisitor.visit(program);
	EXPECT_EQ ( 1, visitor.counter );

	GenericTypePtr type2 = GenericType::get(manager, "int", toVector<TypePtr>(type, type), toVector<IntTypeParam>('p'), type);

	visitor.reset();
	visitor.visit(type2);
	EXPECT_EQ ( 1, visitor.counter );

	visitor.reset();
	recVisitor.visit(type2);
	EXPECT_EQ ( 4, visitor.counter );

	IfStmtPtr ifStmt = IfStmt::get(manager,
		Literal::get(manager, "12", type),
		Literal::get(manager, "14", type),
		CompoundStmt::get(manager)
	);

	visitor.reset();
	visitor.visit(ifStmt);
	EXPECT_EQ ( 1, visitor.counter );

	visitor.reset();
	recVisitor.visit(ifStmt);
	EXPECT_EQ ( 6, visitor.counter );

}
