/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <gtest/gtest.h>

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/array.h"

using namespace insieme::core;

class SimpleVisitor : public IRVisitor<void> {
  public:
	int countGenericTypes;
	int countTypeVariables;
	int countExpressions;

	SimpleVisitor() : IRVisitor<void>(true), countGenericTypes(0), countTypeVariables(0), countExpressions(0){};

  public:
	void visitGenericType(const GenericTypePtr& cur) override {
		countGenericTypes++;
	}

	void visitExpression(const ExpressionPtr& cur) override {
		countExpressions++;
	}

	void visitTypeVariable(const TypeVariablePtr& cur) override {
		countTypeVariables++;
	}
};

TEST(IRVisitor, DispatcherTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	SimpleVisitor visitor;

	ProgramPtr program = Program::get(manager);

	EXPECT_EQ(0, visitor.countGenericTypes);
	EXPECT_EQ(0, visitor.countTypeVariables);
	EXPECT_EQ(0, visitor.countExpressions);

	visitor.visit(program);

	EXPECT_EQ(0, visitor.countGenericTypes);
	EXPECT_EQ(0, visitor.countTypeVariables);
	EXPECT_EQ(0, visitor.countExpressions);

	GenericTypePtr type = GenericType::get(manager, "int");
	visitor.visit(type);

	EXPECT_EQ(1, visitor.countGenericTypes);
	EXPECT_EQ(0, visitor.countTypeVariables);
	EXPECT_EQ(0, visitor.countExpressions);

	auto intType = manager.getLangBasic().getInt16();
	visitor.visit(intType);

	EXPECT_EQ(2, visitor.countGenericTypes);
	EXPECT_EQ(0, visitor.countTypeVariables);
	EXPECT_EQ(0, visitor.countExpressions);

	LiteralPtr literal = Literal::get(manager, type, "3");
	visitor.visit(literal);

	EXPECT_EQ(2, visitor.countGenericTypes);
	EXPECT_EQ(0, visitor.countTypeVariables);
	EXPECT_EQ(1, visitor.countExpressions);

	TypePtr typeVar = builder.typeVariable("a");
	visitor.visit(typeVar);

	EXPECT_EQ(2, visitor.countGenericTypes);
	EXPECT_EQ(1, visitor.countTypeVariables);
	EXPECT_EQ(1, visitor.countExpressions);
}


class CountingVisitor : public IRVisitor<int> {
  public:
	int counter;

	CountingVisitor(bool countTypes = true) : IRVisitor<int>(countTypes), counter(0){};

	int visitNode(const NodePtr& node) override {
		// std::cout << *node << std::endl;
		return ++counter;
	};

	void reset() {
		counter = 0;
	}
};

class CountingAddressVisitor : public IRVisitor<int, Address> {
  public:
	int counter;

	CountingAddressVisitor(bool countTypes = true) : IRVisitor<int, Address>(countTypes), counter(0){};

	int visitNode(const NodeAddress& address) override {
		return ++counter;
	};

	void reset() {
		counter = 0;
	}
};


TEST(IRVisitor, RecursiveVisitorTest) {
	// TODO: run recursive visitor test

	NodeManager manager;
	IRBuilder builder(manager);

	CountingVisitor visitor(true);
	auto recVisitor = makeDepthFirstVisitor(visitor);

	ProgramPtr program = Program::get(manager);

	visitor.reset();
	visitor.visit(program);
	EXPECT_EQ(1, visitor.counter);

	visitor.reset();
	recVisitor.visit(program);
	EXPECT_EQ(1, visitor.counter);


	GenericTypePtr type = GenericType::get(manager, "int");
	visitor.visit(type);

	visitor.reset();
	visitor.visit(program);
	EXPECT_EQ(1, visitor.counter);

	visitor.reset();
	recVisitor.visit(program);
	EXPECT_EQ(1, visitor.counter);

	GenericTypePtr type2 = builder.genericType("int", toVector<TypePtr>(type, type));

	visitor.reset();
	visitor.visit(type2);
	EXPECT_EQ(1, visitor.counter);

	visitor.reset();
	recVisitor.visit(type2);
	EXPECT_EQ(12, visitor.counter);

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	visitor.reset();
	visitor.visit(ifStmt);
	EXPECT_EQ(1, visitor.counter);

	visitor.reset();
	recVisitor.visit(ifStmt);
	EXPECT_EQ(15, visitor.counter);


	// ------ test for addresses ----
	CountingAddressVisitor adrVisitor(true);
	auto recAdrVisitor = makeDepthFirstVisitor(adrVisitor);

	adrVisitor.reset();
	adrVisitor.visit(NodeAddress(ifStmt));
	EXPECT_EQ(1, adrVisitor.counter);

	adrVisitor.reset();
	recAdrVisitor.visit(NodeAddress(ifStmt));
	EXPECT_EQ(15, adrVisitor.counter);


	// test without types
	CountingVisitor noTypePtrVisitor(false);
	auto recNoTypeVisitor = makeDepthFirstVisitor(noTypePtrVisitor);

	noTypePtrVisitor.reset();
	noTypePtrVisitor.visit(ifStmt);
	EXPECT_EQ(1, noTypePtrVisitor.counter);

	noTypePtrVisitor.reset();
	recNoTypeVisitor.visit(ifStmt);
	EXPECT_EQ(7, noTypePtrVisitor.counter);

	CountingAddressVisitor noTypeAdrVisitor(false);
	auto recNoTypeAdrVisitor = makeDepthFirstVisitor(noTypeAdrVisitor);

	noTypeAdrVisitor.reset();
	noTypeAdrVisitor.visit(NodeAddress(ifStmt));
	EXPECT_EQ(1, noTypeAdrVisitor.counter);

	noTypeAdrVisitor.reset();
	recNoTypeAdrVisitor.visit(NodeAddress(ifStmt));
	EXPECT_EQ(7, noTypeAdrVisitor.counter);
}

TEST(IRVisitor, BreadthFirstIRVisitorTest) {
	// TODO: run recursive visitor test

	NodeManager manager;
	CountingVisitor visitor;

	// create a Test CASE
	GenericTypePtr typeD = GenericType::get(manager, "D");
	GenericTypePtr typeE = GenericType::get(manager, "E");
	GenericTypePtr typeF = GenericType::get(manager, "F");

	GenericTypePtr typeB = GenericType::get(manager, "B", toVector<TypePtr>(typeD, typeE));
	GenericTypePtr typeC = GenericType::get(manager, "C", toVector<TypePtr>(typeF));

	GenericTypePtr typeA = GenericType::get(manager, "A", toVector<TypePtr>(typeB, typeC));


	// create a resulting list
	vector<NodePtr> res;

	// create a visitor collecting all nodes
	auto collector = makeLambdaVisitor([&res](const NodePtr& cur) { res.push_back(cur); }, true);

	auto breadthVisitor = makeBreadthFirstVisitor(collector);

	breadthVisitor.visit(typeA);
	vector<NodePtr> expected;
	expected.push_back(typeA);
	expected.push_back(typeA->getName());
	expected.push_back(typeA->getParents());
	expected.push_back(typeA->getTypeParameter());
	expected.push_back(typeB);
	expected.push_back(typeC);
	expected.push_back(typeB->getName());
	expected.push_back(typeB->getParents());
	expected.push_back(typeB->getTypeParameter());
	expected.push_back(typeC->getName());
	expected.push_back(typeC->getParents());
	expected.push_back(typeC->getTypeParameter());
	expected.push_back(typeD);
	expected.push_back(typeE);
	expected.push_back(typeF);
	expected.push_back(typeD->getName());
	expected.push_back(typeD->getParents());
	expected.push_back(typeD->getTypeParameter());
	expected.push_back(typeE->getName());
	expected.push_back(typeE->getParents());
	expected.push_back(typeE->getTypeParameter());
	expected.push_back(typeF->getName());
	expected.push_back(typeF->getParents());
	expected.push_back(typeF->getTypeParameter());

	EXPECT_EQ(toString(expected), toString(res));
	EXPECT_TRUE(equals(expected, res));

	res.clear();
	EXPECT_TRUE(equals(vector<NodePtr>(), res));
	visitBreadthFirst(typeA, collector);
	EXPECT_TRUE(equals(expected, res));

	res.clear();
	EXPECT_TRUE(equals(vector<NodePtr>(), res));
	visitBreadthFirst(typeA, [&res](const NodePtr& cur) { res.push_back(cur); }, true);
	EXPECT_TRUE(equals(expected, res));
}


TEST(IRVisitor, VisitOnceIRVisitorTest) {
	NodeManager manager;


	// build a simple type sharing nodes
	TypePtr shared = GenericType::get(manager, "shared");
	NodePtr type = TupleType::get(manager, toVector(shared, shared));

	// create a resulting list
	vector<NodePtr> res;

	// create a visitor collecting all nodes
	auto collector = makeLambdaVisitor([&res](const TypePtr& cur) { res.push_back(cur); }, true);

	// visit all recursively
	res.clear();
	auto recursive = makeDepthFirstVisitor(collector);
	recursive.visit(type);

	EXPECT_EQ(toVector<NodePtr>(type, shared, shared), res);

	// visit all, only once
	res.clear();
	auto prefix = makeDepthFirstOnceVisitor(collector);
	prefix.visit(type);
	EXPECT_TRUE(equals(toVector<NodePtr>(type, shared), res));

	res.clear();
	auto postfix = makeDepthFirstOnceVisitor(collector, false);
	postfix.visit(type);

	EXPECT_TRUE(equals(toVector<NodePtr>(shared, type), res));
}

TEST(IRVisitor, UtilitiesTest) {
	NodeManager manager;


	// build a simple type sharing nodes
	TypePtr shared = GenericType::get(manager, "shared");
	NodePtr type = TupleType::get(manager, toVector(shared, shared));

	// create a resulting list
	vector<NodePtr> res;

	// create a visitor collecting all nodes
	auto fun = [&res](const TypePtr& cur) { res.push_back(cur); };

	auto collector = makeLambdaVisitor(fun, true);

	// visit all recursively
	res.clear();
	visitDepthFirst(type, collector);
	EXPECT_EQ(toVector<NodePtr>(type, shared, shared), res);

	// visit all, only once
	res.clear();
	visitDepthFirstOnce(type, collector);
	EXPECT_EQ(toVector<NodePtr>(type, shared), res);

	res.clear();
	visitDepthFirstOnce(type, collector, false);
	EXPECT_EQ(toVector<NodePtr>(shared, type), res);


	// visit all recursively
	res.clear();
	visitDepthFirst(type, fun, true, false);
	EXPECT_TRUE(equals(toVector<NodePtr>(), res));

	res.clear();
	visitDepthFirst(type, fun, true, true);
	EXPECT_TRUE(equals(toVector<NodePtr>(type, shared, shared), res));

	res.clear();
	visitDepthFirst(type, fun, false, true);
	EXPECT_TRUE(equals(toVector<NodePtr>(shared, shared, type), res));

	// visit all, only once
	res.clear();
	visitDepthFirstOnce(type, fun, true, true);
	EXPECT_TRUE(equals(toVector<NodePtr>(type, shared), res));

	res.clear();
	visitDepthFirstOnce(type, fun, true, true);
	EXPECT_TRUE(equals(toVector<NodePtr>(type, shared), res));

	res.clear();
	visitDepthFirstOnce(type, fun, false, true);
	EXPECT_TRUE(equals(toVector<NodePtr>(shared, type), res));
}

template <template <class Target> class Ptr>
class InterruptingVisitor : public IRVisitor<bool, Ptr> {
  public:
	int counter;
	int limit;

	InterruptingVisitor(int limit) : IRVisitor<bool, Ptr>(true), counter(0), limit(limit){};

	bool visitType(const Ptr<const Type>& node) {
		return !(++counter < limit);
	}

	bool visitStatement(const Ptr<const Statement>& node) {
		return !(++counter < limit);
	}

	bool visitNode(const Ptr<const Node>& node) {
		return false;
	};

	void reset() {
		counter = 0;
	}
};

TEST(IRVisitor, RecursiveInterruptibleVisitorTest) {
	// TODO: run recursive visitor test

	NodeManager manager;
	IRBuilder builder(manager);
	InterruptingVisitor<Pointer> limit3(3);
	InterruptingVisitor<Pointer> limit10(10);

	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	limit3.reset();
	EXPECT_TRUE(visitDepthFirstInterruptible(ifStmt, limit3));
	EXPECT_EQ(3, limit3.counter);

	limit10.reset();
	EXPECT_FALSE(visitDepthFirstInterruptible(ifStmt, limit10));
	EXPECT_EQ(7, limit10.counter);

	// ------ test for addresses ----
	InterruptingVisitor<Address> limitA3(3);
	InterruptingVisitor<Address> limitA10(10);

	limitA3.reset();
	EXPECT_TRUE(visitDepthFirstInterruptible(NodeAddress(ifStmt), limitA3));
	EXPECT_EQ(3, limitA3.counter);

	limitA10.reset();
	visitDepthFirstInterruptible(ifStmt, limit10);
	EXPECT_FALSE(visitDepthFirstInterruptible(NodeAddress(ifStmt), limitA10));
	EXPECT_EQ(7, limitA10.counter);

	//---test breadth-first-visitor pointers
	limit3.reset();
	visitBreadthFirstInterruptible(ifStmt, limit3);
	EXPECT_EQ(3, limit3.counter);

	limit10.reset();
	visitBreadthFirstInterruptible(ifStmt, limit10);
	EXPECT_EQ(7, limit10.counter);

	//---test breadth-first-visitor addresses
	limitA3.reset();
	visitBreadthFirstInterruptible(ifStmt, limit3);
	visitBreadthFirstInterruptible(NodeAddress(ifStmt), limitA3);
	EXPECT_EQ(3, limitA3.counter);

	limitA10.reset();
	visitBreadthFirstInterruptible(ifStmt, limit10);
	visitBreadthFirstInterruptible(NodeAddress(ifStmt), limitA10);
	EXPECT_EQ(7, limitA10.counter);
}


TEST(IRVisitor, VisitOnceInterruptibleVisitorTest) {
	// TODO: run recursive visitor test

	NodeManager manager;
	IRBuilder builder(manager);
	InterruptingVisitor<Pointer> limit3(3);
	InterruptingVisitor<Pointer> limit10(10);

	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	limit3.reset();
	EXPECT_TRUE(visitDepthFirstOnceInterruptible(ifStmt, limit3));
	EXPECT_EQ(3, limit3.counter);

	limit10.reset();
	EXPECT_FALSE(visitDepthFirstOnceInterruptible(ifStmt, limit10));
	EXPECT_EQ(6, limit10.counter);

	// check number of nodes when visiting all nodes
	limit10.reset();
	visitDepthFirstOnce(ifStmt, limit10);
	EXPECT_EQ(6, limit10.counter);

	// ------ test for addresses ----
	InterruptingVisitor<Address> limitA3(3);
	InterruptingVisitor<Address> limitA10(10);

	limitA3.reset();
	EXPECT_TRUE(visitDepthFirstOnceInterruptible(NodeAddress(ifStmt), limitA3));
	EXPECT_EQ(3, limitA3.counter);

	limitA10.reset();
	EXPECT_FALSE(visitDepthFirstOnceInterruptible(NodeAddress(ifStmt), limitA10));
	EXPECT_EQ(6, limitA10.counter);

	// check number of nodes when visiting all nodes
	limitA10.reset();
	visitDepthFirstOnce(NodeAddress(ifStmt), limitA10);
	EXPECT_EQ(6, limitA10.counter);
}


class PruningVisitor : public IRVisitor<bool, Address> {
  public:
	int counter;
	int depthLimit;

	PruningVisitor(int depthLimit) : IRVisitor<bool, Address>(true), counter(0), depthLimit(depthLimit){};

	bool visitNode(const NodeAddress& node) {
		counter++;
		return (node.getDepth() >= (std::size_t)depthLimit);
	};

	void reset() {
		counter = 0;
	}
};

TEST(IRVisitor, RecursivePrunableVisitorTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	// ------ test for addresses ----
	PruningVisitor limitA(1);
	PruningVisitor limitB(2);

	limitA.reset();
	visitDepthFirstPrunable(NodeAddress(ifStmt), limitA);
	EXPECT_EQ(1, limitA.counter);

	limitB.reset();
	visitDepthFirstPrunable(NodeAddress(ifStmt), limitB);
	EXPECT_EQ(4, limitB.counter);
}


TEST(IRVisitor, VisitOncePrunableVisitorTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	// ------ test for addresses ----
	PruningVisitor limitA(1);
	PruningVisitor limitB(2);

	limitA.reset();
	visitDepthFirstOncePrunable(NodeAddress(ifStmt), limitA);
	EXPECT_EQ(1, limitA.counter);

	limitB.reset();
	visitDepthFirstOncePrunable(NodeAddress(ifStmt), limitB);
	EXPECT_EQ(4, limitB.counter);

	// check number of nodes when visiting all nodes
	limitB.reset();
	visitDepthFirstOnce(NodeAddress(ifStmt), limitB);
	EXPECT_EQ(11, limitB.counter);
}


TEST(IRVisitor, SingleTypeLambdaVisitor) {
	NodeManager manager;
	IRBuilder builder(manager);

	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	// ------ test for addresses ----

	{
		int counter = 0;
		auto visitor = makeLambdaVisitor([&counter](const LiteralPtr& cur) { counter++; });
		visitDepthFirst(ifStmt, visitor);
		EXPECT_EQ(counter, 2);
	}

	{
		int counter = 0;
		auto visitor = makeLambdaVisitor([&counter](const CompoundStmtPtr& cur) { counter++; });
		visitDepthFirst(ifStmt, visitor);
		EXPECT_EQ(counter, 2);
	}
}


bool filterLiteral(const LiteralPtr& cur) {
	return cur->getValue()->getValue() == "14";
}

TEST(IRVisitor, FilteredLambdaVisitor) {
	NodeManager manager;
	IRBuilder builder(manager);

	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	// ------ test for addresses ----

	auto filter = [](const LiteralPtr& cur) { return cur->getValue()->getValue() == "12"; };

	{
		int counter = 0;
		auto visitor = makeLambdaVisitor(filter, [&counter](const LiteralPtr& cur) { counter++; });
		visitDepthFirst(ifStmt, visitor);
		EXPECT_EQ(counter, 1);
	}

	{
		// without filter
		int counter = 0;
		auto visitor = makeLambdaVisitor([&counter](const LiteralPtr& cur) { counter++; });
		visitDepthFirst(ifStmt, visitor);
		EXPECT_EQ(counter, 2);
	}

	{
		// with reject-all filter
		int counter = 0;
		auto visitor = makeLambdaVisitor(RejectAll<const LiteralPtr&>(), [&counter](const LiteralPtr& cur) { counter++; });
		visitDepthFirst(ifStmt, visitor);
		EXPECT_EQ(counter, 0);
	}
}


TEST(IRVisitor, ParameterTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	GenericTypePtr type = GenericType::get(manager, "int");

	IfStmtPtr ifStmt = builder.ifStmt(Literal::get(manager, type, "12"), Literal::get(manager, type, "14"), CompoundStmt::get(manager));

	auto visitor = makeLambdaVisitor([](const NodePtr& cur, int& a, int& b) {
		a++;
		b--;
	}, true);


	int n = 0;
	int m = 1;
	visitor.visit(ifStmt, n, m);
	EXPECT_EQ(1, n);
	EXPECT_EQ(0, m);

	n = 0;
	m = 0;
	auto recVisitor = makeDepthFirstVisitor(visitor);
	recVisitor.visit(ifStmt, n, m);
	EXPECT_EQ(15, n);
	EXPECT_EQ(-15, m);

	// this should work - but it does not ...
	//	visitAllP(type, visitor, false, n, m);
}


class ID_A : public IRVisitor<NodePtr> {
	NodePtr visitNode(const NodePtr& node) {
		return node;
	}
};

class ID_B : public IRVisitor<preserve_node_type> {
	NodePtr visitNode(const NodePtr& node) {
		return node;
	}
};


TEST(IRVisitor, TypePreservation) {
	NodeManager manager;
	IRBuilder builder(manager);

	GenericTypePtr a = builder.genericType("A");
	TupleTypePtr b = builder.tupleType();

	ID_A idA;
	ID_B idB;

	// with the version A you can only do this
	NodePtr a1 = idA.visit(a);
	NodePtr b1 = idA.visit(b);

	GenericTypePtr a2 = idB.visit(a);
	TupleTypePtr b2 = idB.visit(b);
}
