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

#include "insieme/core/ir_instance.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace core {

	typedef NodeInstance::Path Path;

	TEST(IRInstanceTest, IteratorValue) {
		IteratorValue v0;
		IteratorValue v1 = 1;
		IteratorValue v2 = 2;

		IteratorValue vS = IteratorValue::STAR;

		IteratorValue vn1 = -1;
		IteratorValue vn2 = -2;

		EXPECT_EQ(v0, v0);

		EXPECT_EQ("0", toString(v0));
		EXPECT_EQ("1", toString(v1));
		EXPECT_EQ("2", toString(v2));
		EXPECT_EQ("*", toString(vS));
		EXPECT_EQ("-1", toString(vn1));
		EXPECT_EQ("-2", toString(vn2));

		EXPECT_LT(v0, v1);
		EXPECT_LT(v0, v2);
		EXPECT_LT(v0, vS);
		EXPECT_LT(v0, vn2);
		EXPECT_LT(v0, vn1);

		EXPECT_LT(v1, v2);
		EXPECT_LT(v1, vS);
		EXPECT_LT(v1, vn2);
		EXPECT_LT(v1, vn1);

		EXPECT_LT(v2, vS);
		EXPECT_LT(v2, vn2);
		EXPECT_LT(v2, vn1);

		EXPECT_LT(vS, vn2);
		EXPECT_LT(vS, vn1);

		EXPECT_LT(vn2, vn1);

		auto list = toVector(vn1, vS, v0, vn2, v2, v1);
		std::sort(list.begin(), list.end());
		EXPECT_EQ("[0,1,2,*,-2,-1]", toString(list));
	}

	TEST(NodeInstanceTest, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		// start with root node R
		NodeInstance instRoot(root);
		EXPECT_TRUE(instRoot);
		EXPECT_TRUE(instRoot.isValid());
		EXPECT_EQ(static_cast<unsigned short>(1), instRoot.getDepth());

		EXPECT_EQ(root, instRoot.getAddressedNode());
		EXPECT_EQ(root, instRoot.getRootNode());
		EXPECT_EQ(instRoot, instRoot.getParentInstance(0));

		EXPECT_EQ("0", toString(instRoot));

		// go to node R-A
		NodeInstance instA = instRoot.getInstanceOfChild(2).getInstanceOfChild(0);
		EXPECT_TRUE(instA);
		EXPECT_TRUE(instA.isValid());
		EXPECT_EQ(static_cast<unsigned short>(3), instA.getDepth());

		EXPECT_EQ(typeA, instA.getAddressedNode());
		EXPECT_EQ(root, instA.getRootNode());
		EXPECT_EQ(instA, instA.getParentInstance(0));
		EXPECT_EQ(instRoot.getInstanceOfChild(2), instA.getParentInstance(1));
		EXPECT_EQ(instRoot, instA.getParentInstance(2));
		EXPECT_EQ(typeA, instA.getParentNode(0));
		EXPECT_EQ(root, instA.getParentNode(2));

		EXPECT_EQ("0-2-0", toString(instA));

		// go to child R-A-2
		TypePtr type2 = builder.genericType("2");
		NodeInstance instA2 = instA.getInstanceOfChild(2).getInstanceOfChild(1);
		EXPECT_TRUE(instA2);
		EXPECT_TRUE(instA2.isValid());
		EXPECT_EQ(static_cast<unsigned short>(5), instA2.getDepth());

		EXPECT_EQ(type2, instA2.getAddressedNode());
		EXPECT_EQ(root, instA2.getRootNode());

		EXPECT_EQ(instA2, instA2.getParentInstance(0));
		EXPECT_EQ(instA.getInstanceOfChild(2), instA2.getParentInstance(1));
		EXPECT_EQ(instA, instA2.getParentInstance(2));
		EXPECT_EQ(instRoot.getInstanceOfChild(2), instA2.getParentInstance(3));
		EXPECT_EQ(instRoot, instA2.getParentInstance(4));

		EXPECT_EQ(type2, instA2.getParentNode(0));
		EXPECT_EQ(typeA->getChildList()[2], instA2.getParentNode(1));
		EXPECT_EQ(typeA, instA2.getParentNode(2));
		EXPECT_EQ(root->getChildList()[2], instA2.getParentNode(3));
		EXPECT_EQ(root, instA2.getParentNode(4));

		EXPECT_EQ("0-2-0-2-1", toString(instA2));

		EXPECT_EQ(instA2, NodeInstance(Path(root).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(1)));
		EXPECT_NE(instA2.getPath(), Path(root).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		EXPECT_NE(instA2, NodeInstance(Path(root).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0)));
	}

	TEST(NodeInstance, AddressConversion) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		NodeAddress addr = NodeAddress(root).getAddressOfChild(2, 0, 2, 1);
		EXPECT_EQ("0-2-0-2-1", toString(addr));

		NodeInstance inst = addr;
		EXPECT_EQ("0-2-0-2-1", toString(inst));

		NodeAddress addr2 = inst;
		NodeInstance inst2 = addr2;

		EXPECT_EQ(addr, addr2);
		EXPECT_EQ(inst, inst2);
	}

	TEST(NodeInstanceTest, NullTest) {
		NodeInstance addr;
		EXPECT_FALSE(addr);
		EXPECT_EQ("NULL", toString(addr));
	}

	TEST(NodeInstanceTest, SizeTest) {
		NodeInstance addr;
		EXPECT_EQ(sizeof(addr), 3 * sizeof(int*));
	}

	TEST(NodeInstanceTest, RootPathTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		GenericTypePtr genType = builder.genericType("A");

		GenericTypeInstance adr(genType);

		EXPECT_TRUE(adr.isValid());
		EXPECT_TRUE(adr.isRoot());

		EXPECT_FALSE(adr->getName().isRoot());
	}


	TEST(NodeInstanceTest, HashSinglePath) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.genericType("A");

		TypeInstance adr(type);

		EXPECT_EQ(adr.hash(), adr.getPath().hash());
	}


	TEST(NodeInstanceTest, EqualityTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test a diamond
		TypePtr typeD = builder.genericType("D");
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(typeD));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeD));
		TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

		EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

		// start with root node R
		NodeInstance ABD1(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeInstance ABD2(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeInstance ACD(Path(typeA).extendForChild(2).extendForChild(1).extendForChild(2).extendForChild(0));

		EXPECT_EQ(ABD1, ABD2);
		EXPECT_NE(ABD1, ACD);
		EXPECT_NE(ABD2, ACD);

		EXPECT_EQ(ABD1.getAddressedNode(), ACD.getAddressedNode());
		EXPECT_EQ(ABD2.getAddressedNode(), ACD.getAddressedNode());
	}

	TEST(NodeInstanceTest, ForLoopIteration) {
		NodeManager manager;
		IRBuilder builder(manager);

		StatementPtr loop = builder.parseStmt("for(int<4> i = 0 .. 10 : 1) {"
		                                      "	for(int<4> j = 0 .. 10 : 1) {"
		                                      "		1;"
		                                      "	}"
		                                      "}");

		ASSERT_TRUE(loop);

		StatementInstance root(loop);

		auto a0 = root.as<ForStmtInstance>()->getBody();
		EXPECT_EQ("0-3/0", toString(a0));

		auto a5 = a0.getIteration(5);
		EXPECT_EQ("0-3/5", toString(a5));

		EXPECT_LT(a0, a5);

		auto b00 = a0.as<CompoundStmtInstance>()[0].as<ForStmtInstance>()->getBody();
		EXPECT_EQ("0-3/0-0-3/0", toString(b00));

		auto b50 = a5.as<CompoundStmtInstance>()[0].as<ForStmtInstance>()->getBody();
		EXPECT_EQ("0-3/5-0-3/0", toString(b50));

		auto b07 = a0.as<CompoundStmtInstance>()[0].as<ForStmtInstance>()->getBody().getIteration(7);
		EXPECT_EQ("0-3/0-0-3/7", toString(b07));

		auto b57 = a5.as<CompoundStmtInstance>()[0].as<ForStmtInstance>()->getBody().getIteration(7);
		EXPECT_EQ("0-3/5-0-3/7", toString(b57));

		// some ordering checks ..

		EXPECT_LT(a0, a5);
		EXPECT_LT(a0, b00);
		EXPECT_LT(a0, b07);

		EXPECT_LT(a0, a5);
		EXPECT_LT(b00, a5);
		EXPECT_LT(b07, a5);
		EXPECT_LT(a5, b50);
		EXPECT_LT(a5, b57);
		EXPECT_LT(b50, b57);
	}

	TEST(NodeInstanceTest, WhileLoopIteration) {
		NodeManager manager;
		IRBuilder builder(manager);

		StatementPtr loop = builder.parseStmt("while ( true ) {"
											  "	1;"
											  "}");

		ASSERT_TRUE(loop);

		StatementInstance root(loop);

		auto a0 = root.as<WhileStmtInstance>()->getBody();
		EXPECT_EQ("0-1/0", toString(a0));

		auto a5 = a0.getIteration(5);
		EXPECT_EQ("0-1/5", toString(a5));

		EXPECT_LT(a0, a5);
	}

	TEST(NodeInstanceTest, Visitor) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		// just check whether it can be utilized within the visitor framework
		vector<NodeInstance> list;
		visitDepthFirst(NodeInstance(root), [&](const NodeInstance& cur) { list.push_back(cur); }, true, true);

		EXPECT_EQ(28u, list.size()) << list;
	}

	TEST(NodeInstanceTest, LessTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test a diamond
		TypePtr typeE = builder.genericType("E");
		TypePtr typeDE = builder.genericType("D", toVector<TypePtr>(typeE));
		TypePtr typeD = builder.genericType("D");
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(typeD));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeDE));
		TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

		EXPECT_EQ("A<B<D>,C<D<E>>>", toString(*typeA));

		// start with root node R
		NodeInstance ABD1(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeInstance ABD2(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeInstance AC(Path(typeA).extendForChild(2).extendForChild(1).extendForChild(2));

		EXPECT_FALSE(ABD1 < ABD2);
		EXPECT_FALSE(ABD2 < ABD1);

		EXPECT_TRUE(ABD1 < AC);
		EXPECT_FALSE(AC < ABD1);
		EXPECT_FALSE(AC < ABD2);

		EXPECT_TRUE(ABD1 < AC);
		EXPECT_FALSE(AC < ABD1);

		NodeInstance BD(Path(typeB).extendForChild(2));
		EXPECT_EQ("[D]", toString(*BD));
		NodeInstance CDE(Path(typeC).extendForChild(2).extendForChild(0));
		EXPECT_EQ("D<E>", toString(*CDE));

		if(*typeB < *typeC) {
			EXPECT_FALSE(CDE < BD);
			EXPECT_TRUE(BD < CDE);
		} else {
			EXPECT_FALSE(BD < CDE);
			EXPECT_TRUE(CDE < BD);
		}

		EXPECT_FALSE(BD < BD);
		EXPECT_FALSE(BD < BD);
	}

	TEST(NodeInstanceTest, LessTest2) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr typeA = builder.genericType("A");
		TypePtr typeB = builder.genericType("B");

		// test root-node equality
		TypeAddress addrA(typeA);
		TypeAddress addrB(typeB);
		TypeAddress addrC(typeB);

		EXPECT_EQ(addrA, addrA);
		EXPECT_NE(addrA, addrB);
		EXPECT_EQ(addrB, addrC);

		EXPECT_NE(addrA, addrC);

		if(*typeA < *typeB) {
			EXPECT_LT(addrA, addrB);
			EXPECT_LT(addrA, addrC);
		} else {
			EXPECT_LT(addrB, addrA);
			EXPECT_LT(addrC, addrA);
		}

		EXPECT_TRUE(addrA < addrB || addrB < addrA);
		EXPECT_TRUE(addrA < addrC || addrC < addrA);
		EXPECT_FALSE(addrB < addrC || addrC < addrB);
	}


	TEST(NodeInstanceTest, ImplicitConversion) {
		NodeManager manager;
		IRBuilder builder(manager);

		GenericTypePtr genTypePtr = builder.genericType("A");
		TypePtr typePtr = genTypePtr;

		// not working (explicit construction required)
		// GenericTypeAddress genTypeAddr = genTypePtr;

		GenericTypeInstance genTypeInst(genTypePtr);
		TypeInstance typeInst = genTypeInst;


		// now testing pure assignments
		NodePtr ptr;

		ptr = genTypePtr;
		ptr = typePtr;

		typePtr = genTypePtr;

		// should not work
		//	genTypePtr = ptr;  // => compiler error


		NodeAddress addr;

		// same for addresses
		NodeInstance inst;
		StatementInstance stmtInst;

		addr = genTypeInst;
		addr = typeInst;

		typeInst = genTypeInst;

		// should not work
		//	genTypeAddr = addr; // => compiler error


		// cross-pointer/address passing

		//	addr = typePtr; // => should not work

		genTypePtr = genTypeInst;
		ptr = genTypeInst;

		//	NodePtr ptr;
		//	NodeInstance addr;
	}


	TEST(NodeInstanceTest, NullComparison) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr nodeA = builder.genericType("A");
		NodePtr nodeB = builder.genericType("B");

		NodeInstance adrA(nodeA);
		NodeInstance adrB(nodeB);
		NodeInstance nil;

		EXPECT_EQ(adrA, adrA);
		EXPECT_EQ(adrB, adrB);
		EXPECT_EQ(nil, nil);

		EXPECT_NE(adrA, adrB);
		EXPECT_NE(adrB, adrA);
		EXPECT_NE(nil, adrA);
		EXPECT_NE(adrA, nil);

		EXPECT_LT(nil, adrA);
		EXPECT_LT(nil, adrB);
	}

	TEST(Pointer, As) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodeInstance node(builder.genericType("A"));

		// check target node type
		TypeInstance type = node.as<TypeInstance>();
		GenericTypeInstance genType = node.as<GenericTypeInstance>();

		EXPECT_EQ(type, node);
		EXPECT_EQ(type, genType);

		TypePtr typePtr = node.as<TypePtr>();
		GenericTypePtr genTypePtr = node.as<GenericTypePtr>();

		EXPECT_EQ(typePtr, type.getAddressedNode());
		EXPECT_EQ(genTypePtr, genType.getAddressedNode());
	}


	TEST(NodePointer, DynamicCast) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodeInstance a(builder.intLit(12));

		// this should work
		EXPECT_TRUE(a.isa<ExpressionPtr>());
		EXPECT_TRUE(a.isa<ExpressionAddress>());
		EXPECT_TRUE(a.isa<ExpressionInstance>());
		a.as<ExpressionInstance>();

		// this should also work
		EXPECT_TRUE(a.isa<LiteralPtr>());
		EXPECT_TRUE(a.isa<LiteralAddress>());
		EXPECT_TRUE(a.isa<LiteralInstance>());
		a.as<LiteralInstance>();

		// this should not work
		// a.as<CallExprAddress>();
	}


} // end namespace core
} // end namespace insieme
