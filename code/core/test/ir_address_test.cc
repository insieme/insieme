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
 *
 */
#include <gtest/gtest.h>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace core {

	typedef NodeAddress::Path Path;

	TEST(NodeAddressTest, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		// start with root node R
		NodeAddress addrRoot(root);
		EXPECT_TRUE(addrRoot);
		EXPECT_TRUE(addrRoot.isValid());
		EXPECT_EQ(static_cast<unsigned short>(1), addrRoot.getDepth());

		EXPECT_EQ(root, addrRoot.getAddressedNode());
		EXPECT_EQ(root, addrRoot.getRootNode());
		EXPECT_EQ(addrRoot, addrRoot.getParentAddress(0));

		EXPECT_EQ("0", toString(addrRoot));

		// go to node R-A
		NodeAddress addrA = addrRoot.getAddressOfChild(2).getAddressOfChild(0);
		EXPECT_TRUE(addrA);
		EXPECT_TRUE(addrA.isValid());
		EXPECT_EQ(static_cast<unsigned short>(3), addrA.getDepth());

		EXPECT_EQ(typeA, addrA.getAddressedNode());
		EXPECT_EQ(root, addrA.getRootNode());
		EXPECT_EQ(addrA, addrA.getParentAddress(0));
		EXPECT_EQ(addrRoot.getAddressOfChild(2), addrA.getParentAddress(1));
		EXPECT_EQ(addrRoot, addrA.getParentAddress(2));
		EXPECT_EQ(typeA, addrA.getParentNode(0));
		EXPECT_EQ(root, addrA.getParentNode(2));

		EXPECT_EQ("0-2-0", toString(addrA));

		// go to child R-A-2
		TypePtr type2 = builder.genericType("2");
		NodeAddress addrA2 = addrA.getAddressOfChild(2).getAddressOfChild(1);
		EXPECT_TRUE(addrA2);
		EXPECT_TRUE(addrA2.isValid());
		EXPECT_EQ(static_cast<unsigned short>(5), addrA2.getDepth());

		EXPECT_EQ(type2, addrA2.getAddressedNode());
		EXPECT_EQ(root, addrA2.getRootNode());

		EXPECT_EQ(addrA2, addrA2.getParentAddress(0));
		EXPECT_EQ(addrA.getAddressOfChild(2), addrA2.getParentAddress(1));
		EXPECT_EQ(addrA, addrA2.getParentAddress(2));
		EXPECT_EQ(addrRoot.getAddressOfChild(2), addrA2.getParentAddress(3));
		EXPECT_EQ(addrRoot, addrA2.getParentAddress(4));

		EXPECT_EQ(type2, addrA2.getParentNode(0));
		EXPECT_EQ(typeA->getChildList()[2], addrA2.getParentNode(1));
		EXPECT_EQ(typeA, addrA2.getParentNode(2));
		EXPECT_EQ(root->getChildList()[2], addrA2.getParentNode(3));
		EXPECT_EQ(root, addrA2.getParentNode(4));

		EXPECT_EQ("0-2-0-2-1", toString(addrA2));

		EXPECT_EQ(addrA2, NodeAddress(Path(root).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(1)));
		EXPECT_NE(addrA2, NodeAddress(Path(root).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0)));
	}

	TEST(NodeAddressTest, NullTest) {
		NodeAddress addr;
		EXPECT_FALSE(addr);
		EXPECT_EQ("NULL", toString(addr));
	}

	TEST(NodeAddressTest, SizeTest) {
		NodeAddress addr;
		EXPECT_EQ(sizeof(addr), 3 * sizeof(int*));
	}

	TEST(NodeAddressTest, RootPathTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		GenericTypePtr genType = builder.genericType("A");

		GenericTypeAddress adr(genType);

		EXPECT_TRUE(adr.isValid());
		EXPECT_TRUE(adr.isRoot());

		EXPECT_FALSE(adr->getName().isRoot());
	}


	TEST(NodeAddressTest, HashSinglePath) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.genericType("A");

		TypeAddress adr(type);

		EXPECT_EQ(adr.hash(), adr.getPath().hash());
	}


	TEST(NodeAddressTest, EqualityTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test a diamond
		TypePtr typeD = builder.genericType("D");
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(typeD));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeD));
		TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

		EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

		// start with root node R
		NodeAddress ABD1(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeAddress ABD2(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeAddress ACD(Path(typeA).extendForChild(2).extendForChild(1).extendForChild(2).extendForChild(0));

		EXPECT_EQ(ABD1, ABD2);
		EXPECT_NE(ABD1, ACD);
		EXPECT_NE(ABD2, ACD);

		EXPECT_EQ(ABD1.getAddressedNode(), ACD.getAddressedNode());
		EXPECT_EQ(ABD2.getAddressedNode(), ACD.getAddressedNode());
	}

	TEST(NodeAddressTest, LessTest) {
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
		NodeAddress ABD1(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeAddress ABD2(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));
		NodeAddress AC(Path(typeA).extendForChild(2).extendForChild(1).extendForChild(2));

		EXPECT_FALSE(ABD1 < ABD2);
		EXPECT_FALSE(ABD2 < ABD1);

		EXPECT_TRUE(ABD1 < AC);
		EXPECT_FALSE(AC < ABD1);
		EXPECT_FALSE(AC < ABD2);

		EXPECT_TRUE(ABD1 < AC);
		EXPECT_FALSE(AC < ABD1);

		NodeAddress BD(Path(typeB).extendForChild(2));
		EXPECT_EQ("[D]", toString(*BD));
		NodeAddress CDE(Path(typeC).extendForChild(2).extendForChild(0));
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

	TEST(NodeAddressTest, LessTest2) {
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

	TEST(NodeAddressTest, MergePaths) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test a diamond
		TypePtr typeD = builder.genericType("D");
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(typeD));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeD));
		TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

		EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

		// start with root node R
		NodeAddress BD(Path(typeB).extendForChild(2).extendForChild(0));
		NodeAddress AB(Path(typeA).extendForChild(2).extendForChild(0));

		NodeAddress ABD1 = concat(AB, BD);
		EXPECT_EQ("0-2-0-2-0", toString(ABD1));

		EXPECT_EQ(typeD, ABD1.getAddressedNode());
	}

	TEST(NodeAddressTest, Find) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		NodeAddress addrRoot(root);

		EXPECT_EQ(Address<const Type>::find(typeC, root), addrRoot.getAddressOfChild(2, 2));
		EXPECT_EQ(Address<const Type>::find(typeC, root, false), addrRoot.getAddressOfChild(2, 2));
	}

	TEST(NodeAddressTest, FindAll) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA1 = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeA2 = builder.genericType("A", toVector<TypePtr>(builder.genericType("3"), builder.genericType("4")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA1, typeB, typeC, typeC, typeA2));

		EXPECT_EQ("root<A<1,2>,B<3>,C,C,A<3,4>>", toString(*root));

		auto addr1 = Address<const Type>::findAll(typeC, root);
		auto addr2 = Address<const Type>::findAll(typeC, root, false);

		auto exp_addr1 = NodeAddress(Path(root).extendForChild(2).extendForChild(2));
		auto exp_addr2 = NodeAddress(Path(root).extendForChild(2).extendForChild(3));

		EXPECT_EQ(2, addr1.size());
		EXPECT_EQ(2, addr2.size());

		EXPECT_EQ(exp_addr1, addr1[0]);
		EXPECT_EQ(exp_addr2, addr1[1]);

		EXPECT_EQ(exp_addr1, addr2[0]);
		EXPECT_EQ(exp_addr2, addr2[1]);
	}

TEST(NodeAddressTest, CloneTo) {
	NodeManager nm1;
	NodeManager nm2;

	IRBuilder builder(nm1);

	TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
	TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
	TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

	TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

	EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

	NodeAddress rootAddress(root);

	NodeAddress newOwnRoot = rootAddress.cloneTo(nm1);
	EXPECT_TRUE(&(newOwnRoot.getNodeManager()) == &nm1);

	NodeAddress newRoot = rootAddress.cloneTo(nm2);

	EXPECT_EQ(Address<const Type>::find(typeC, root), rootAddress.getAddressOfChild(2, 2));
	EXPECT_EQ(Address<const Type>::find(typeC, newRoot), rootAddress.getAddressOfChild(2, 2));
	EXPECT_EQ(Address<const Type>::find(typeC, newRoot), newRoot.getAddressOfChild(2, 2));

	EXPECT_EQ(Address<const Type>::find(typeC, newOwnRoot), rootAddress.getAddressOfChild(2, 2));
	EXPECT_EQ(Address<const Type>::find(typeC, newOwnRoot), newOwnRoot.getAddressOfChild(2, 2));

}

	TEST(NodeAddressTest, Visiting) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test a diamond
		TypePtr typeD = builder.genericType("D");
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(typeD));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeD));
		TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

		EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

		// start with root node R
		NodeAddress ABD(Path(typeA).extendForChild(2).extendForChild(0).extendForChild(2).extendForChild(0));

		vector<TypePtr> list;
		auto collector = makeLambdaVisitor([&](const TypePtr& type) { list.push_back(type); }, true);

		// bottom up visitor
		list.clear();
		visitPathBottomUp(ABD, collector);
		EXPECT_EQ(toVector<TypePtr>(typeD, typeB, typeA), list);

		// top down visitor
		list.clear();
		visitPathTopDown(ABD, collector);
		EXPECT_EQ(toVector<TypePtr>(typeA, typeB, typeD), list);


		auto interruptCollector = makeLambdaVisitor([&](const TypePtr& type) {
			if(list.size() > static_cast<std::size_t>(1)) { return true; }
			list.push_back(type);
			return false;
		});

		// bottom up visitor
		list.clear();
		EXPECT_TRUE(visitPathBottomUpInterruptible(ABD, interruptCollector));
		EXPECT_EQ(toVector<TypePtr>(typeD, typeB), list);

		// top down visitor
		list.clear();
		EXPECT_TRUE(visitPathTopDownInterruptible(ABD, interruptCollector));
		EXPECT_EQ(toVector<TypePtr>(typeA, typeB), list);
	}

	TEST(NodeAddressTest, IsChildOf) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		NodeAddress addr(root);

		// isChildOf will also return true when passed the same addresses
		EXPECT_TRUE(isChildOf(addr, addr));

		NodeAddress addr1 = addr.getAddressOfChild(2); // the Types child
		NodeAddress addr2 = addr.getAddressOfChild(2).getAddressOfChild(0); // typeA

		EXPECT_EQ("0-2", toString(addr1));
		EXPECT_EQ("0-2-0", toString(addr2));

		EXPECT_TRUE(isChildOf(addr, addr1));
		EXPECT_TRUE(isChildOf(addr, addr2));
		EXPECT_TRUE(isChildOf(addr1, addr2));

		NodeAddress addr3 = addr.getAddressOfChild(0);
		EXPECT_EQ("0-0", toString(addr3));

		EXPECT_TRUE(isChildOf(addr, addr3));
		EXPECT_FALSE(isChildOf(addr1, addr3));

		// ensure that isChildOf also compares the root nodes
		NodeAddress addrB(typeB); // typeB - as root
		NodeAddress addrB3 = addrB.getAddressOfChild(2).getAddressOfChild(0); // 3 as child of typeB

		EXPECT_EQ("0-2-0", toString(addrB3));

		// addrB3 is NOT a child of addr1
		EXPECT_FALSE(isChildOf(addr1, addrB3));
	}

	TEST(NodeAddressTest, UpdateRoot) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A", toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
		TypePtr typeB = builder.genericType("B", toVector<TypePtr>(builder.genericType("3")));
		TypePtr typeC = builder.genericType("C", toVector<TypePtr>());

		TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

		EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

		NodeAddress addr(root);

		NodeAddress addr1 = addr.getAddressOfChild(2);
		EXPECT_EQ("[A<1,2>,B<3>,C]", toString(*addr1));
		NodeAddress addr2 = addr.getAddressOfChild(2).getAddressOfChild(0);
		EXPECT_EQ(typeA, addr2);

		NodeAddress addr3 = cropRootNode(addr2, addr1);
		EXPECT_EQ("[A<1,2>,B<3>,C]", toString(*addr3.getRootNode()));
		EXPECT_EQ(typeA, addr3);
		EXPECT_NE(addr2, addr3);
		EXPECT_EQ(addr2.getAddressedNode(), addr3.getAddressedNode());
	}


	TEST(NodeAddressTest, ImplicitConversion) {
		NodeManager manager;
		IRBuilder builder(manager);

		GenericTypePtr genTypePtr = builder.genericType("A");
		TypePtr typePtr = genTypePtr;

		// not working (explicit construction required)
		// GenericTypeAddress genTypeAddr = genTypePtr;

		GenericTypeAddress genTypeAddr(genTypePtr);
		TypeAddress typeAddr = genTypeAddr;


		// now testing pure assignments
		NodePtr ptr;
		StatementPtr stmtPtr;

		ptr = genTypePtr;
		ptr = typePtr;

		typePtr = genTypePtr;

		// should not work
		//	genTypePtr = ptr;  // => compiler error


		// same for addresses
		NodeAddress addr;
		StatementAddress stmtAddr;

		addr = genTypeAddr;
		addr = typeAddr;

		typeAddr = genTypeAddr;

		// should not work
		//	genTypeAddr = addr; // => compiler error


		// cross-pointer/address passing

		//	addr = typePtr; // => should not work

		genTypePtr = genTypeAddr;
		ptr = genTypeAddr;

		//	NodePtr ptr;
		//	NodeAddress addr;
	}


	TEST(NodeAddressTest, NullComparison) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr nodeA = builder.genericType("A");
		NodePtr nodeB = builder.genericType("B");

		NodeAddress adrA(nodeA);
		NodeAddress adrB(nodeB);
		NodeAddress nil;

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

		NodeAddress node(builder.genericType("A"));

		// check target node type
		TypeAddress type = node.as<TypeAddress>();
		GenericTypeAddress genType = node.as<GenericTypeAddress>();

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

		NodeAddress a(builder.intLit(12));

		// this should work
		EXPECT_TRUE(a.isa<ExpressionPtr>());
		EXPECT_TRUE(a.isa<ExpressionAddress>());
		a.as<ExpressionAddress>();

		// this should also work
		EXPECT_TRUE(a.isa<LiteralPtr>());
		EXPECT_TRUE(a.isa<LiteralAddress>());
		a.as<LiteralAddress>();

		// this should not work
		// a.as<CallExprAddress>();
	}


	TEST(Address, MultiLevelIndirectionSegfaultBug) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto lit = builder.intLit(12);

		LiteralAddress a(lit);
		NodeAddress x = a->getChildList()[0]->getChildList()[0];

		EXPECT_EQ("int", toString(*x));
	}

} // end namespace core
} // end namespace insieme
