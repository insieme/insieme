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

#include "ast_address.h"
#include "ast_builder.h"

namespace insieme {
namespace core {

TEST(NodeAddressTest, Basic) {
	ASTBuilder builder;

	TypePtr typeA = builder.genericType("A",toVector<TypePtr>(builder.genericType("1"), builder.genericType("2")));
	TypePtr typeB = builder.genericType("B",toVector<TypePtr>(builder.genericType("3")));
	TypePtr typeC = builder.genericType("C",toVector<TypePtr>());

	TypePtr root = builder.genericType("root", toVector(typeA, typeB, typeC));

	EXPECT_EQ("root<A<1,2>,B<3>,C>", toString(*root));

	// start with root node R
	NodeAddress addrRoot(root);
	EXPECT_TRUE(addrRoot);
	EXPECT_TRUE(addrRoot.isValid());
	EXPECT_EQ(static_cast<unsigned short>(1), addrRoot.getDepth());

	EXPECT_EQ(root, addrRoot.getAddressedNode());
	EXPECT_EQ(root, addrRoot.getRootNode());
	EXPECT_EQ(addrRoot, addrRoot.getParentNodeAddress(0));

	EXPECT_EQ(toString(toVector(PathEntry(0, root))), toString(addrRoot.getPath()));
	EXPECT_EQ("0", toString(addrRoot));

	// go to node R-A
	NodeAddress addrA = addrRoot.getAddressOfChild(0);
	EXPECT_TRUE(addrA);
	EXPECT_TRUE(addrA.isValid());
	EXPECT_EQ(static_cast<unsigned short>(2), addrA.getDepth());

	EXPECT_EQ(typeA, addrA.getAddressedNode());
	EXPECT_EQ(root, addrA.getRootNode());
	EXPECT_EQ(addrA, addrA.getParentNodeAddress(0));
	EXPECT_EQ(addrRoot, addrA.getParentNodeAddress(1));
	EXPECT_EQ(typeA, addrA.getParentNode(0));
	EXPECT_EQ(root,  addrA.getParentNode(1));

	EXPECT_EQ(toString(toVector(PathEntry(0,root), PathEntry(0,typeA))), toString(addrA.getPath()));
	EXPECT_EQ("0-0", toString(addrA));

	// go to child R-A-2
	TypePtr type2 = builder.genericType("2");
	NodeAddress addrA2 = addrA.getAddressOfChild(1);
	EXPECT_TRUE(addrA2);
	EXPECT_TRUE(addrA2.isValid());
	EXPECT_EQ(static_cast<unsigned short>(3), addrA2.getDepth());

	EXPECT_EQ(type2, addrA2.getAddressedNode());
	EXPECT_EQ(root, addrA2.getRootNode());
	EXPECT_EQ(addrA2, addrA2.getParentNodeAddress(0));
	EXPECT_EQ(addrA, addrA2.getParentNodeAddress(1));
	EXPECT_EQ(addrRoot, addrA2.getParentNodeAddress(2));
	EXPECT_EQ(type2, addrA2.getParentNode(0));
	EXPECT_EQ(typeA, addrA2.getParentNode(1));
	EXPECT_EQ(root,  addrA2.getParentNode(2));

	EXPECT_EQ(toString(toVector(PathEntry(0,root), PathEntry(0,typeA), PathEntry(1,type2))), toString(addrA2.getPath()));
	EXPECT_EQ("0-0-1", toString(addrA2));
}


TEST(NodeAddressTest, EqualityTest) {
	ASTBuilder builder;

	// test a diamond
	TypePtr typeD = builder.genericType("D");
	TypePtr typeB = builder.genericType("B",toVector<TypePtr>(typeD));
	TypePtr typeC = builder.genericType("C",toVector<TypePtr>(typeD));
	TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

	EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

	// start with root node R
	NodeAddress ABD1(toVector(PathEntry(-1,typeA), PathEntry(0,typeB), PathEntry(0,typeD)));
	NodeAddress ABD2(toVector(PathEntry(-1,typeA), PathEntry(0,typeB), PathEntry(0,typeD)));
	NodeAddress ACD(toVector(PathEntry(-1,typeA), PathEntry(1,typeC), PathEntry(0,typeD)));

	EXPECT_EQ(ABD1, ABD2);
	EXPECT_NE(ABD1, ACD);
	EXPECT_NE(ABD2, ACD);

	EXPECT_EQ(ABD1.getAddressedNode(), ACD.getAddressedNode());
	EXPECT_EQ(ABD2.getAddressedNode(), ACD.getAddressedNode());
}

} // end namespace core
} // end namespace insieme

