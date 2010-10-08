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

#include <vector>

#include "ast_builder.h"
#include "ast_navigating_visitor.h"
#include "container_utils.h"

using namespace insieme::core;

class SimpleNavigator : public NavigatingASTVisitor {

public:

	/**
	 * a list accumulating the addresses of all visited nodes
	 */
	std::vector<NodeAddress> list;

	void visitNode(const NodePtr& cur) {
		list.push_back(getCurrentAddress());
		const Node::ChildList& children = cur->getChildList();
		std::for_each(children.begin(), children.end(), [&](const NodePtr& cur) {
			this->visit(cur);
		});
	}

};

TEST(NavigatingASTVisitor, Basic) {

	// build diamond
	ASTBuilder builder;

	TypePtr typeD = builder.genericType("D");
	TypePtr typeB = builder.genericType("B",toVector<TypePtr>(typeD));
	TypePtr typeC = builder.genericType("C",toVector<TypePtr>(typeD));
	TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

	EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

	SimpleNavigator navigator;
	navigator.list.clear();

	navigator.visit(typeA);

	// construct all node addresses
	NodeAddress addrA(typeA);
	NodeAddress addrB = addrA.getAddressOfChild(0);
	NodeAddress addrD1 = addrB.getAddressOfChild(0);
	NodeAddress addrC = addrA.getAddressOfChild(1);
	NodeAddress addrD2 = addrC.getAddressOfChild(0);
	std::vector<NodeAddress> all = toVector(addrA,addrB,addrD1, addrC, addrD2);

	EXPECT_EQ(all, navigator.list);

	EXPECT_TRUE(::all(navigator.list, [](const NodeAddress& cur) {
		return cur.isValid();
	}));

	navigator.list.clear();
	navigator.visit(typeB);
	addrB = NodeAddress(typeB);
	addrD1 = addrB.getAddressOfChild(0);
	EXPECT_EQ(toVector(addrB, addrD1), navigator.list);

	EXPECT_TRUE(::all(navigator.list, [](const NodeAddress& cur) {
		return cur.isValid();
	}));

}
