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
#include <sstream>
#include "visitor.h"

#include <iostream>

class MNode: public Visitable<MNode*> {

	MNode* first;
	MNode* second;

public:

	int value;

	MNode(int value = 0, MNode* first = NULL, MNode* second = NULL) :
		value(value), first(first), second(second) {
	}

	Visitable<MNode*>::ChildList getChildren() const {
		std::cout << "Forming children ... " << std::endl;
		auto res = newChildList();
		if (first) {
			res->push_back(first);
		}
		std::cout << "Adding second child ... " << std::endl;
		if (second) {
			res->push_back(second);
		}
		std::cout << "Result " << res->size() << std::endl;
		return res;
	}
};

TEST(Visitor, DepthFirstVisitor) {

	// create tree ...
	MNode n4(4);
	MNode n5(5);
	MNode n6(6);
	MNode n2(2, &n4, &n5);
	MNode n3(3, &n6);
	MNode n1(1, &n2, &n3);

	MNode& root = n1;

	std::cout << "Before" << std::endl;

	std::stringstream res;
	DepthFirstVisitor<MNode*> visitor(
			[&res](MNode* cur) { std::cout << "Step" << std::endl; res << cur->value; }
			);
	visitor.visit(&root);
	EXPECT_EQ (res.str(), "452631");

}


class CNode: public Visitable<const CNode*> {

	const CNode* first;
	const CNode* second;

public:

	int value;

	CNode(int value = 0, const CNode* first = NULL, const CNode* second = NULL) :
		value(value), first(first), second(second) {
	}

	Visitable<const CNode*>::ChildList getChildren() const {
		std::cout << "Forming children ... " << std::endl;
		auto res = newChildList();
		if (first) {
			res->push_back(first);
		}
		std::cout << "Adding second child ... " << std::endl;
		if (second) {
			res->push_back(second);
		}
		std::cout << "Result " << res->size() << std::endl;
		return res;
	}
};

TEST(Visitor, ConstDepthFirstVisitor) {

	// create tree ...
	CNode n4(4);
	CNode n5(5);
	CNode n6(6);
	CNode n2(2, &n4, &n5);
	CNode n3(3, &n6);
	CNode n1(1, &n2, &n3);

	CNode& root = n1;

	std::cout << "Before" << std::endl;

	std::stringstream res;

	DepthFirstVisitor<const CNode*> visitor( 
		[&res](const CNode* cur) { std::cout << "Step" << std::endl; res << cur->value; }		
		);
	visitor.visit(&root);


	EXPECT_EQ (res.str(), "452631");

}

