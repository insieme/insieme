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

#include "ast_node.h"
#include "annotation.h"
#include "container_utils.h"


namespace insieme {
namespace core {



class DummyAnnotation : public Annotation {



};

template<typename NodePtr>
Node::ChildList toList(const vector<NodePtr>& list) {
	Node::ChildList res;
	std::copy(list.begin(), list.end(), back_inserter(res));
	return res;
}

template<typename NodePtr>
void basicNodeTests(NodePtr node, const Node::ChildList& children = Node::ChildList()) {

	typedef typename NodePtr::element_type T;

	// ------------ Node Ptr based tests -------------

	// check children
	EXPECT_TRUE ( equals(children, node->getChildList(), equal_target<insieme::core::NodePtr>()) );

	// ------------ Annotation Tests -------------

	// check for shared annotations
	Node::ChildList listA = node->getChildList();
	Node::ChildList listB = node->getChildList();

	EXPECT_TRUE ( equals( listA, listB ) );
	EXPECT_TRUE ( equals( listA, listB, equal_target<insieme::core::NodePtr>()) );


	// ------------ Type Token based tests -------------

	// copy and clone the type
	NodeManager manager;
	T copy = T(*node);
	T* clone = dynamic_cast<T*>(static_cast<const Node*>(&*node)->clone(manager));

	// check whether all are equal
	T* all[] = { &*node, &copy, clone };
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {

			T* a = all[i];
			T* b = all[j];

			EXPECT_EQ ( *a , *b );
			EXPECT_EQ ( a->hash(), b->hash() );
			EXPECT_EQ ( a->getName(), b->getName() );
			EXPECT_EQ ( a->toString(), b->toString() );

		}
	}

	// check type properties
	for (int i=0; i<3; i++) {

		T* cur = all[i];

		// check children
		EXPECT_TRUE( equals(children, cur->getChildList(), equal_target<insieme::core::NodePtr>()) );
	}

	delete clone;
}

} // end namespace core
} // end namespace insieme

