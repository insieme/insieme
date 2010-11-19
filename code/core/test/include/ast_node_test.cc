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

#include "insieme/core/ast_node.h"
#include "insieme/core/annotation.h"

#include "insieme/utils/container_utils.h"

#include "dummy_annotations.cc"

namespace insieme {
namespace core {


template<typename T>
Node::ChildList toList(const vector<T>& list) {
	Node::ChildList res;
	std::copy(list.begin(), list.end(), back_inserter(res));
	return res;
}

template<typename NP>
void basicNodeTests(NP node, const Node::ChildList& children = Node::ChildList()) {

	typedef typename NP::element_type T;

	SCOPED_TRACE( typeid(NP).name() );

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
	NodeManager manager2;
	T copy = T(*node);
	T* clone = &*manager.get(node);
	T* clone2 = &*manager2.get(node);

	EXPECT_EQ(&clone->getNodeManager(), &manager);
	EXPECT_EQ(&clone2->getNodeManager(), &manager2);
	EXPECT_NE(&node->getNodeManager(), &manager);

	// cloning had to be successful
	EXPECT_TRUE(clone);
	EXPECT_TRUE(clone2);

	// and all children have to be in the new manager
	const Node::ChildList& list = clone->getChildList();
	std::for_each(list.begin(), list.end(), [&manager](const NodePtr& cur) {
		EXPECT_TRUE(manager.addressesLocal(cur));
	});

	const Node::ChildList& list2 = clone2->getChildList();
	std::for_each(list2.begin(), list2.end(), [&manager2](const NodePtr& cur) {
		EXPECT_TRUE(manager2.addressesLocal(cur));
	});

	// check whether all are equal
	T* all[] = { &*node, &copy, clone, clone2 };
	for (int i=0; i<4; i++) {
		for (int j=0; j<4; j++) {

			T* a = all[i];
			T* b = all[j];

			EXPECT_EQ ( *a , *b );
			EXPECT_EQ ( a->hash(), b->hash() );
			EXPECT_EQ ( a->toString(), b->toString() );

		}
	}

	// check type properties
	for (int i=0; i<4; i++) {

		T* cur = all[i];

		// check children
		EXPECT_TRUE( equals(children, cur->getChildList(), equal_target<insieme::core::NodePtr>()) );
	}

	// --------------- Annotations -------------

	// check whether annotations become isolated when migrating nodes between managers

	EXPECT_TRUE(node->getAnnotations().empty());

	node->addAnnotation(std::make_shared<DummyAnnotation>(12));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(node->hasAnnotation(DummyAnnotation2::DummyKey));

	// add one annotation to each child - exposing its index
	int i = 0;
	for_each(node->getChildList(), [&i](const NodePtr& cur) {
		cur.addAnnotation(std::make_shared<DummyAnnotation2>(i++));
	});

	NodeManager managerA;
	NodeManager managerB;

	NP nodeInA = managerA.get(node);
	EXPECT_TRUE(nodeInA->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(nodeInA->hasAnnotation(DummyAnnotation2::DummyKey));

	// check child annotations
	i = 0;
	for_each(node->getChildList(), [&i](const NodePtr& cur) {

		EXPECT_FALSE(cur.hasAnnotation(DummyAnnotation::DummyKey));
		EXPECT_TRUE(cur.hasAnnotation(DummyAnnotation2::DummyKey));

		if (cur.hasAnnotation(DummyAnnotation2::DummyKey)) {
			EXPECT_EQ(i, cur.getAnnotation(DummyAnnotation2::DummyKey)->value);
		}

		i++;
	});

	EXPECT_NE(&nodeInA->getAnnotations(), &node->getAnnotations());
	EXPECT_EQ(*node, *nodeInA);

	nodeInA->addAnnotation(std::make_shared<DummyAnnotation2>(14));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(node->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(nodeInA->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(nodeInA->hasAnnotation(DummyAnnotation2::DummyKey));

	NP nodeInB = managerB.get(node);
	EXPECT_EQ(*node, *nodeInA);
	EXPECT_EQ(*node, *nodeInB);
	EXPECT_EQ(*nodeInA, *nodeInB);

	EXPECT_TRUE(nodeInB->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(nodeInB->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_NE(&nodeInA->getAnnotations(), &node->getAnnotations());
	EXPECT_NE(&nodeInA->getAnnotations(), &nodeInB->getAnnotations());
	EXPECT_NE(&node->getAnnotations(), &nodeInB->getAnnotations());

	// remove all annotations
	node->remAnnotation(DummyAnnotation::DummyKey);
	node->remAnnotation(DummyAnnotation2::DummyKey);

	// check children isolation
	NP allPtr[] = { node, nodeInA, nodeInB };
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {
			if (i==j) {
				continue;
			}

			auto a = allPtr[i]->getChildList();
			auto b = allPtr[j]->getChildList();

			std::for_each(
					make_paired_iterator(a.begin(), b.begin()),
					make_paired_iterator(a.end(), b.end()),
					[](const std::pair<NodePtr, NodePtr>& cur) {

				EXPECT_NE(cur.first, cur.second);
				EXPECT_EQ(*cur.first, *cur.second);
				EXPECT_NE(&cur.first.getAnnotations(), &cur.second.getAnnotations());

			});
		}
	}

	// TODO: check child list vs. node mapper argument
	for (int i=0; i<3; i++) {

		NP cur = allPtr[i];

		int count = 0;
		unsigned mask = 0;
		auto mapper = makeLambdaMapper([&](unsigned pos, const NodePtr& ptr)-> const NodePtr {
			count++;
			EXPECT_EQ(ptr, cur->getChildList()[pos]);
			EXPECT_EQ(*ptr, *(node->getChildList()[pos]));
			EXPECT_FALSE(mask & (1<<pos));
			mask |= 1<<pos;
			return ptr;
		});

		cur->substitute(manager, mapper);

		// check number of visited children
		EXPECT_EQ(cur->getChildList().size(), static_cast<size_t>(count));

		// all children have to be visited (mask should have all 1s)
		EXPECT_EQ(static_cast<unsigned>((1<<count) - 1), mask);
	}

	// check whether annotation is preservered during copy process

	EXPECT_TRUE(node->getAnnotations().empty());

	node->addAnnotation(std::make_shared<DummyAnnotation>(12));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(node->hasAnnotation(DummyAnnotation2::DummyKey));

	// clone node
	NodeManager manager3;
	NP other = manager3.get(node);

	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(node->hasAnnotation(DummyAnnotation2::DummyKey));

	EXPECT_TRUE(other->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(other->hasAnnotation(DummyAnnotation2::DummyKey));

	EXPECT_EQ(node->getAnnotation(DummyAnnotation::DummyKey)->value,
			  other->getAnnotation(DummyAnnotation::DummyKey)->value);

	node->remAnnotation(DummyAnnotation::DummyKey);
}

} // end namespace core
} // end namespace insieme

