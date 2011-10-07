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

#include "insieme/utils/annotation.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/program.h"

#include "insieme/utils/container_utils.h"

#include "dummy_annotations.inc"

namespace insieme {
namespace core {

bool hasHash(const Node& a, std::size_t hash) {
	return a.hash() == hash;
}

typedef std::size_t hash_t;

hash_t hash_combine(hash_t& seed, hash_t value) {
	boost::hash_combine(seed, value);
	return seed;
}

hash_t hash_combine(hash_t& seed, const string& value) {
	return hash_combine(seed, boost::hash_value(value));
}

void hash_node(hash_t& seed, const NodePtr& cur) {
	hash_combine(seed, cur->getNodeType());
	switch(cur->getNodeType()) {
		case insieme::core::NT_GenericType:
		case insieme::core::NT_ArrayType:
		case insieme::core::NT_VectorType:
		case insieme::core::NT_ChannelType:
		case insieme::core::NT_RefType:  {
			const GenericTypePtr& type = static_pointer_cast<const GenericType>(cur);
			hash_combine(seed, type->getFamilyName());
			break;
		}
		case insieme::core::NT_FunctionType: { hash_combine(seed, static_pointer_cast<const FunctionType>(cur)->isPlain()); break; }
		case insieme::core::NT_StructType:   { hash_combine(seed, "struct"); break; }
		case insieme::core::NT_UnionType:    { hash_combine(seed, "union"); break; }
		case insieme::core::NT_Identifier:   { hash_combine(seed, static_pointer_cast<const Identifier>(cur)->getName()); break; }
		case insieme::core::NT_Variable:     { hash_combine(seed, static_pointer_cast<const Variable>(cur)->getId()); break; }
		case insieme::core::NT_TypeVariable: { hash_combine(seed, static_pointer_cast<const TypeVariable>(cur)->getVarName()); break; }
		case insieme::core::NT_Literal:      { hash_combine(seed, static_pointer_cast<const Literal>(cur)->getValue()); break; }
		case insieme::core::NT_TupleProjectionExpr:   { hash_combine(seed, static_pointer_cast<const TupleProjectionExpr>(cur)->getIndex()); break; }
		case insieme::core::NT_MarkerExpr:   { hash_combine(seed, static_pointer_cast<const MarkerExpr>(cur)->getID()); break; }
		case insieme::core::NT_MarkerStmt:   { hash_combine(seed, static_pointer_cast<const MarkerStmt>(cur)->getID()); break; }
		case insieme::core::NT_Program:      { boost::hash_combine(seed, static_pointer_cast<const Program>(cur)->isMain()); break; };
		case insieme::core::NT_VariableIntTypeParam: { boost::hash_combine(seed, static_pointer_cast<const VariableIntTypeParam>(cur)->getSymbol()); break; };
		case insieme::core::NT_ConcreteIntTypeParam: { boost::hash_combine(seed, static_pointer_cast<const ConcreteIntTypeParam>(cur)->getValue()); break; };
		default: {}
	}
}

hash_t computeHashChildOnly(const NodePtr& cur) {

	// handle int-type parameters extra
//	if (IntTypeParamPtr param = dynamic_pointer_cast<const IntTypeParam>(cur)) {
//		return param->hash();
//	}

	hash_t seed = 0;
	hash_node(seed, cur);
	for_each(cur->getChildList(), [&](const NodePtr& child) {
		std::size_t res = computeHashChildOnly(child);
		EXPECT_PRED2(hasHash, *child, res);
		hash_combine(seed, res);
	});
	return seed;
}

hash_t computeHash(const NodePtr& cur) {
	return computeHashChildOnly(cur);
}


template<typename T>
NodeList toList(const vector<T>& list) {
	NodeList res;
	std::copy(list.begin(), list.end(), back_inserter(res));
	return res;
}

bool equalChildLists(const NodeList& listA, const NodeList& listB) {
	return equals(listA, listB, equal_target<insieme::core::NodePtr>());
}


template<typename NP>
void basicNodeTests(NP node, const NodeList& children = NodeList()) {

	typedef typename NP::element_type T;

	SCOPED_TRACE( typeid(NP).name() );

	// ------------ Node Ptr based tests -------------

	// check children
	EXPECT_PRED2(equalChildLists, children, node->getChildList());

	// ------------ Annotation Tests -------------

	// check for shared annotations
	NodeList listA = node->getChildList();
	NodeList listB = node->getChildList();

	EXPECT_TRUE ( equals( listA, listB ) );
	EXPECT_TRUE ( equals( listA, listB, equal_target<insieme::core::NodePtr>()) );

	// ------------ Hash Code Tests -------------

	EXPECT_PRED2(hasHash, *node, computeHash(node));

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
	const NodeList& list = clone->getChildList();
	std::for_each(list.begin(), list.end(), [&manager](const NodePtr& cur) {
		EXPECT_TRUE(manager.addressesLocal(cur));
	});

	const NodeList& list2 = clone2->getChildList();
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

	NodeManager managerA;
	NodeManager managerB;

	NP nodeInA = managerA.get(node);
	EXPECT_TRUE(nodeInA->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(nodeInA->hasAnnotation(DummyAnnotation2::DummyKey));


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

		cur->substitute(manager, mapper, false);

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

