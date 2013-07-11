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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "ir_dummy_annotations.inc"

using std::shared_ptr;

namespace insieme {
namespace core {


TEST(Annotation, ASTNode) {

	NodeManager manager;

	// Just try to add an annotation to a AST node
	std::shared_ptr<DummyAnnotation> annotation(new DummyAnnotation(1));

	GenericTypePtr ptr = GenericType::get(manager, "test");
	ptr->addAnnotation(annotation);

	EXPECT_TRUE ( ptr->hasAnnotation(DummyAnnotation::DummyKey));
}


TEST(Annotation, AnnotationsAndClone) {

	NodeManager manager;

	// simple migration between manager
	GenericTypePtr type = GenericType::get(manager, "A");
	type->addAnnotation(std::make_shared<DummyAnnotation>(1));
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(type->hasAnnotation(DummyAnnotation2::DummyKey));

	NodeManager manager2;
	GenericTypePtr type2 = manager2.get(type);
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(type->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(type2->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(type2->hasAnnotation(DummyAnnotation2::DummyKey));

}

TEST(Annotation, CastTest) {

	NodeManager manager;

	// create a node
	GenericTypePtr type = GenericType::get(manager, "A");
	type->addAnnotation(std::make_shared<DummyAnnotation>(1));
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(type->hasAnnotation(DummyAnnotation2::DummyKey));

	// cast up ...
	NodePtr node = type;
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(node->hasAnnotation(DummyAnnotation2::DummyKey));

	node->addAnnotation(std::make_shared<DummyAnnotation2>(12));
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation2::DummyKey));

	// dynamic pointer cast
	TypePtr type2 = dynamic_pointer_cast<const Type>(node);
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(type2->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(type2->hasAnnotation(DummyAnnotation2::DummyKey));

	type2->remAnnotation(DummyAnnotation::DummyKey);
	EXPECT_FALSE(type->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(type->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(node->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(node->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(type2->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(type2->hasAnnotation(DummyAnnotation2::DummyKey));
}

TEST(Annotation, EqualsTest) {

	NodeManager manager1;
	NodeManager manager2;

	// create a node
	TypePtr child = GenericType::get(manager1, "B");
	GenericTypePtr type1 = GenericType::get(manager1, "A", toVector(child));
	GenericTypePtr type2 = GenericType::get(manager2, "A", toVector(child));
	EXPECT_EQ(*type1, *type2);

	// add an annotation
	type1->getTypeParameter()[0]->addAnnotation(std::make_shared<DummyAnnotation>(1));

	EXPECT_TRUE(type1->getTypeParameter()[0]->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(type2->getTypeParameter()[0]->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_EQ(*type1, *type2);

	EXPECT_FALSE(equalsWithAnnotations(type1, type2));
	EXPECT_FALSE(equalsWithAnnotations(type1->getTypeParameter()[0], type2->getTypeParameter()[0]));
}

TEST(Annotation, ValueAnnotations) {

	NodeManager manager;
	TypePtr ptr = GenericType::get(manager, "A");

	EXPECT_FALSE(ptr->hasAttachedValue<int>());

	ptr->attachValue<int>(12);

	EXPECT_TRUE(ptr->hasAttachedValue<int>());
	EXPECT_EQ(12,ptr->getAttachedValue<int>());

	ptr->detachValue<int>();

	EXPECT_FALSE(ptr->hasAttachedValue<int>());

}

TEST(Annotation, ValueAnnotations2) {

	NodeManager manager;
	TypePtr ptr = GenericType::get(manager, "A");

	EXPECT_FALSE(ptr->hasAttachedValue<TypePtr>());

	ptr->attachValue<TypePtr>(ptr);

	EXPECT_TRUE(ptr->hasAttachedValue<TypePtr>());
	EXPECT_EQ(ptr,ptr->getAttachedValue<TypePtr>());

	ptr->detachValue<TypePtr>();

	EXPECT_FALSE(ptr->hasAttachedValue<TypePtr>());

}

template<typename T>
class is_printable {
		typedef char yes;
		typedef long no;

		static int consume(...);
		template <typename C> static yes test( decltype(consume(*(std::ostream*)(0) << *(T*)(0))) );
		template <typename C> static no  test( ... );

public:

		enum { value = sizeof(decltype(test<T>(0))) == sizeof(yes) };
};

TEST(Annotation, ValueAnnotationPrint) {

	NodeManager manager;
	TypePtr ptr = GenericType::get(manager, "A");

	struct unprintable {} s;

	ptr->attachValue(12);
	ptr->attachValue(string("X"));
	ptr->attachValue(s);

	EXPECT_EQ("12", toString(ptr->getAttachedValue<decltype(12)>()));
	EXPECT_EQ("X", toString(ptr->getAttachedValue<string>()));

	EXPECT_EQ("$[Value(ZN7insieme4core36Annotation_ValueAnnotationPrint_Test8TestBodyEvE11unprintable), 12, X: A]$", toString(core::printer::PrettyPrinter(ptr, core::printer::PrettyPrinter::PRINT_ANNOTATIONS)));
}


TEST(Annotation, CopyOnCloneTest) {

	struct AnnotationDefault {
		int x; AnnotationDefault(int x = 0) : x(x) {};
		bool operator==(const AnnotationDefault& other) const { return x == other.x; }
	};

	struct AnnotationDrop : public value_annotation::drop_on_clone {
		int x; AnnotationDrop(int x = 0) : x(x) {};
		bool operator==(const AnnotationDrop& other) const { return x == other.x; }
	};

	struct AnnotationMigrate : public value_annotation::cloneable {
		int x; AnnotationMigrate(int x = 0) : x(x) {};
		bool operator==(const AnnotationMigrate& other) const { return x == other.x; }
		 void cloneTo(const NodePtr& target) const {
			 target->attachValue(AnnotationMigrate(x+1));	// modify during migration
		 }
	};


	// create two managers
	NodeManager mgrA, mgrB, mgrC;

	// get a node in A
	NodePtr nodeA = GenericType::get(mgrA, "A");
	NodePtr nodeB = mgrB.get(nodeA);
	NodePtr nodeC;

	EXPECT_TRUE(nodeA);
	EXPECT_FALSE(nodeA->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeA->hasAttachedValue<AnnotationDrop>());
	EXPECT_FALSE(nodeA->hasAttachedValue<AnnotationMigrate>());

	EXPECT_TRUE(nodeB);
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDrop>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationMigrate>());

	// attach values to node
	nodeA->attachValue(AnnotationDefault(12));
	nodeA->attachValue(AnnotationDrop(22));
	nodeA->attachValue(AnnotationMigrate(32));

	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationDefault>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationDrop>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationMigrate>());

	EXPECT_EQ(12, nodeA->getAttachedValue<AnnotationDefault>().x);
	EXPECT_EQ(22, nodeA->getAttachedValue<AnnotationDrop>().x);
	EXPECT_EQ(32, nodeA->getAttachedValue<AnnotationMigrate>().x);

	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDrop>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationMigrate>());

	// migrate node to third node manager
	EXPECT_FALSE(mgrC.contains(nodeA));
	nodeC = mgrC.get(nodeA);

	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationDefault>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationDrop>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationMigrate>());

	EXPECT_EQ(12, nodeA->getAttachedValue<AnnotationDefault>().x);
	EXPECT_EQ(22, nodeA->getAttachedValue<AnnotationDrop>().x);
	EXPECT_EQ(32, nodeA->getAttachedValue<AnnotationMigrate>().x);

	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDrop>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationMigrate>());

	EXPECT_TRUE(nodeC->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeC->hasAttachedValue<AnnotationDrop>());
	EXPECT_TRUE(nodeC->hasAttachedValue<AnnotationMigrate>());

	EXPECT_EQ(12, nodeC->getAttachedValue<AnnotationDefault>().x);
	EXPECT_EQ(33, nodeC->getAttachedValue<AnnotationMigrate>().x);

}

TEST(Annotation, CopyOnMigrationTest) {

	struct AnnotationDefault {
		int x; AnnotationDefault(int x = 0) : x(x) {};
		bool operator==(const AnnotationDefault& other) const { return x == other.x; }
	};

	struct AnnotationClone : public value_annotation::copy_on_migration {
		int x; AnnotationClone(int x = 0) : x(x) {};
		bool operator==(const AnnotationClone& other) const { return x == other.x; }
	};

	struct AnnotationMigrate : public value_annotation::migratable {
		int x; AnnotationMigrate(int x = 0) : x(x) {};
		bool operator==(const AnnotationMigrate& other) const { return x == other.x; }
		bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
			after->attachValue(AnnotationMigrate(x+1)); return true;
		}
	};


	// create two managers
	NodeManager mgr;

	// get a node
	NodePtr nodeA = GenericType::get(mgr, "A");
	NodePtr nodeB = GenericType::get(mgr, "B");

	EXPECT_TRUE(nodeA);
	EXPECT_FALSE(nodeA->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeA->hasAttachedValue<AnnotationClone>());
	EXPECT_FALSE(nodeA->hasAttachedValue<AnnotationMigrate>());

	EXPECT_TRUE(nodeB);
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationClone>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationMigrate>());

	// attach values to node
	nodeA->attachValue(AnnotationDefault(12));
	nodeA->attachValue(AnnotationClone(22));
	nodeA->attachValue(AnnotationMigrate(32));

	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationDefault>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationClone>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationMigrate>());

	EXPECT_EQ(12, nodeA->getAttachedValue<AnnotationDefault>().x);
	EXPECT_EQ(22, nodeA->getAttachedValue<AnnotationClone>().x);
	EXPECT_EQ(32, nodeA->getAttachedValue<AnnotationMigrate>().x);

	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDefault>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationClone>());
	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationMigrate>());

	// migrate annotations (as done by transformation utilities)
	transform::utils::migrateAnnotations(nodeA, nodeB);

	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationDefault>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationClone>());
	EXPECT_TRUE(nodeA->hasAttachedValue<AnnotationMigrate>());

	EXPECT_EQ(12, nodeA->getAttachedValue<AnnotationDefault>().x);
	EXPECT_EQ(22, nodeA->getAttachedValue<AnnotationClone>().x);
	EXPECT_EQ(32, nodeA->getAttachedValue<AnnotationMigrate>().x);

	EXPECT_FALSE(nodeB->hasAttachedValue<AnnotationDefault>());
	EXPECT_TRUE(nodeB->hasAttachedValue<AnnotationClone>());
	EXPECT_TRUE(nodeB->hasAttachedValue<AnnotationMigrate>());

	EXPECT_EQ(22, nodeB->getAttachedValue<AnnotationClone>().x);
	EXPECT_EQ(33, nodeB->getAttachedValue<AnnotationMigrate>().x);

}


} // end namespace core
} // end namespace insieme

