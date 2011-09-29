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
#include "insieme/core/types.h"

#include "dummy_annotations.inc"

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

TEST(Annotation, ValueAnnotaitons) {

	NodeManager manager;
	TypePtr ptr = GenericType::get(manager, "A");

	EXPECT_FALSE(ptr->hasAttachedValue<int>());

	ptr->attachValue<int>(12);

	EXPECT_TRUE(ptr->hasAttachedValue<int>());
	EXPECT_EQ(12,ptr->getAttachedValue<int>());

	ptr->detachValue<int>();

	EXPECT_FALSE(ptr->hasAttachedValue<int>());

}

} // end namespace core
} // end namespace insieme

