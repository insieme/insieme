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

#include "annotation.h"
#include "types.h"

using std::shared_ptr;

namespace insieme {
namespace core {


class DummyAnnotation : public Annotation {
public:
	static StringKey<DummyAnnotation> DummyKey;
	int value;
	DummyAnnotation(int value) : value(value) { };

	virtual AnnotationKey* getKey() const {
		return &DummyKey;
	}
};

class DummyAnnotation2 : public Annotation {
public:
	static StringKey<DummyAnnotation2> DummyKey;
	int value;
	DummyAnnotation2(int value) : value(value) { };

	virtual AnnotationKey* getKey() const {
		return &DummyKey;
	}
};

// initalization of the dummy key
StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");
StringKey<DummyAnnotation2> DummyAnnotation2::DummyKey("DummyKey2");


TEST(Annotation, Basic) {

	typedef shared_ptr<DummyAnnotation> DummyAnnotationPtr;
	typedef shared_ptr<DummyAnnotation2> DummyAnnotation2Ptr;

	// create instance
	Annotatable target;

	// some basic tests
//	EXPECT_DEATH( target.addAnnotation(DummyAnnotationPtr()), ".*Cannot add NULL annotation!.*");

	// check annotations
	EXPECT_EQ ( 0, target.getAnnotations().size() );
	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.contains(DummyAnnotation::DummyKey));
	EXPECT_FALSE(target.contains(DummyAnnotation2::DummyKey));

	DummyAnnotationPtr dummyA(new DummyAnnotation(1));

	target.addAnnotation(dummyA);
	EXPECT_EQ ( 1, target.getAnnotations().size());
	EXPECT_EQ ( dummyA->value, target.getAnnotation(DummyAnnotation::DummyKey)->value);
	EXPECT_EQ ( &*dummyA, target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( target.contains(DummyAnnotation::DummyKey) );

	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.contains(DummyAnnotation2::DummyKey));


	DummyAnnotationPtr dummyB(new DummyAnnotation(2));
	target.addAnnotation(dummyB);
	EXPECT_EQ ( 1, target.getAnnotations().size());
	EXPECT_EQ ( dummyB->value, target.getAnnotation(DummyAnnotation::DummyKey)->value);
	EXPECT_EQ ( &*dummyB, target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( target.contains(DummyAnnotation::DummyKey) );

	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.contains(DummyAnnotation2::DummyKey));

	DummyAnnotation2Ptr dummyC(new DummyAnnotation2(123));
	target.addAnnotation(dummyC);
	EXPECT_EQ ( 2, target.getAnnotations().size());
	EXPECT_EQ ( dummyB->value, target.getAnnotation(DummyAnnotation::DummyKey)->value);
	EXPECT_EQ ( &*dummyB, target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( target.contains(DummyAnnotation::DummyKey) );

	EXPECT_EQ ( dummyC->value, target.getAnnotation(DummyAnnotation2::DummyKey)->value);
	EXPECT_EQ ( &*dummyC, target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( target.contains(DummyAnnotation2::DummyKey) );

	// test removing annotation
	target.remAnnotation(DummyAnnotation::DummyKey);
	EXPECT_EQ ( 1, target.getAnnotations().size());
	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( target.contains(DummyAnnotation::DummyKey) );

	EXPECT_EQ ( dummyC->value, target.getAnnotation(DummyAnnotation2::DummyKey)->value);
	EXPECT_EQ ( &*dummyC, target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( target.contains(DummyAnnotation2::DummyKey) );

	target.remAnnotation(DummyAnnotation2::DummyKey);
	EXPECT_EQ ( 0, target.getAnnotations().size());
	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( target.contains(DummyAnnotation::DummyKey) );

	EXPECT_TRUE ( NULL == target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.contains(DummyAnnotation2::DummyKey));
}

TEST(Annotation, ASTNode) {

	NodeManager manager;

	// Just try to add an annotation to a AST node
	std::shared_ptr<DummyAnnotation> annotation(new DummyAnnotation(1));

	GenericTypePtr ptr = GenericType::get(manager, "test");
	ptr->addAnnotation(annotation);

}

} // end namespace core
} // end namespace insieme

