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

#include <string>

#include <gtest/gtest.h>

#include "annotated_ptr.h"
#include "ast_builder.h"

using std::string;

using namespace insieme::core;

// ------------- utility classes required for the test case --------------

struct A {
	void f() {};
};
struct B : public A { };


class DummyAnnotation : public Annotation {
public:
	static StringKey<DummyAnnotation> DummyKey;
	int value;
	DummyAnnotation(int value) : value(value) { };

	virtual AnnotationKey* getKey() const {
		return &DummyKey;
	}

	const std::string getAnnotationName() const {
		 return "DummyAnnotation";
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

	const std::string getAnnotationName() const {
		 return "DummyAnnotation2";
	}
};

StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");
StringKey<DummyAnnotation2> DummyAnnotation2::DummyKey("DummyKey2");

// testing basic properties
TEST(AnnotatedPtr, Basic) {

	// FIXME: annotated pointer are getting bigger and bigger ...
	EXPECT_LE ( sizeof(AnnotatedPtr<int>) , 5*sizeof(int*) );

	int a = 10;
	int b = 15;

	// test simple creation
	AnnotatedPtr<int> refA(&a);
	EXPECT_EQ (*refA, a);

	// ... and for another element
	AnnotatedPtr<int> refB(&b);
	EXPECT_EQ (*refB, b);

	// test whether modifications are reflected
	a++;
	EXPECT_EQ (*refA, a);

}

TEST(AnnotatedPtr, UpCast) {

	// create two related instances
	A a;
	B b;

	// create references
	AnnotatedPtr<A> refA(&a);
	AnnotatedPtr<B> refB(&b);

	// make assignment (if it compiles, test passed!)
	refA = refB;
}

TEST(AnnotatedPtr, SimplePointerTest) {

	int value = 3;
	AnnotatedPtr<int> ptr(&value);

	EXPECT_EQ( 3, value);
	EXPECT_EQ( 3, *ptr);

	value = 4;
	EXPECT_EQ( 4, value);
	EXPECT_EQ( 4, *ptr);

	*ptr = 5;
	EXPECT_EQ( 5, value);
	EXPECT_EQ( 5, *ptr);
}

TEST(AnnotatedPtr, CopyAndAssignment) {
	ASTBuilder builder;

	NodePtr A = builder.genericType("A");
	A.addAnnotation(std::make_shared<DummyAnnotation>(1));
	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation::DummyKey));

	// test copy constructor
	NodePtr B(A);
	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(B.hasAnnotation(DummyAnnotation2::DummyKey));

	B.addAnnotation(std::make_shared<DummyAnnotation2>(123));
	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation2::DummyKey));

	EXPECT_EQ(&A.getAnnotations(), &B.getAnnotations());

	// test assignment
	A = builder.genericType("C");
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation2::DummyKey));

	EXPECT_NE(&A.getAnnotations(), &B.getAnnotations());

	B.remAnnotation(DummyAnnotation2::DummyKey);
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(B.hasAnnotation(DummyAnnotation2::DummyKey));

	A.addAnnotation(std::make_shared<DummyAnnotation2>(1));
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(B.hasAnnotation(DummyAnnotation2::DummyKey));


	NodePtr C = A;
	EXPECT_FALSE(A.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(B.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(B.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(C.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(C.hasAnnotation(DummyAnnotation2::DummyKey));

	EXPECT_NE(&A.getAnnotations(), &B.getAnnotations());
	EXPECT_EQ(&A.getAnnotations(), &C.getAnnotations());
	EXPECT_NE(&C.getAnnotations(), &B.getAnnotations());

	// finally, assign A := B
	A = B;

	EXPECT_EQ(&A.getAnnotations(), &B.getAnnotations());
	EXPECT_NE(&A.getAnnotations(), &C.getAnnotations());
	EXPECT_NE(&C.getAnnotations(), &B.getAnnotations());

}

TEST(AnnotatedPtr, AnnotationPreservation) {
	ASTBuilder builder;

	GenericTypePtr A = builder.genericType("A");
	A.addAnnotation(std::make_shared<DummyAnnotation2>(1));
	GenericTypePtr B = builder.genericType("B",toVector<TypePtr>(), toVector<IntTypeParam>(), A);

	A.addAnnotation(std::make_shared<DummyAnnotation>(12));

//	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation::DummyKey));
//	EXPECT_FALSE(B->getBaseType().hasAnnotation(DummyAnnotation::DummyKey));
//
//	EXPECT_TRUE(A.hasAnnotation(DummyAnnotation2::DummyKey));
//	EXPECT_TRUE(B->getBaseType().hasAnnotation(DummyAnnotation2::DummyKey));
}
