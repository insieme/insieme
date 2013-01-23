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

using std::shared_ptr;

namespace insieme {
namespace utils {

namespace {

	class DummyAnnotation : public Annotation {
	public:
		static const string NAME;
		static const StringKey<DummyAnnotation> DummyKey;
		int value;
		DummyAnnotation(int value) : value(value) { };

		virtual const AnnotationKeyPtr getKey() const {
			return &DummyKey;
		}

		const std::string& getAnnotationName() const {
			 return NAME;
		}
	};

	class DummyAnnotation2 : public Annotation {
	public:
		static const string NAME;
		static const StringKey<DummyAnnotation2> DummyKey;
		int value;
		DummyAnnotation2(int value) : value(value) { };

		virtual const AnnotationKeyPtr getKey() const {
			return &DummyKey;
		}

		const std::string& getAnnotationName() const {
			 return NAME;
		}

		std::ostream& printTo(std::ostream& out) const { return out << NAME; }
	};

	// initalization of the dummy key
	const string DummyAnnotation::NAME = "DummyAnnotation";
	const StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");
	const string DummyAnnotation2::NAME = "DummyAnnotation2";
	const StringKey<DummyAnnotation2> DummyAnnotation2::DummyKey("DummyKey2");

}

TEST(Annotation, Basic) {

	typedef shared_ptr<DummyAnnotation> DummyAnnotationPtr;
	typedef shared_ptr<DummyAnnotation2> DummyAnnotation2Ptr;

	// create instance
	Annotatable<> target;

	// some basic tests
//	EXPECT_DEATH( target.addAnnotation(DummyAnnotationPtr()), ".*Cannot add NULL annotation!.*");

	// check annotations
	EXPECT_EQ ( static_cast<size_t>(0), target.getAnnotations().size() );
	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE(target.hasAnnotation(DummyAnnotation2::DummyKey));

	DummyAnnotationPtr dummyA(new DummyAnnotation(1));

	target.addAnnotation(dummyA);
	EXPECT_EQ ( static_cast<size_t>(1), target.getAnnotations().size());
	EXPECT_EQ ( dummyA->value, target.getAnnotation(DummyAnnotation::DummyKey)->value);
	EXPECT_EQ ( dummyA, target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( target.hasAnnotation(DummyAnnotation::DummyKey) );

	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.hasAnnotation(DummyAnnotation2::DummyKey));


	DummyAnnotationPtr dummyB(new DummyAnnotation(2));
	target.addAnnotation(dummyB);
	EXPECT_EQ ( static_cast<size_t>(1), target.getAnnotations().size());
	EXPECT_EQ ( dummyB->value, target.getAnnotation(DummyAnnotation::DummyKey)->value);
	EXPECT_EQ ( dummyB, target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( target.hasAnnotation(DummyAnnotation::DummyKey) );

	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.hasAnnotation(DummyAnnotation2::DummyKey));

	DummyAnnotation2Ptr dummyC(new DummyAnnotation2(123));
	target.addAnnotation(dummyC);
	EXPECT_EQ ( static_cast<size_t>(2), target.getAnnotations().size());
	EXPECT_EQ ( dummyB->value, target.getAnnotation(DummyAnnotation::DummyKey)->value);
	EXPECT_EQ ( dummyB, target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( target.hasAnnotation(DummyAnnotation::DummyKey) );

	EXPECT_EQ ( dummyC->value, target.getAnnotation(DummyAnnotation2::DummyKey)->value);
	EXPECT_EQ ( dummyC, target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( target.hasAnnotation(DummyAnnotation2::DummyKey) );

	// test removing annotation
	target.remAnnotation(DummyAnnotation::DummyKey);
	EXPECT_EQ ( static_cast<size_t>(1), target.getAnnotations().size());
	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( target.hasAnnotation(DummyAnnotation::DummyKey) );

	EXPECT_EQ ( dummyC->value, target.getAnnotation(DummyAnnotation2::DummyKey)->value);
	EXPECT_EQ ( dummyC, target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( target.hasAnnotation(DummyAnnotation2::DummyKey) );

	target.remAnnotation(DummyAnnotation2::DummyKey);
	EXPECT_EQ ( static_cast<size_t>(0), target.getAnnotations().size());
	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( target.hasAnnotation(DummyAnnotation::DummyKey) );

	EXPECT_FALSE ( target.getAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE(target.hasAnnotation(DummyAnnotation2::DummyKey));
}

TEST(Annotation, CopyTests) {

	// Just try to add an annotation to a AST node
	auto annotation = std::make_shared<DummyAnnotation>(1);

	// create first instance
	Annotatable<> a;
	a.addAnnotation(annotation);

	// test copy constructor
	Annotatable<> b(a);
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( b.hasAnnotation(DummyAnnotation::DummyKey));

	auto annotation2 = std::make_shared<DummyAnnotation2>(2);
	a.addAnnotation(annotation2);
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));

	b.remAnnotation(DummyAnnotation::DummyKey);
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));

	// test assignment
	Annotatable<> c;
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE ( c.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( c.hasAnnotation(DummyAnnotation2::DummyKey));

	c.addAnnotation(annotation);
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( c.hasAnnotation(DummyAnnotation2::DummyKey));

	// assign a to c ... (annotations should not be shared)
	c = a;
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation2::DummyKey));

	b.addAnnotation(annotation);
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation2::DummyKey));

	a.remAnnotation(DummyAnnotation::DummyKey);
	EXPECT_FALSE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation2::DummyKey));

	c.remAnnotation(DummyAnnotation2::DummyKey);
	EXPECT_FALSE ( a.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE ( a.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( b.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( b.hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE ( c.hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_FALSE ( c.hasAnnotation(DummyAnnotation2::DummyKey));
}

TEST(Annotation, ValueAnnotation) {

	// create first instance
	Annotatable<> a;

	EXPECT_FALSE(a.hasAttachedValue<int>());
	EXPECT_FALSE(a.hasAttachedValue<string>());

	a.attachValue<int>(123);

	EXPECT_TRUE(a.hasAttachedValue<int>());
	EXPECT_FALSE(a.hasAttachedValue<string>());

	a.attachValue<string>("Hello");

	EXPECT_TRUE(a.hasAttachedValue<int>());
	EXPECT_TRUE(a.hasAttachedValue<string>());

	EXPECT_EQ("Hello", a.getAttachedValue<string>());

	a.attachValue<string>("World");
	EXPECT_TRUE(a.hasAttachedValue<string>());
	EXPECT_EQ("World", a.getAttachedValue<string>());

	EXPECT_EQ(123, a.getAttachedValue<int>());

	a.detachValue<int>();

	EXPECT_FALSE(a.hasAttachedValue<int>());
	EXPECT_TRUE(a.hasAttachedValue<string>());

	a.detachValue<string>();

	EXPECT_FALSE(a.hasAttachedValue<int>());
	EXPECT_FALSE(a.hasAttachedValue<string>());

}

// a stateless class to be attached to some annotateable object.
struct TestMarker {};

TEST(Annotation, StatelessValueAnnotation) {

	// create first instance
	Annotatable<> a;

	EXPECT_FALSE(a.hasAttachedValue<TestMarker>());

	a.attachValue<TestMarker>();

	EXPECT_TRUE(a.hasAttachedValue<TestMarker>());

	a.detachValue<TestMarker>();

	EXPECT_FALSE(a.hasAttachedValue<TestMarker>());
}

} // end namespace core
} // end namespace insieme

