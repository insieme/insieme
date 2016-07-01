/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/utils/properties.h"

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	// establish a value implementation for this test ...

	typedef properties::make_value_type<bool, int, string>::type Value;
	typedef properties::Property<Value>::ptr PropertyPtr;
	typedef properties::ListProperty<Value>::ptr ListPropertyPtr;

	template <typename T>
	PropertyPtr atom(const string& desc = "") {
		return properties::atom<Value, T>(desc);
	}

	template <typename... T>
	PropertyPtr tuple(const T&... params) {
		return properties::tuple<Value>(params...);
	}

	template <typename... T>
	PropertyPtr tuple(const char*& desc, const T&... params) {
		return properties::tuple<Value>(desc, params...);
	}

	template <typename... T>
	ListPropertyPtr list(const T&... params) {
		return properties::list<Value>(params...);
	}

	template <typename... T>
	ListPropertyPtr list(const char*& desc, const T&... params) {
		return properties::list<Value>(desc, params...);
	}

	Value makeValue(int value) {
		return properties::makeValue<Value>(value);
	}
	Value makeValue(string value) {
		return properties::makeValue<Value>(value);
	}

	template <typename... T>
	Value combineValues(const T&... values) {
		return properties::combineValues<Value>(values...);
	}

	template <typename T>
	T getValue(const Value& value) {
		return properties::getValue<T, Value>(value);
	}

	template <>
	Value getValue<>(const Value& value) {
		return value;
	}

	template <typename T, typename... Path>
	T getValue(const Value& value, Path... path) {
		return properties::getValue<T>(value, path...);
	}


	// -------------------- Start Tests ---------------------------------

	TEST(Property, Composition) {
		const PropertyPtr single = atom<int>();
		EXPECT_TRUE(!!single);
		EXPECT_EQ("int", toString(*single));

		const PropertyPtr pair = tuple(atom<int>(), atom<string>());
		EXPECT_TRUE(!!pair);
		EXPECT_EQ("(int,string)", toString(*pair));

		const PropertyPtr empty = tuple();
		EXPECT_TRUE(!!empty);
		EXPECT_EQ("()", toString(*empty));

		const PropertyPtr nested = tuple(atom<int>(), tuple(atom<string>()));
		EXPECT_TRUE(!!nested);
		EXPECT_EQ("(int,(string))", toString(*nested));

		const PropertyPtr intList = list(atom<int>());
		EXPECT_TRUE(!!intList);
		EXPECT_EQ("[int*]", toString(*intList));

		const PropertyPtr complex = tuple(intList, nested, list(pair));
		EXPECT_TRUE(!!complex);
		EXPECT_EQ("([int*],(int,(string)),[(int,string)*])", toString(*complex));
	}

	TEST(Property, Printer) {
		const PropertyPtr nested = tuple(atom<int>("param A"), tuple("A nested tuple", atom<string>("param B"), list("list X", atom<int>("param C"))));
		const string info = toString(properties::printInfo(nested));
		EXPECT_PRED2(containsSubString, info, "param A") << info;
		EXPECT_PRED2(containsSubString, info, "param B") << info;
		EXPECT_PRED2(containsSubString, info, "list X") << info;
	}


	TEST(Values, Composition) {
		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));

		Value b = makeValue("hello");
		EXPECT_EQ("hello", toString(b));


		Value c = combineValues(a, b);
		EXPECT_EQ("[12,hello]", toString(c));


		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,hello,[12,hello],12]", toString(d));
	}

	TEST(Values, TypeCheck) {
		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));
		EXPECT_TRUE(atom<int>()->isValid(a));
		EXPECT_FALSE(atom<string>()->isValid(a));

		Value b = makeValue("hello");
		EXPECT_EQ("hello", toString(b));
		EXPECT_TRUE(atom<string>()->isValid(b));
		EXPECT_FALSE(atom<int>()->isValid(b));


		Value c = combineValues(a, b);
		EXPECT_EQ("[12,hello]", toString(c));
		EXPECT_TRUE(tuple(atom<int>(), atom<string>())->isValid(c));
		EXPECT_FALSE(atom<int>()->isValid(c));


		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,hello,[12,hello],12]", toString(d));
		EXPECT_TRUE(tuple(atom<int>(), atom<string>(), tuple(atom<int>(), atom<string>()), atom<int>())->isValid(d));
		EXPECT_FALSE(tuple(atom<int>(), atom<string>(), tuple(atom<string>(), atom<int>()), atom<int>())->isValid(d));
		EXPECT_FALSE(atom<int>()->isValid(d));
	}


	TEST(Values, ReadValue) {
		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));
		EXPECT_EQ(12, getValue<int>(a));

		Value b = makeValue("hello");
		EXPECT_EQ("hello", toString(b));
		EXPECT_EQ("hello", getValue<string>(b));

		Value c = combineValues(a, b);
		EXPECT_EQ("[12,hello]", toString(c));
		EXPECT_EQ(12, getValue<int>(c, 0));
		EXPECT_EQ("hello", getValue<string>(c, 1));

		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,hello,[12,hello],12]", toString(d));
		EXPECT_EQ(12, getValue<int>(d, 0));
		EXPECT_EQ("hello", getValue<string>(d, 1));
		EXPECT_EQ(c, getValue<Value>(d, 2));
		EXPECT_EQ(12, getValue<int>(d, 2, 0));
		EXPECT_EQ("hello", getValue<string>(d, 2, 1));
		EXPECT_EQ(12, getValue<int>(d, 3));
	}


} // end namespace transform
} // end namespace insieme
