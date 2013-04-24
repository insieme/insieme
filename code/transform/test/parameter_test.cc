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

#include "insieme/transform/parameter.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace transform {
namespace parameter {


	TEST(Parameter, Composition) {

		const ParameterPtr single = atom<int>();
		EXPECT_TRUE(!!single);
		EXPECT_EQ("int", toString(*single));

		const ParameterPtr pair = tuple(atom<int>(), atom<bool>());
		EXPECT_TRUE(!!pair);
		EXPECT_EQ("(int,bool)", toString(*pair));

		const ParameterPtr empty = tuple();
		EXPECT_TRUE(!!empty);
		EXPECT_EQ("()", toString(*empty));

		const ParameterPtr nested = tuple(atom<int>(), tuple(atom<bool>()));
		EXPECT_TRUE(!!nested);
		EXPECT_EQ("(int,(bool))", toString(*nested));

		const ParameterPtr intList = list(atom<int>());
		EXPECT_TRUE(!!intList);
		EXPECT_EQ("[int*]", toString(*intList));

		const ParameterPtr complex = tuple(intList,nested,list(pair));
		EXPECT_TRUE(!!complex);
		EXPECT_EQ("([int*],(int,(bool)),[(int,bool)*])", toString(*complex));
	}

	TEST(Parameter, Printer) {

		const ParameterPtr nested = tuple(atom<int>("param A"), tuple("A nested tuple", atom<bool>("param B"), list("list X", atom<int>("param C"))));
		const string info = toString(utils::properties::printInfo(nested));
		EXPECT_PRED2(containsSubString, info, "param A") << info;
		EXPECT_PRED2(containsSubString, info, "param B") << info;
		EXPECT_PRED2(containsSubString, info, "list X") << info;

	}


	TEST(Values, Composition) {

		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));

		Value b = makeValue(false);
		EXPECT_EQ("0", toString(b));


		Value c = combineValues(a,b);
		EXPECT_EQ("[12,0]", toString(c));


		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,0,[12,0],12]", toString(d));

	}

	TEST(Values, IsTypeCheck) {

		Value a = makeValue(12);
		EXPECT_TRUE(isTypeOf<int>(a));
		EXPECT_FALSE(isTypeOf<bool>(a));

		Value b = makeValue(false);
		EXPECT_FALSE(isTypeOf<int>(b));
		EXPECT_TRUE(isTypeOf<bool>(b));

		Value c = combineValues(a,b);
		EXPECT_EQ("[12,0]", toString(c));
		EXPECT_TRUE(isTypeOf<vector<Value>>(c));
		EXPECT_FALSE(isTypeOf<int>(c));

	}

	TEST(Values, TypeCheck) {

		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));
		EXPECT_TRUE(atom<int>()->isValid(a));
		EXPECT_FALSE(atom<bool>()->isValid(a));

		Value b = makeValue(false);
		EXPECT_EQ("0", toString(b));
		EXPECT_TRUE(atom<bool>()->isValid(b));
		EXPECT_FALSE(atom<int>()->isValid(b));


		Value c = combineValues(a,b);
		EXPECT_EQ("[12,0]", toString(c));
		EXPECT_TRUE(tuple(atom<int>(),atom<bool>())->isValid(c));
		EXPECT_FALSE(atom<int>()->isValid(c));


		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,0,[12,0],12]", toString(d));
		EXPECT_TRUE(tuple(atom<int>(),atom<bool>(),tuple(atom<int>(),atom<bool>()), atom<int>())->isValid(d));
		EXPECT_FALSE(tuple(atom<int>(),atom<bool>(),tuple(atom<bool>(),atom<int>()), atom<int>())->isValid(d));
		EXPECT_FALSE(atom<int>()->isValid(d));

	}


	TEST(Values, ReadValue) {

		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));
		EXPECT_EQ(12, getValue<int>(a));

		Value b = makeValue(false);
		EXPECT_EQ("0", toString(b));
		EXPECT_FALSE(getValue<bool>(b));

		Value c = combineValues(a,b);
		EXPECT_EQ("[12,0]", toString(c));
		EXPECT_EQ(12, getValue<int>(c,0));
		EXPECT_FALSE(getValue<bool>(c,1));

		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,0,[12,0],12]", toString(d));
		EXPECT_EQ(12, getValue<int>(d,0));
		EXPECT_FALSE(getValue<bool>(d,1));
		EXPECT_EQ(c, getValue<Value>(d,2));
		EXPECT_EQ(12, getValue<int>(d,2,0));
		EXPECT_FALSE(getValue<bool>(d,2,1));
		EXPECT_EQ(12, getValue<int>(d,3));

	}


	TEST(Values, EmptyTest) {

		ParameterPtr empty = parameter::no_parameters();
		EXPECT_TRUE(empty->isValid(parameter::emptyValue));

		ParameterPtr intType = atom<int>();
		EXPECT_FALSE(intType->isValid(parameter::emptyValue));

	}

} // end namespace parameter
} // end namespace transform
} // end namespace insieme


