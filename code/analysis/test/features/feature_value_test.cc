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

#include "insieme/analysis/features/feature_value.h"

namespace insieme {
namespace analysis {
namespace features {


	TEST(Type, Composition) {

		const TypePtr single = atom<int>();
		EXPECT_TRUE(!!single);
		EXPECT_EQ("int", toString(*single));

		const TypePtr pair = tuple(atom<int>(), atom<double>());
		EXPECT_TRUE(!!pair);
		EXPECT_EQ("(int,double)", toString(*pair));

		const TypePtr empty = tuple();
		EXPECT_TRUE(!!empty);
		EXPECT_EQ("()", toString(*empty));

		const TypePtr nested = tuple(atom<int>(), tuple(atom<double>()));
		EXPECT_TRUE(!!nested);
		EXPECT_EQ("(int,(double))", toString(*nested));

		const TypePtr intList = list(atom<int>());
		EXPECT_TRUE(!!intList);
		EXPECT_EQ("[int*]", toString(*intList));

		const TypePtr complex = tuple(intList,nested,list(pair));
		EXPECT_TRUE(!!complex);
		EXPECT_EQ("([int*],(int,(double)),[(int,double)*])", toString(*complex));
	}

	TEST(Values, TypeCheck) {

		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));
		EXPECT_TRUE(atom<int>()->isValid(a));
		EXPECT_FALSE(atom<double>()->isValid(a));

		Value b = makeValue(10.2);
		EXPECT_EQ("10.2", toString(b));
		EXPECT_TRUE(atom<double>()->isValid(b));
		EXPECT_FALSE(atom<int>()->isValid(b));


		Value c = combineValues(a,b);
		EXPECT_EQ("[12,10.2]", toString(c));
		EXPECT_TRUE(tuple(atom<int>(),atom<double>())->isValid(c));
		EXPECT_FALSE(atom<int>()->isValid(c));


		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,10.2,[12,10.2],12]", toString(d));
		EXPECT_TRUE(tuple(atom<int>(),atom<double>(),tuple(atom<int>(),atom<double>()), atom<int>())->isValid(d));
		EXPECT_FALSE(tuple(atom<int>(),atom<double>(),tuple(atom<double>(),atom<int>()), atom<int>())->isValid(d));
		EXPECT_FALSE(atom<int>()->isValid(d));

	}

	TEST(Values, ReadValue) {

		Value a = makeValue(12);
		EXPECT_EQ("12", toString(a));
		EXPECT_EQ(12, getValue<int>(a));

		Value b = makeValue(10.2);
		EXPECT_EQ("10.2", toString(b));
		EXPECT_DOUBLE_EQ(10.2, getValue<double>(b));

		Value c = combineValues(a,b);
		EXPECT_EQ("[12,10.2]", toString(c));
		EXPECT_EQ(12, getValue<int>(c,0));
		EXPECT_DOUBLE_EQ(10.2, getValue<double>(c,1));

		Value d = combineValues(a, b, c, a);
		EXPECT_EQ("[12,10.2,[12,10.2],12]", toString(d));
		EXPECT_EQ(12, getValue<int>(d,0));
		EXPECT_DOUBLE_EQ(10.2, getValue<double>(d,1));
		EXPECT_EQ(c, getValue<Value>(d,2));
		EXPECT_EQ(12, getValue<int>(d,2,0));
		EXPECT_DOUBLE_EQ(10.2, getValue<double>(d,2,1));
		EXPECT_EQ(12, getValue<int>(d,3));

	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
