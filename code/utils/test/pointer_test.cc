/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <string>
#include <iostream>

#include <gtest/gtest.h>

#include "insieme/utils/pointer.h"
#include "insieme/utils/string_utils.h"

using std::string;
using std::cout;
using std::endl;

TEST(Ptr, NullTest) {
	Ptr<int> null = Ptr<int>(NULL);

	Ptr<int> ptrN(0);
	EXPECT_TRUE(ptrN == null);
	EXPECT_TRUE(ptrN == ptrN);
	EXPECT_FALSE(ptrN);

	int a = 10;
	Ptr<int> ptrA(&a);
	EXPECT_FALSE(ptrA == null);
	EXPECT_FALSE(null == ptrA);
	EXPECT_FALSE(ptrA == ptrN);
	EXPECT_FALSE(ptrN == ptrA);
	EXPECT_TRUE(!!ptrA);

	Ptr<int> ptrA2(&a);
	EXPECT_TRUE(ptrA == ptrA2);
	EXPECT_TRUE(ptrA2 == ptrA);

	int b = 12;
	Ptr<int> ptrB(&b);
	EXPECT_FALSE(ptrB == null);
	EXPECT_FALSE(null == ptrB);
	EXPECT_FALSE(ptrB == ptrN);
	EXPECT_FALSE(ptrN == ptrB);
	EXPECT_TRUE(!!ptrB);

	EXPECT_FALSE(ptrA == ptrB);
	EXPECT_FALSE(ptrB == ptrA);
}


TEST(Ptr, Size) {
	// just ensures
	EXPECT_LE(sizeof(Ptr<int>), 2 * sizeof(int*));
}

TEST(Ptr, Print) {
	// just ensures
	int a = 10;
	Ptr<int> ptrA(&a);
	EXPECT_EQ("P(10)", toString(ptrA));

	Ptr<int> ptrN = Ptr<int>(NULL);
	EXPECT_EQ("P(NULL)", toString(ptrN));
}

typedef float real;

struct A {
	// required to be polymorphic (dynamic cast)
	virtual real hell() {
		return 66.6;
	};
};
class B : public A {};
class C : public A {};


TEST(Ptr, Casts) {
	A a;
	B b;
	C c;

	Ptr<const A> refA(&a);
	Ptr<const B> refB(&b);
	Ptr<const C> refC(&c);

	refA = refB;
	// refB = refA;
	refB = dynamic_pointer_cast<const B>(refA);
	EXPECT_FALSE(refB == Ptr<B>(NULL));

	// should not compile ...
	//	refC = dynamic_pointer_cast<const C >(refA);
	//	EXPECT_TRUE( refC == Ptr<C>(NULL) );

	refA = refC;
	refB = dynamic_pointer_cast<const B>(refA);
	EXPECT_TRUE(refB == Ptr<B>(NULL));
}

TEST(Ptr, CastShortcuts) {
	A a;
	B b;
	C c;

	Ptr<A> refA(&a);
	Ptr<B> refB(&b);
	Ptr<C> refC(&c);

	EXPECT_TRUE(refB == refB.as<Ptr<B>>());
	EXPECT_TRUE(refB == refB.as<Ptr<A>>());

	// should not compile
	//	refC.as<Ptr<B>>();

	refA = refC;
	refB = refA.isa<Ptr<B>>();
	EXPECT_FALSE(refB);
}
