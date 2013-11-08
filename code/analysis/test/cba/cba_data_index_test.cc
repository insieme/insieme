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

#include "insieme/analysis/cba/framework/data_index.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace analysis {
namespace cba {


	namespace {

		using namespace core;

		template<typename T>
		void testIndexTypeConcepts() {

			// the type has to be default-constructable
			T a;
			T b;

			// copy-constructable
			T c = b;

			// assignable
			c = b;

			// instances need to be comparable
			a == b;
			a < b;

			// elements need to be hashable (for boost)
			hash_value(a);
			EXPECT_EQ(hash_value(a), hash_value(b));

			// any index has to over a cross operator
			std::map<T,int> mA, mB;
			std::set<T> x = cross(mA, mB);		// only need to compile => rest is checked by individual tests

			// and a extract operator
			extract(mA, a); // only need to compile => rest is checked by individual tests

			// and an overlap operator
			overlap(a, b);
		}


		template<typename T>
		void testArrayIndexTypeConcepts() {

			// test default properties
			testIndexTypeConcepts<T>();

			// also from a formula
			NodeManager mgr;
			IRBuilder builder(mgr);

			// .. a linear one
			arithmetic::Formula f1 = arithmetic::toFormula(builder.parseExpr("12"));
			EXPECT_TRUE(f1.isInteger());
			T b(f1);

			// .. as well as a more complex one
			arithmetic::Formula f2 = arithmetic::toFormula(builder.parseExpr("lit(\"a\":int<4>)"));
			EXPECT_FALSE(f2.isInteger());
			T c(f2);

		}

	}

	TEST(CBA, NominalIndex) {

		typedef NominalIndex<string> NominalIndex;

		testIndexTypeConcepts<NominalIndex>();

		string s1 = "a";
		string s2 = "b";

		// just a few basic tests
		NominalIndex a(s1);
		NominalIndex b(s2);

		EXPECT_EQ("a", toString(a));
		EXPECT_EQ("b", toString(b));

		NominalIndex c(s1);
		EXPECT_NE(a,b);
		EXPECT_EQ(a,c);
		EXPECT_NE(b,c);

		EXPECT_NE(a.hash(), b.hash());
		EXPECT_EQ(a.hash(), c.hash());
		EXPECT_NE(b.hash(), c.hash());


		// check operators
		std::map<NominalIndex, int> mA;
		mA[a] = 3;

		std::map<NominalIndex, int> mB;
		mB[b] = 5;

		// check the cross operator
		EXPECT_EQ("{a}", toString(cross(mA, mA)));
		EXPECT_EQ("{a,b}", toString(cross(mA, mB)));
		EXPECT_EQ("{b}", toString(cross(mB, mB)));

		// check the extract operator
		EXPECT_EQ("3", toString(extract(mA, a)));
		EXPECT_EQ("0", toString(extract(mA, b)));
		EXPECT_EQ("0", toString(extract(mB, a)));
		EXPECT_EQ("5", toString(extract(mB, b)));

	}

	TEST(CBA, UnitIndex) {

		testArrayIndexTypeConcepts<UnitIndex>();

		// just a few basic tests
		UnitIndex a;
		UnitIndex b;

		// copy-constructible
		UnitIndex c = a;
		c = b;	// and assignable

		EXPECT_EQ("*", toString(a));
		EXPECT_EQ(a,b);

		// check operators
		std::map<UnitIndex, int> mA;
		mA[a] = 3;

		std::map<UnitIndex, int> mB;

		// check the cross operator
		EXPECT_EQ("{*}", toString(cross(mA, mA)));
		EXPECT_EQ("{*}", toString(cross(mA, mB)));
		EXPECT_EQ("{}", toString(cross(mB, mB)));

		// check the extract operator
		EXPECT_EQ("3", toString(extract(mA, a)));
		EXPECT_EQ("0", toString(extract(mB, a)));
	}

	TEST(CBA, SingleIndex) {

		testArrayIndexTypeConcepts<SingleIndex>();

		// a few tests
		SingleIndex is;
		SingleIndex i1 = 1;
		SingleIndex i2 = 2;

		SingleIndex ix = i1;
		ix = i2;

		EXPECT_EQ("*", toString(is));
		EXPECT_EQ("1", toString(i1));
		EXPECT_EQ("2", toString(i2));
		EXPECT_EQ("2", toString(ix));

		EXPECT_LT(i1,i2);
		EXPECT_LT(i1,is);
		EXPECT_LT(i2,is);

		// check operators
		std::map<SingleIndex, int> mA;
		mA[i1] = 3;

		std::map<SingleIndex, int> mB;
		mB[i2] = 5;
		mB[is] = 7;

		EXPECT_EQ("{1=3}", toString(mA));
		EXPECT_EQ("{2=5, *=7}", toString(mB));

		// check the cross operator
		EXPECT_EQ("{1}", toString(cross(mA, mA)));
		EXPECT_EQ("{1,2,*}", toString(cross(mA, mB)));
		EXPECT_EQ("{2,*}", toString(cross(mB, mB)));

		// check the extract operator
		EXPECT_EQ("3", toString(extract(mA, i1)));
		EXPECT_EQ("0", toString(extract(mA, i2)));
		EXPECT_EQ("0", toString(extract(mA, is)));

		EXPECT_EQ("7", toString(extract(mB, i1)));
		EXPECT_EQ("5", toString(extract(mB, i2)));
		EXPECT_EQ("7", toString(extract(mB, is)));
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
