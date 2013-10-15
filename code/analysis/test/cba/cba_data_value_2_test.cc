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

#include "insieme/analysis/cba/framework/data_value_2.h"
#include "insieme/analysis/cba/framework/data_index.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace std;
	using namespace core;

	TEST(DataValue, AtomData) {

		// just some simple data path handling

		typedef Data<int> Data;

		Data a = atomic(1);
		Data b = atomic(2);
		Data c = atomic(0);

		EXPECT_EQ("1", toString(a));
		EXPECT_EQ("2", toString(b));
		EXPECT_EQ("0", toString(c));

		EXPECT_EQ(a, atomic(1));
		EXPECT_NE(a, atomic(2));
	}

	TEST(DataValue, CompositeData) {

		// just some simple data path handling

		typedef Data<int> Data;

		auto e0 = atomic(0);
		auto e1 = atomic(1);
		auto e2 = atomic(2);


		// test with the unit index
		auto elementUA = element(UnitIndex(), toSet(e0,e1));
		Data aU = compositeData(elementUA);
		EXPECT_EQ("{[*]={0,1}}", toString(aU));


		// test with the single index
		auto elementSA = element(SingleIndex(0), toSet(e0));
		auto elementSB = element(SingleIndex(1), toSet(e1,e2));
		auto elementSC = element(SingleIndex(), toSet(e2));

		Data aS = compositeData(elementSA, elementSB, elementSC);
		EXPECT_EQ("{[0]={0},[1]={1,2},[*]={2}}", toString(aS));

	}

	TEST(DataValue, DataSet) {

		auto e0 = atomic(0);
		auto e1 = atomic(1);
		auto e2 = atomic(2);

		// test an empty set
		auto se = Data<int>();
		EXPECT_EQ("{}", toString(se));

		// create a simple set
		auto s1 = toSet(e1,e0,e2,e0);
		EXPECT_EQ("{0,1,2}", toString(s1));

		// and another containing variations of arrays

		auto s2 = toSet(
			compositeData(element(UnitIndex(), toSet(e0,e2))),
			compositeData(element(UnitIndex(), toSet(e1)))
		);

		EXPECT_EQ("{{[*]={0,2}},{[*]={1}}}", toString(s2));
	}

//	TEST(DataValue, Union) {
//
//		auto e0 = atomic(0);
//		auto e1 = atomic(1);
//		auto e2 = atomic(2);
//
//		EXPECT_EQ("0", toString(e0));
//		EXPECT_EQ("{0,1}", toString(e0 + e1));
//		EXPECT_EQ("", toString(e0 + e1 + e2));
//		EXPECT_EQ("", toString(e0 + e1 + e2 + e2));
//
//	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
