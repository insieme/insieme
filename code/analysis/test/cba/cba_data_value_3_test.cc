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

#include "insieme/analysis/cba/framework/data_value_3.h"
#include "insieme/analysis/cba/framework/data_index.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace std;
	using namespace core;


	TEST(DataValue, AtomicElements) {

		typedef Element<int> Element;
		EXPECT_EQ(sizeof(int*), sizeof(Element));

		DataManager<int> mgr;

		Element a = mgr.atomic(1);
		Element b = mgr.atomic(2);
		Element c = mgr.atomic(3);

		EXPECT_EQ("1", toString(a));
		EXPECT_EQ("2", toString(b));
		EXPECT_EQ("3", toString(c));

		EXPECT_EQ(a, mgr.atomic(1));
		EXPECT_EQ(b, mgr.atomic(2));
		EXPECT_EQ(c, mgr.atomic(3));

		EXPECT_EQ(a.getPtr(), mgr.atomic(1).getPtr());
		EXPECT_EQ(b.getPtr(), mgr.atomic(2).getPtr());
		EXPECT_EQ(c.getPtr(), mgr.atomic(3).getPtr());
	}

	TEST(DataValue, DataSet) {

		typedef Data<int> Data;
		EXPECT_EQ(sizeof(int*), sizeof(Data));

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);

		EXPECT_EQ("{0}", toString(Data(e0)));
		EXPECT_EQ("{1}", toString(Data(e1)));

		EXPECT_EQ("{}", toString(mgr.set()));
		EXPECT_EQ("{0}", toString(mgr.set(e0)));
		EXPECT_EQ("{1}", toString(mgr.set(e1)));
		EXPECT_EQ("{0,1}", toString(mgr.set(e0,e1)));
		EXPECT_EQ("{0,1}", toString(mgr.set(e0,e1,e0)));

		// check pointer-equality
		EXPECT_EQ(mgr.set(e0,e1).getPtr(), mgr.set(e1,e0).getPtr());

	}


	TEST(DataValue, CompoundElements) {

		typedef Data<int> Data;
		typedef Element<int> Element;

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		std::map<UnitIndex, Data> m1;
		m1[UnitIndex()] = e1;

		Element a = mgr.compound(m1);
		Element b = mgr.compound(m1);

		EXPECT_EQ(a,b);
		EXPECT_EQ(a.getPtr(), b.getPtr());

		EXPECT_EQ("[*={1}]", toString(a));
		EXPECT_EQ("[*={1}]", toString(b));

		std::map<SingleIndex, Data> m2;
		m2[1] = e1;
		m2[2] = mgr.set(e0,e2);
		m2[SingleIndex()] = e2;

		Element c = mgr.compound(m2);
		EXPECT_EQ("[1={1},2={0,2},*={2}]", toString(c));
		EXPECT_NE(a,c);
		EXPECT_NE(b,c);


		Element d = mgr.compound(
				entry(NominalIndex("a"), mgr.set(e1,e2)),
				entry(NominalIndex("b"), mgr.set(e0,e2))
		);

		EXPECT_EQ("[a={1,2},b={0,2}]", toString(d));
	}


	TEST(DataValue, Merge) {

		typedef Data<int> Data;

		DataManager<int> mgr;

		auto e0 = mgr.atomic(0);
		auto e1 = mgr.atomic(1);
		auto e2 = mgr.atomic(2);

		Data s1;
		Data s2 = mgr.set(e0);
		Data s3 = mgr.set(e1);
		Data s4 = mgr.set(e2);
		Data s5 = mgr.set(e0,e2);

		EXPECT_EQ("{}", toString(setUnion(s1,s1)));
		EXPECT_EQ("{0}", toString(setUnion(s1,s2)));
		EXPECT_EQ("{1}", toString(setUnion(s1,s3)));
		EXPECT_EQ("{0,1}", toString(setUnion(s2,s3)));
		EXPECT_EQ("{1,2}", toString(setUnion(s3,s4)));
		EXPECT_EQ("{0,2}", toString(setUnion(s2,s5)));
		EXPECT_EQ("{0,1,2}", toString(setUnion(s3,s5)));

	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
