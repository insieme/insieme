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

#include <sstream>

//#include "insieme/annotations/meta_info/effort_estimation.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/dump/binary_dump.h"

#include "include/test_info.h"

namespace insieme {
namespace annotations {

	using namespace std;
	using namespace core;
	using namespace core::dump::binary;

	TEST(InfoAnnotation, Basics) {

		NodeManager manager;
		IRBuilder builder(manager);


		auto fun = builder.parseExpr(
				"let int = int<4> in "
				"(int a, int b)->int { return a*b; }"
		);

		// build annotation
		test_info info;
		info.fun = fun;
		info.valA = 12;
		info.valB = 0.14;
		info.valC = true;

		NodePtr dummy = builder.genericType("A");

		// attach meta info
		EXPECT_FALSE(dummy.hasAttachedValue<test_info>());
		dummy.attachValue(info);
		EXPECT_TRUE(dummy.hasAttachedValue<test_info>());
		EXPECT_EQ(info, dummy.getAttachedValue<test_info>());


		// check that annotations are moved by clone
		{
			NodeManager mgr;
			auto other = mgr.get(dummy);
			EXPECT_NE(dummy, other);
			EXPECT_TRUE(other.hasAttachedValue<test_info>());

			auto info2 = other.getAttachedValue<test_info>();
			EXPECT_EQ(info, info2);

			EXPECT_TRUE(mgr.contains(info2.fun));

		}


		// check dumpable
		{
			// create a in-memory stream
			stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

			// dump IR using a binary format
			dumpIR(buffer, dummy);

			// reload IR using a different node manager
			NodeManager mgr;
			NodePtr restored = loadIR(buffer, mgr);

			EXPECT_NE(restored, dummy);
			EXPECT_EQ(*restored, *dummy);

			EXPECT_TRUE(restored.hasAttachedValue<test_info>());

			auto info2 = restored.getAttachedValue<test_info>();

			// full check
			EXPECT_EQ(info, info2);

			// details
			EXPECT_EQ(*info.fun, *info2.fun);
			EXPECT_EQ(info.valA, info2.valA);
			EXPECT_EQ(info.valB, info2.valB);
			EXPECT_EQ(info.valC, info2.valC);

			EXPECT_TRUE(mgr.contains(info2.fun));

		}

	}

	TEST(InfoAnnotation, IsMetaInfoTest) {

		NodeManager manager;
		IRBuilder builder(manager);

		// build annotation
		opencl_info info;
		info.opencl = false;

		NodePtr dummy = builder.genericType("A");
		dummy.attachValue(info);

		// also the corresponding value annotation pointer
		EXPECT_EQ(1u, dummy.getAnnotations().size());
		EXPECT_TRUE(isMetaInfo(dummy.getAnnotations().begin()->second));

	}

	TEST(InfoAnnotation, Migration) {

		NodeManager manager;
		IRBuilder builder(manager);

		// build annotation
		opencl_info info;
		info.opencl = false;

		NodePtr dummy = builder.genericType("A");
		dummy.attachValue(info);
		dummy.attachValue(123);

		// also the corresponding value annotation pointer
		EXPECT_EQ(2u, dummy.getAnnotations().size());

		NodePtr dummy2 = builder.genericType("B");

		EXPECT_EQ(2u, dummy.getAnnotations().size());
		migrateMetaInfos(dummy, dummy2);

		EXPECT_EQ(1u, dummy2.getAnnotations().size());
		EXPECT_TRUE(isMetaInfo(dummy2.getAnnotations().begin()->second));
	}

} // end namespace annotations
} // end namespace insieme
