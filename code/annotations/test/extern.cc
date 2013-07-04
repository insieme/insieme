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

#include "insieme/annotations/c/extern.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/dump/binary_dump.h"

namespace insieme {
namespace annotations {
namespace c {

	TEST(Extern, Basic) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// just add and remove some name tagging
		auto lit = builder.stringLit("test");


		EXPECT_FALSE(isExtern(lit));

		// attach the flag
		markExtern(lit);
		EXPECT_TRUE(isExtern(lit));

		// remove it again
		markExtern(lit, false);
		EXPECT_FALSE(isExtern(lit));

	}


	TEST(Extern, Migration) {

		core::NodeManager mgrA;
		core::IRBuilder builder(mgrA);

		// just add and remove some name tagging
		auto litA = builder.stringLit("testA");
		auto litB = builder.stringLit("testB");

		EXPECT_NE(litA, litB);

		markExtern(litB);

		EXPECT_FALSE(isExtern(litA));
		EXPECT_TRUE(isExtern(litB));

		core::NodeManager mgrB;

		EXPECT_FALSE(isExtern(mgrB.get(litA)));
		EXPECT_TRUE(isExtern(mgrB.get(litB)));

	}

	TEST(Extern, Dump) {

		core::NodeManager mgrA;
		core::IRBuilder builder(mgrA);

		// just add and remove some name tagging
		auto litA = builder.stringLit("testA");
		auto litB = builder.stringLit("testB");

		EXPECT_NE(litA, litB);
		markExtern(litB);

		EXPECT_FALSE(isExtern(litA));
		EXPECT_TRUE(isExtern(litB));


		// ---- dump and restore both literals -----

		using std::ios_base;
		using std::stringstream;

		{
			// create a in-memory stream
			stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

			// dump IR using a binary format
			core::dump::binary::dumpIR(buffer, litA);

			// reload IR using a different node manager
			core::NodeManager mgr;
			core::NodePtr restored = core::dump::binary::loadIR(buffer, mgr);

			ASSERT_TRUE(restored.isa<core::LiteralPtr>());
			EXPECT_FALSE(isExtern(restored.as<core::LiteralPtr>()));

		}

		{
			// create a in-memory stream
			stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

			// dump IR using a binary format
			core::dump::binary::dumpIR(buffer, litB);

			// reload IR using a different node manager
			core::NodeManager mgr;
			core::NodePtr restored = core::dump::binary::loadIR(buffer, mgr);

			ASSERT_TRUE(restored.isa<core::LiteralPtr>());
			EXPECT_TRUE(isExtern(restored.as<core::LiteralPtr>()));

		}

	}

} // end namespace c
} // end namespace annotations
} // end namespace insieme
