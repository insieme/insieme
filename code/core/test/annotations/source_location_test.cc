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
#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/dump/binary_dump.h"

namespace insieme {
namespace core {
namespace annotations {

	TEST(TextPosition, Basic) {
		TextPosition a(10, 3);
		TextPosition b(10, 5);
		TextPosition c(11, 1);
		TextPosition d(11, 6);

		// to string should work
		EXPECT_EQ("10:3", toString(a));

		// also check equality
		EXPECT_EQ(a, a);
		EXPECT_NE(a, b);

		// and the less-than implementation
		EXPECT_LT(a, b);
		EXPECT_LT(a, c);
		EXPECT_LT(a, d);

		EXPECT_GT(b, a);
		EXPECT_GT(c, b);
	}

	TEST(Location, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TextPosition a(10, 3);
		TextPosition b(10, 5);

		NodePtr node = builder.compoundStmt();

		EXPECT_FALSE(hasAttachedLocation(node));

		// attach a location
		attachLocation(node, "test.txt", a, b);

		EXPECT_TRUE(hasAttachedLocation(node));

		// retrieve the location
		EXPECT_EQ("test.txt@10:3-10:5", toString(getAttachedLocation(node)));


		// attach the same location a second time
		attachLocation(node, "test.txt", a, b);

		// should not have changed anything
		EXPECT_TRUE(hasAttachedLocation(node));
		EXPECT_EQ("test.txt@10:3-10:5", toString(getAttachedLocation(node)));


		// attach a different location
		attachLocation(node, "test.txt", b, b);

		// should not have changed anything
		EXPECT_TRUE(hasAttachedLocation(node));
		EXPECT_EQ("-shared node-", toString(getAttachedLocation(node)));
	}

	TEST(Location, Migration) {
		core::NodeManager mgrA;
		core::IRBuilder builder(mgrA);

		TextPosition pos(10, 5);

		// just add and remove some name tagging
		auto litA = builder.stringLit("testA");
		auto litB = builder.stringLit("testB");
		auto litC = builder.stringLit("testC");


		EXPECT_NE(litA, litB);

		attachLocation(litB, "test.txt", pos);
		attachLocation(litC, Location::getShared());

		EXPECT_FALSE(hasAttachedLocation(litA));
		EXPECT_TRUE(hasAttachedLocation(litB));
		EXPECT_TRUE(hasAttachedLocation(litC));

		EXPECT_FALSE(getAttachedLocation(litB).isShared());
		EXPECT_TRUE(getAttachedLocation(litC).isShared());

		core::NodeManager mgrB;

		EXPECT_FALSE(hasAttachedLocation(mgrB.get(litA)));
		EXPECT_TRUE(hasAttachedLocation(mgrB.get(litB)));
		EXPECT_TRUE(hasAttachedLocation(mgrB.get(litC)));

		EXPECT_EQ(getAttachedLocation(litB), getAttachedLocation(mgrB.get(litB)));
		EXPECT_EQ(getAttachedLocation(litC), getAttachedLocation(mgrB.get(litC)));

		EXPECT_FALSE(getAttachedLocation(mgrB.get(litB)).isShared());
		EXPECT_TRUE(getAttachedLocation(mgrB.get(litC)).isShared());
	}

	TEST(Location, Clone) {
		core::NodeManager mgrA;
		core::IRBuilder builder(mgrA);

		TextPosition pos(10, 5);

		// just add and remove some name tagging
		auto litA = builder.stringLit("testA");

		attachLocation(litA, "test.txt", pos);

		EXPECT_TRUE(hasAttachedLocation(litA));
		EXPECT_FALSE(getAttachedLocation(litA).isShared());
		EXPECT_TRUE(mgrA.addressesLocal(getAttachedLocation(litA).getFileValue()));

		// clone annotation to another manager
		core::NodeManager mgrB;
		auto litB = mgrB.get(litA);
		EXPECT_EQ(getAttachedLocation(litA), getAttachedLocation(litB));

		EXPECT_TRUE(hasAttachedLocation(litB));
		EXPECT_FALSE(getAttachedLocation(litB).isShared());
		EXPECT_TRUE(mgrB.addressesLocal(getAttachedLocation(litB).getFileValue()));

		EXPECT_FALSE(mgrA.addressesLocal(getAttachedLocation(litB).getFileValue()));
		EXPECT_FALSE(mgrB.addressesLocal(getAttachedLocation(litA).getFileValue()));
	}

	TEST(Location, Dump) {
		core::NodeManager mgrA;
		core::IRBuilder builder(mgrA);

		TextPosition pos(10, 5);

		// just add and remove some name tagging
		auto litA = builder.stringLit("testA");
		auto litB = builder.stringLit("testB");
		auto litC = builder.stringLit("testC");


		EXPECT_NE(litA, litB);
		EXPECT_NE(litA, litC);
		EXPECT_NE(litB, litC);

		attachLocation(litB, "test.txt", pos);
		attachLocation(litC, Location::getShared());

		EXPECT_FALSE(hasAttachedLocation(litA));
		EXPECT_TRUE(hasAttachedLocation(litB));
		EXPECT_TRUE(hasAttachedLocation(litC));

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
			EXPECT_FALSE(hasAttachedLocation(restored.as<core::LiteralPtr>()));
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
			EXPECT_TRUE(hasAttachedLocation(restored.as<core::LiteralPtr>()));
			EXPECT_EQ(getAttachedLocation(litB), getAttachedLocation(restored));
			EXPECT_TRUE(mgr.addressesLocal(getAttachedLocation(restored).getFileValue()));
		}

		{
			// create a in-memory stream
			stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

			// dump IR using a binary format
			core::dump::binary::dumpIR(buffer, litC);

			// reload IR using a different node manager
			core::NodeManager mgr;
			core::NodePtr restored = core::dump::binary::loadIR(buffer, mgr);

			ASSERT_TRUE(restored.isa<core::LiteralPtr>());
			EXPECT_TRUE(hasAttachedLocation(restored.as<core::LiteralPtr>()));
			EXPECT_EQ(getAttachedLocation(litC), getAttachedLocation(restored));
		}
	}


	TEST(Location, LocationDeduction) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt("{"
		                            "	1;" // this should be shared => unknown location, but approximated
		                            "	2;" // this should be distinct
		                            "	1;" // cause some sharing
		                            "}")
		              .as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		// mark locations
		attachLocation(code, "test.txt", TextPosition(0, 0), TextPosition(4, 0));
		attachLocation(code[0], "test.txt", 1, 4);
		attachLocation(code[1], "test.txt", 2, 4);
		attachLocation(code[2], "test.txt", 3, 4);

		EXPECT_TRUE(getLocation(code));
		EXPECT_TRUE(getLocation(code[0]));
		EXPECT_TRUE(getLocation(code[1]));
		EXPECT_TRUE(getLocation(code[2]));

		EXPECT_EQ("test.txt@0:0-4:0", toString(*getLocation(code)));
		EXPECT_EQ("test.txt@0:0-2:4", toString(*getLocation(code[0])));
		EXPECT_EQ("test.txt@2:4", toString(*getLocation(code[1])));
		EXPECT_EQ("test.txt@2:4-4:0", toString(*getLocation(code[2])));
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme
