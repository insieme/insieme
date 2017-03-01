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
#include <gtest/gtest.h>

#include "insieme/core/dump/text_dump.h"

#include <sstream>

#include "insieme/core/ir_builder.h"

using std::shared_ptr;

namespace insieme {
namespace core {
namespace dump {

	using namespace std;


	TEST(TextDump, StoreLoad) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

		NodePtr code = builder.parseStmt("{ "
		                                 "	for(uint<4> i = 10u .. 50u) { "
		                                 "		v[i]; "
		                                 "	} "
		                                 "	for(uint<4> j = 5u .. 25u) { "
		                                 "		v[j]; "
		                                 "	} "
		                                 "}",
		                                 symbols);

		EXPECT_TRUE(code) << *code;

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a text format
		text::dumpIR(buffer, code);

		// reload IR using a different node manager
		NodeManager managerB;
		NodePtr restored = text::loadIR(buffer, managerB);

		EXPECT_NE(code, restored);
		EXPECT_EQ(*code, *restored);

		buffer.seekg(0); // reset stream

		NodePtr restored2 = text::loadIR(buffer, managerA);
		EXPECT_EQ(code, restored2);
	}

	TEST(TextDump, StoreLoadAddress) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));

		NodePtr code = builder.parseStmt("{ "
		                                 "	for(uint<4> i = 10u .. 50u) { "
		                                 "		v[i]; "
		                                 "	} "
		                                 "	for(uint<4> j = 5u .. 25u) { "
		                                 "		v[j]; "
		                                 "	} "
		                                 "}",
		                                 symbols);

		EXPECT_TRUE(code) << *code;

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		NodeAddress adr(code);
		adr = adr.getAddressOfChild(1, 3);

		// dump IR using a binary format
		text::dumpAddress(buffer, adr);

		// reload IR using a different node manager
		NodeManager managerB;
		NodeAddress restored = text::loadAddress(buffer, managerB);

		EXPECT_EQ(adr, restored);
		EXPECT_NE(adr.getAddressedNode(), restored.getAddressedNode());
		EXPECT_EQ(*adr, *restored);
		EXPECT_EQ(*adr.getRootNode(), *restored.getRootNode());

		buffer.seekg(0); // reset stream

		NodePtr restored2 = text::loadAddress(buffer, managerA);
		EXPECT_EQ(adr, restored2);
	}


} // end namespace dump
} // end namespace core
} // end namespace insieme
