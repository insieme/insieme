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

	NodePtr code = builder.parseStmt(
		"{ "
		"	for(uint<4> i = 10u .. 50u) { "
		"		v[i]; "
		"	} "
		"	for(uint<4> j = 5u .. 25u) { "
		"		v[j]; "
		"	} "
		"}", symbols);

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

	NodePtr code = builder.parseStmt(
		"{ "
		"	for(uint<4> i = 10u .. 50u) { "
		"		v[i]; "
		"	} "
		"	for(uint<4> j = 5u .. 25u) { "
		"		v[j]; "
		"	} "
		"}", symbols);

	EXPECT_TRUE(code) << *code;

	// create a in-memory stream
	stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

	NodeAddress adr(code);
	adr = adr.getAddressOfChild(1,3);

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

