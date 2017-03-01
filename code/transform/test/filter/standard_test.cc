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

#include "insieme/transform/filter/standard_filter.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace transform {
namespace filter {


	TEST(TargetFilter, OutermostLoops) {
		core::NodeManager manager;

		core::IRBuilder builder(manager);

		std::map<std::string, core::NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10>,f,f,plain>"));

		auto node = builder.parseStmt("for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "}",
		                              symbols);

		EXPECT_TRUE(node);

		auto isFor = [](const core::NodeAddress& cur) { return cur->getNodeType() == core::NT_ForStmt; };

		// get outermost loop filter
		TargetFilter filter = outermostLoops();

		auto res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(core::NodeAddress(node)), res);


		// try multiple outermost for loops
		node = builder.parseStmt("{"
		                         "10;"
		                         "for(uint<4> i = 10u .. 50u) {"
		                         "	for(uint<4> j = 3u .. 25u) {"
		                         "		for(uint<4> k = 2u .. 100u) {"
		                         "			v[i+j];"
		                         "		}"
		                         "	}"
		                         "};"
		                         "12;"
		                         "for(uint<4> i = 10u .. 50u) {"
		                         "	for(uint<4> j = 3u .. 25u) {"
		                         "		for(uint<4> k = 2u .. 100u) {"
		                         "			v[i+j];"
		                         "		}"
		                         "	}"
		                         "};"
		                         "}",
		                         symbols);

		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		auto root = core::NodeAddress(node);
		EXPECT_EQ(toVector(root.getAddressOfChild(1), root.getAddressOfChild(3)), res);
	}


	TEST(TargetFilter, InnermostLoops) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		std::map<std::string, core::NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10>,f,f,plain>"));

		auto node = builder.parseStmt("{"
		                              "10;"
		                              "for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "	for(uint<4> k = 2u .. 100u) {"
		                              "		v[i+k];"
		                              "	}"
		                              "}"
		                              "12;"
		                              "for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "}"
		                              "}",
		                              symbols);

		EXPECT_TRUE(node);

		auto root = core::NodeAddress(node);
		auto isFor = [](const core::NodeAddress& cur) { return cur->getNodeType() == core::NT_ForStmt; };

		// try various innermost loop levels
		TargetFilter filter = innermostLoops();
		auto res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(root.getAddressOfChild(1, 3, 0, 3, 0), root.getAddressOfChild(1, 3, 1), root.getAddressOfChild(3, 3, 0, 3, 0)), res);

		// try various innermost loop levels
		filter = innermostLoops(2);
		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(root.getAddressOfChild(1, 3, 0), root.getAddressOfChild(3, 3, 0)), res);

		// try various innermost loop levels
		filter = innermostLoops(3);
		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(root.getAddressOfChild(1), root.getAddressOfChild(3)), res);

		// try various innermost loop levels
		filter = innermostLoops(4);
		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector<core::NodeAddress>(), res);
	}


	TEST(TargetFilter, LoopPicker) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		std::map<std::string, core::NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10>,f,f,plain>"));

		auto node = builder.parseStmt("{"
		                              "10;"
		                              "for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "	for(uint<4> k = 2u .. 100u) {"
		                              "		v[i+k];"
		                              "	}"
		                              "}"
		                              "12;"
		                              "for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "}"
		                              "}",
		                              symbols);

		auto root = core::NodeAddress(node);

		// extract addresses of loops
		auto for0 = root.getAddressOfChild(1);
		auto for00 = root.getAddressOfChild(1, 3, 0);
		auto for000 = root.getAddressOfChild(1, 3, 0, 3, 0);
		auto for01 = root.getAddressOfChild(1, 3, 1);
		auto for1 = root.getAddressOfChild(3);
		auto for10 = root.getAddressOfChild(3, 3, 0);
		auto for100 = root.getAddressOfChild(3, 3, 0, 3, 0);


		// check loop picker
		EXPECT_EQ(toVector(for0), pickLoop(0)(root));
		EXPECT_EQ(toVector(for00), pickLoop(0, 0)(root));
		EXPECT_EQ(toVector(for000), pickLoop(0, 0, 0)(root));
		EXPECT_EQ(toVector(for01), pickLoop(0, 1)(root));
		EXPECT_EQ(toVector(for1), pickLoop(1)(root));
		EXPECT_EQ(toVector(for10), pickLoop(1, 0)(root));
		EXPECT_EQ(toVector(for100), pickLoop(1, 0, 0)(root));

		EXPECT_TRUE(pickLoop(0, 0, 1)(root).empty());
		EXPECT_TRUE(pickLoop(1, 0, 1)(root).empty());
	}

	TEST(TargetFilter, AddressPicker) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		std::map<std::string, core::NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10>,f,f,plain>"));

		auto node = builder.parseStmt("{"
		                              "10;"
		                              "for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "	for(uint<4> k = 2u .. 100u) {"
		                              "		v[i+k];"
		                              "	}"
		                              "}"
		                              "12;"
		                              "for(uint<4> i = 10u .. 50u) {"
		                              "	for(uint<4> j = 3u .. 25u) {"
		                              "		for(uint<4> k = 2u .. 100u) {"
		                              "			v[i+j];"
		                              "		}"
		                              "	}"
		                              "}"
		                              "}",
		                              symbols);

		EXPECT_TRUE(node);

		auto root = core::NodeAddress(node);

		// extract addresses of loops
		auto for0 = root.getAddressOfChild(1);
		auto for00 = root.getAddressOfChild(1, 3, 0);
		auto for000 = root.getAddressOfChild(1, 3, 0, 3, 0);
		auto for01 = root.getAddressOfChild(1, 3, 1);
		auto for1 = root.getAddressOfChild(3);
		auto for10 = root.getAddressOfChild(3, 3, 0);
		auto for100 = root.getAddressOfChild(3, 3, 0, 3, 0);

		core::NodeAddress decl0 = for0.as<core::ForStmtAddress>()->getDeclaration();
		core::NodeAddress decl100 = for100.as<core::ForStmtAddress>()->getDeclaration();


		// check loop picker
		EXPECT_EQ(toVector(for0), pickRelative(for0)(root));
		EXPECT_EQ(toVector(for00), pickRelative(for00)(root));
		EXPECT_EQ(toVector(for000), pickRelative(for000)(root));
		EXPECT_EQ(toVector(for01), pickRelative(for01)(root));
		EXPECT_EQ(toVector(for1), pickRelative(for1)(root));
		EXPECT_EQ(toVector(for10), pickRelative(for10)(root));
		EXPECT_EQ(toVector(for100), pickRelative(for100)(root));
		EXPECT_EQ(toVector(decl0), pickRelative(decl0)(root));
		EXPECT_EQ(toVector(decl100), pickRelative(decl100)(root));

		// some applications which should not find anything
		EXPECT_TRUE(pickLoop(for0)(for0).empty());
		EXPECT_TRUE(pickLoop(for00)(for1).empty());
	}

} // end namespace filter
} // end namespace transform
} // end namespace insieme
