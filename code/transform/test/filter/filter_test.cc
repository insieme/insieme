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
 */

#include <gtest/gtest.h>

#include "insieme/transform/filter/filter.h"

#include "insieme/core/ir_builder.h"

#include "insieme/core/pattern/ir_pattern.h"

namespace insieme {
namespace transform {
namespace filter {

namespace p = core::pattern;
namespace irp = core::pattern::irp;


TEST(TargetFilter, Basic) {
	core::NodeManager manager;
	core::IRBuilder builder(manager);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10u>,f,f,plain>"));

	auto node = builder.parseStmt("for(uint<4> i = 10u .. 50u ) {"
	                              "	for(uint<4> j = 3u .. 25u) {"
	                              "		for(uint<4> k = 2u .. 100u) {"
	                              "			v[i+j];"
	                              "		}"
	                              "	}"
	                              "}",
	                              symbols)
	                .as<core::ForStmtPtr>();

	EXPECT_TRUE(node);

	// try tree-variable filter
	TargetFilter filter = pattern(p::var("x", irp::forStmt()), "x");

	EXPECT_EQ("all x within ($x:(ForStmt|[_]*))", toString(filter));
	EXPECT_EQ(toVector(core::NodeAddress(node)), filter(node));


	// try list-variable filter
	filter = pattern(p::rT(p::var("y", irp::forStmt(p::any, p::any, p::any, p::any, p::recurse | !p::recurse))), "y");
	EXPECT_EQ("all y within (rT.x($y:(ForStmt|(DeclarationStmt|(Declaration|_,_),_),_,_,(CompoundStmt|rec.x | !(rec.x)) | rec.x | !(rec.x))))", toString(filter));
	EXPECT_EQ(3u, filter(node).size());
}


TEST(TargetFilter, AllMatches) {
	core::NodeManager manager;

	core::IRBuilder builder(manager);
	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10u>,f,f,plain>"));

	auto node = builder.parseStmt("for(uint<4> l = 8u .. 70u) {"
	                              "	for(uint<4> i = 10u .. 50u) {"
	                              "		for(uint<4> j = 3u .. 25u) {"
	                              "			for(uint<4> k = 2u .. 100u) {"
	                              "				v[i+j];"
	                              "			}"
	                              "		}"
	                              "	}"
	                              "}",
	                              symbols);

	EXPECT_TRUE(node);

	core::NodeAddress for1(node);
	core::NodeAddress for2 = for1.getAddressOfChild(3, 0);
	core::NodeAddress for3 = for2.getAddressOfChild(3, 0);
	core::NodeAddress for4 = for3.getAddressOfChild(3, 0);

	EXPECT_EQ(core::NT_ForStmt, for1->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for2->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for3->getNodeType());
	EXPECT_EQ(core::NT_ForStmt, for4->getNodeType());

	// try find all
	TargetFilter filter = allMatches(irp::forStmt());

	EXPECT_EQ("all matching ((ForStmt|[_]*))", toString(filter));
	EXPECT_EQ(toVector(for1, for2, for3, for4), filter(node));


	// try find all (innermost 2)
	filter = allMatches(irp::forStmt(!aT(irp::forStmt())));
	EXPECT_EQ(toVector(for4), filter(node));

	// search 2 innermost loops
	filter = allMatches(irp::forStmt(p::rT(irp::forStmt(!aT(irp::forStmt())) | !(irp::forStmt() | !p::step(p::recurse)))));
	EXPECT_EQ(toVector(for3), filter(node));

	// also using the conjunction
	filter = allMatches(irp::forStmt(p::rT(irp::forStmt(!aT(irp::forStmt())) | ((!irp::forStmt()) & p::step(p::recurse)))));
	EXPECT_EQ(toVector(for3), filter(node));

	// search for 3 innermost loops
	filter = allMatches(irp::forStmt(
	    p::rT(irp::forStmt(p::rT(irp::forStmt(!aT(irp::forStmt())) | ((!irp::forStmt()) & p::step(p::recurse)))) | ((!irp::forStmt()) & p::step(p::recurse)))));
	EXPECT_EQ(toVector(for2), filter(node));
}


} // end namespace filter
} // end namespace transform
} // end namespace insieme
