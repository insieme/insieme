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

#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/logging.h"

#ifndef TEST
// avoiding warnings in eclipse, enabling auto completions
#define TEST(...) void fun
#endif

using namespace insieme::utils::log;

namespace insieme {
namespace core {
namespace pattern {

	bool isMatch(const TreePattern& pattern, const NodePtr& node) {
		return pattern.match(node);
	}

	bool noMatch(const TreePattern& pattern, const NodePtr& node) {
		return !isMatch(pattern, node);
	}

	template <typename T>
	vector<T> filterNull(const vector<T>& list) {
		vector<T> res;
		for(auto cur : list) {
			if(cur) { res.push_back(cur); }
		}
		return res;
	}

	TEST(PatternUtils, Basic) {
		core::NodeManager manager;
		IRBuilder builder(manager);

		std::map<std::string, core::NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1u>>"));

		core::NodePtr node = builder.parseStmt(R"(
		for(uint<4> l = 8u .. 70u : 3u) {
			l;
			for(uint<4> i = 10u .. 50u) {
				for(uint<4> j = 3u .. 25u) {
					j;
					for(uint<4> k = 2u .. 100u) {
						v[i+j];
					};
		            var ref<uint<4>> a = 3u;
					a = i;
				}
			}
			for(uint<4> k = 2u .. 100u) {
				v[l+k];
			}
        }
		)",
		                                       symbols);

		EXPECT_TRUE(node);

		core::NodeAddress root(node);

		core::NodeAddress for1(node);
		core::NodeAddress for2 = for1.getAddressOfChild(3, 1);
		core::NodeAddress for3 = for2.getAddressOfChild(3, 0);
		core::NodeAddress for4 = for3.getAddressOfChild(3, 1);
		core::NodeAddress for5 = for1.getAddressOfChild(3, 2);
		auto allFors = toVector(for1, for2, for3, for4, for5);

		EXPECT_EQ(core::NT_ForStmt, for1->getNodeType());
		EXPECT_EQ(core::NT_ForStmt, for2->getNodeType());
		EXPECT_EQ(core::NT_ForStmt, for3->getNodeType());
		EXPECT_EQ(core::NT_ForStmt, for4->getNodeType());
		EXPECT_EQ(core::NT_ForStmt, for5->getNodeType());

		// collect all
		TreePattern pattern = irp::forStmt();
		EXPECT_EQ(allFors, irp::collectAll(pattern, root));
		pattern = irp::whileStmt();
		EXPECT_TRUE(irp::collectAll(pattern, root).empty());

		// match all
		pattern = irp::forStmt();
		std::vector<core::NodePtr> results;
		irp::matchAll(pattern, root, [&](AddressMatch m) { results.push_back(m.getRoot().getAddressedNode()); });
		EXPECT_EQ(toVector(for1.getAddressedNode(), for2.getAddressedNode(), for3.getAddressedNode(), for4.getAddressedNode(), for5.getAddressedNode()),
		          results);

		pattern = irp::whileStmt();
		results.clear();
		irp::matchAll(pattern, root, [&](AddressMatch m) { results.push_back(m.getRoot()); });
		EXPECT_TRUE(results.empty());

		// replace all
		auto insertStmt = builder.declarationStmt(builder.intLit(5));
		pattern = irp::forStmt();
		auto result = irp::replaceAll(pattern, root, [&](AddressMatch m) {
			return builder.compoundStmt(toVector<StatementPtr>(insertStmt, m.getRoot().getAddressedNode().as<StatementPtr>()));
		});
		pattern = irp::declarationStmt();
		// check if we now have one more declarationStmt per for stmt
		EXPECT_EQ(irp::collectAll(pattern, root).size() + allFors.size(), irp::collectAll(pattern, result).size());
	}


	TEST(PatternUtils, Performance) {
		core::NodeManager manager;
		IRBuilder builder(manager);

		core::StatementPtr node = builder.parseStmt("for(uint<4> k = 2u .. 100u) { "
		                                            "	var ref<uint<4>> a = 3u;"
		                                            "} ");

		EXPECT_TRUE(node);

		StatementList l(100, node);
		auto compound = builder.compoundStmt(l);

		auto pattern = aT((!irp::forStmt()) & irp::declarationStmt());

		unsigned matches = 0;
		auto result = irp::replaceAll(pattern, compound, [&](AddressMatch m) {
			matches++;
			return m.getRoot();
		}, true);
		EXPECT_EQ(401, matches);
	}
}
}
}
