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

#include "insieme/core/ir_builder.h"

#include "insieme/core/pattern/match.h"
#include "insieme/core/pattern/generator.h"

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/ir_generator.h"

namespace insieme {
namespace core {
namespace pattern {

	using namespace core;

	namespace p = pattern;
	namespace g = pattern::generator;
	namespace irg = pattern::generator::irg;


	TEST(Generator, Atom) {
		TreePtr a = makeTree('a');

		TreeGenerator gen;
		Match<tree_target> match;

		gen = g::atom(a);

		EXPECT_EQ(a, g::impl::generate(gen, match));
	}

	TEST(Generator, GenerateCode) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto ps = [&manager](string str) { return IRBuilder(manager).parseStmt(str); };

		StatementPtr stmt = ps("for(int<4> i = 0 .. 2) { "
		                       "	for(int<4> j = 1 .. 3){ "
		                       "		7; "
		                       "		6; "
		                       "		continue; "
		                       "		8;"
		                       " 	} "
		                       "}");

		LiteralPtr one = builder.intLit(1);

		TreePattern pattern = irp::forStmt(p::var("i"), p::var("s1"), p::var("e1"), p::atom(one),
		                                   irp::forStmt(var("j"), p::var("s2"), p::var("e2"), p::atom(one), *p::var("b", p::any)));

		auto match = pattern.matchPointer(stmt);

		TreeGenerator generator = irg::forStmt(irg::int4(), g::var("j"), g::var("s2"), g::var("e2"), g::atom(one),
		                                       irg::forStmt(irg::int4(), g::var("i"), g::var("s1"), g::var("e1"), g::atom(one), g::listVar("b")));

		NodePtr res = generator.generate(*match);

		// switch again
		NodePtr final = generator.generate(*pattern.matchPointer(res));

		EXPECT_NE(*stmt, *res);
		EXPECT_EQ(*stmt, *final);
	}


} // end namespace pattern
} // end namespace core
} // end namespace insieme
