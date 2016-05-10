/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
