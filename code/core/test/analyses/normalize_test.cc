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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/normalize.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(Normalizing, Basic) {

		NodeManager manager;
		IRBuilder builder(manager);
		auto& basic = manager.getLangBasic();

		// create two variables not being normalized
		VariablePtr a = builder.variable(basic.getInt4(), 2);
		VariablePtr b = builder.variable(basic.getBool(), 3);

		// test some simple stuff
		EXPECT_EQ("AP(int<4>)", toString(normalize(basic.getInt4())));

		// a and b should be normalized to the same variable
		EXPECT_EQ(builder.variable(a->getType(), 0), normalize(a));
		EXPECT_EQ(builder.variable(b->getType(), 0), normalize(b));

		// test a compound
		EXPECT_EQ("AP({v0; v1;})", toString(normalize(builder.compoundStmt(a, b))));


		// test a nested compound
		EXPECT_EQ("AP({v0; {v1;};})", toString(normalize(
				builder.compoundStmt(a, builder.compoundStmt(b)))
			));


		// test a function
		NodePtr node = builder.parse("{ int<4> a = 0; let f = (int<4> a, int<4> b)->int<4> { return a; } in f(a,a); }");
		EXPECT_EQ("AP({int<4> v1 = 0; rec v5.{v5=fun(int<4> v3, int<4> v4) {return v3;}}(v1, v1);})", toString(node));
		EXPECT_EQ("AP({int<4> v0 = 0; rec v0.{v0=fun(int<4> v1, int<4> v2) {return v1;}}(v0, v0);})", toString(normalize(node)));
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
