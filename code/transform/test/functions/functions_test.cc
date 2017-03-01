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

#include "insieme/transform/functions/transformations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"



namespace insieme {
namespace transform {
namespace functions {


	TEST(RecFunUnrolling, Factory) {
		// ---- test invalid construction parameters ---------

		EXPECT_THROW(RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(0u)), InvalidParametersException);

		EXPECT_THROW(RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(1u)), InvalidParametersException);

		EXPECT_THROW(RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2)), InvalidParametersException);

		// and a valid one
		EXPECT_NO_THROW(RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2u)));
	}

	TEST(RecFunUnrolling, Basic) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a recursive function
		auto lambda = builder.parseExpr("decl f: (int<4>)->int<4>;"
										"def f = (x: int<4>)->int<4> {"
		                                "	if (x==0) { return 0; }"
		                                "	if (x==1) { return 1; }"
		                                "	return f(x-1) + f(x-2);"
		                                "}; f")
		                  .as<core::LambdaExprPtr>();

		ASSERT_TRUE(lambda);

		// create a 2-times unrolling transformation
		auto trans = RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2u));
		auto mod = trans->apply(lambda);
		EXPECT_EQ(builder.normalize(lambda->unroll(2)), builder.normalize(mod));
		EXPECT_TRUE(core::checks::check(mod).empty()) << core::checks::check(mod);


		trans = RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(4u));
		mod = trans->apply(lambda);
		EXPECT_EQ(builder.normalize(lambda->unroll(4)), builder.normalize(mod));
		EXPECT_TRUE(core::checks::check(mod).empty()) << core::checks::check(mod);
	}


	TEST(RecFunUnrolling, SingleLine) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a recursive function
		auto lambda = builder.parseExpr("decl f: (int<4>)->int<4>;"
										"def f = (x: int<4>)->int<4> {"
		                                "	return (x==0)?0:((x==1)?1:f(x-1)+f(x-2));"
		                                "}; f")
		                  .as<core::LambdaExprPtr>();

		ASSERT_TRUE(lambda);

		// create a 2-times unrolling transformation
		auto trans = RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2u));

		auto mod = trans->apply(lambda);
		EXPECT_TRUE(core::checks::check(mod).empty()) << core::checks::check(mod);
	}

} // end namespace functions
} // end namespace transform
} // end namespace insieme
