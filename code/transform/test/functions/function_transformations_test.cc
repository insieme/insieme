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

#include "insieme/transform/functions/transformations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace transform {
namespace functions {


	TEST(RecFunUnrolling, Factory) {

		// ---- test invalid construction parameters ---------

		EXPECT_THROW(
				RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(0u)),
				InvalidParametersException
		);

		EXPECT_THROW(
				RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(1u)),
				InvalidParametersException
		);

		EXPECT_THROW(
				RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2)),
				InvalidParametersException
		);

		// and a valid one
		EXPECT_NO_THROW(
				RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2u))
		);

	}

	TEST(RecFunUnrolling, Basic) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a recursive function
		auto lambda = builder.parseExpr(
				"let f = (int<4> x)->int<4> {"
				"	if (x==0) return 0;"
				"	if (x==1) return 1;"
				"	return f(x-1) + f(x-2);"
				"} in f"
		).as<core::LambdaExprPtr>();

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
			auto lambda = builder.parseExpr(
					"let f = (int<4> x)->int<4> {"
					"	return (x==0)?0:((x==1)?1:f(x-1)+f(x-2));"
					"} in f"
			).as<core::LambdaExprPtr>();

			ASSERT_TRUE(lambda);

			// create a 2-times unrolling transformation
			auto trans = RecursiveFunctionUnrollingType().buildTransformation(parameter::makeValue(2u));

			auto mod = trans->apply(lambda);
			EXPECT_TRUE(core::checks::check(mod).empty()) << core::checks::check(mod);
		}

} // end namespace functions
} // end namespace transform
} // end namespace insieme


