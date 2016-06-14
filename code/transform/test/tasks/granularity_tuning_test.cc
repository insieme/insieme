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

#include "insieme/transform/tasks/granularity_tuning.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"



namespace insieme {
namespace transform {
namespace tasks {

	TEST(GranularityTuning, Simple) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto addresses = builder.parseAddressesProgram(R"1N5P1RE(
            alias int = int<4>;
			decl taskfun: (int)->unit;
            def taskfun = (v: int) -> unit {
                if(v == 0) {return;}
                parallel(job [1..1] => taskfun(v-1));
				mergeAll();
            };

            unit main() {
				$taskfun$(5);
            }
        )1N5P1RE");

		core::ProgramPtr prog = addresses[0].getRootNode().as<core::ProgramPtr>();
		EXPECT_TRUE(core::checks::check(prog).empty()) << core::checks::check(prog);

		auto opt = applyTaskOptimization(prog);
		EXPECT_TRUE(core::checks::check(opt).empty()) << core::checks::check(opt);

		auto newFunExp = addresses[0].switchRoot(opt).getAddressedNode().as<core::LambdaExprPtr>();
		// dumpColor(newFunExp);

		auto definitions = newFunExp->getDefinition()->getDefinitions();
		// check that we get 4 versions
		EXPECT_EQ(definitions.size(), 4);
		// check that last version is sequential
		EXPECT_TRUE(!core::analysis::isParallel(definitions[3]));
		// check that superfluous merges are removed
		EXPECT_TRUE(core::analysis::isParallel(definitions[2]));
		EXPECT_EQ(core::analysis::countInstances(definitions[2], builder.parseExpr("mergeAll()")), 1);
	}

	TEST(GranularityTuning, Mutual) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto addresses = builder.parseAddressesProgram(R"1N5P1RE(
			alias int = int<4>;
			decl tf2: (int)->unit;
			def tf1 = (v: int) -> unit {
				if(v == 0) {return;}
				parallel(job [1..1] => tf2(v-1));
				mergeAll();
			};
			def tf2 = (v: int) -> unit {
				if(v == 0) {return;}
				parallel(job [1..1] => tf1(v-1));
				mergeAll();
			};

			unit main() {
				$tf1$(5);
			}
		)1N5P1RE");

		core::ProgramPtr prog = addresses[0].getRootNode().as<core::ProgramPtr>();
		EXPECT_TRUE(core::checks::check(prog).empty()) << core::checks::check(prog);

		auto opt = applyTaskOptimization(prog);
		EXPECT_TRUE(core::checks::check(opt).empty()) << core::checks::check(opt);

		auto newFunExp = addresses[0].switchRoot(opt).getAddressedNode().as<core::LambdaExprPtr>();
		// dumpColor(newFunExp);

		auto definitions = newFunExp->getDefinition()->getDefinitions();
		// check that we get 2*4 versions
		EXPECT_EQ(definitions.size(), 8);
		// check that last version of fun1 is sequential
		EXPECT_TRUE(!core::analysis::isParallel(definitions[3]));
		// check that superfluous merges are removed
		EXPECT_TRUE(core::analysis::isParallel(definitions[2]));
		EXPECT_EQ(core::analysis::countInstances(definitions[2], builder.parseExpr("mergeAll()")), 1);
		// check that last version of fun2 is sequential
		EXPECT_TRUE(!core::analysis::isParallel(definitions[7]));
		// check that superfluous merges are removed
		EXPECT_TRUE(core::analysis::isParallel(definitions[6]));
		EXPECT_EQ(core::analysis::countInstances(definitions[6], builder.parseExpr("mergeAll()")), 1);
	}

} // end namespace tasks
} // end namespace transform
} // end namespace insieme
