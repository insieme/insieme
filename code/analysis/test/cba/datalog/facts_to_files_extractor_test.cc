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

#include <boost/filesystem/operations.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/analysis/cba/datalog/facts_to_files_extractor.h"
#include "insieme/analysis/cba/common/preprocessing.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/driver/cmd/commandline_options.h"

#include "insieme/utils/config.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	using namespace insieme::core;
	namespace fs = boost::filesystem;

	TEST(FactsToFilesExtractor, Simple) {
		NodeManager nm;
		IRBuilder builder(nm);

		const string controlValue = "12345";

		auto in("{"
		        "var ref<int<4>> x = " + controlValue + ";"
		        "var ref<ptr<int<4>>> y = ptr_from_ref(x);"
		        "var ref<ptr<ptr<int<4>>>,f,f,plain> z = ptr_from_ref(y);"
		        "$x$;"
		        "$y$;"
		        "$z$;"
		        "}");

		auto ptr = builder.parseAddressesStatement(in);

		ExpressionAddress x = ptr[0].as<VariableAddress>();
		ExpressionAddress y = ptr[1].as<VariableAddress>();
		ExpressionAddress z = ptr[2].as<VariableAddress>();

		TargetRelations targets;
		targets["Targets"].emplace(x);
		targets["Targets"].emplace(y);
		targets["Targets"].emplace(z);

		string edCommands("/^Result\n"
		                  "s/\\.$/, Targets(ID).\n"
		                  "i\n"
		                  ".decl Targets(ID:node) input\n"
		                  ".\n"
		                  "w\n"
		                  "q\n"
		                  );

		bool res = extractPointerFactsToFiles(targets, edCommands);

		EXPECT_TRUE(res);
	}


	// NOTE: DISABLED BECAUSE VALGRIND TEST FAILS
	TEST(FactsToFilesExtractor, DISABLED_LoadFromFile) {
		const string filename = "bool.c";
		const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "analysis/test/cba/common/input_tests/";
		string file = ROOT_DIR + filename;

		SCOPED_TRACE(file);

		// check whether file is present
		EXPECT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		ASSERT_TRUE(fs::exists(file));
		std::cout << "Loading: " << file << "... " << std::flush;

		// load file using the frontend
		NodeManager mgr;
		std::vector<std::string> argv = {"compiler", file, "-fopenmp", "-fcilk"};
		insieme::driver::cmd::Options options = insieme::driver::cmd::Options::parse(argv);

		auto prog = options.job.execute(mgr);
		prog = preProcessing(prog);

		std::cout << "done" << std::endl;

		// running semantic checks
		auto res = core::checks::check(prog);
		EXPECT_TRUE(res.empty()) << res << "\n------\n" << printer::dumpErrors(res);


		// run CBA analysis
		int testCount = 0;
		visitDepthFirst(NodeAddress(prog), [&](const CallExprAddress& call) {

			// only interested in literal calls
			auto fun = call->getFunctionExpr();
			if (!fun.isa<LiteralPtr>() && !fun.isa<LambdaExprPtr>()) return;

			const string& name = (fun.isa<LiteralPtr>()) ?
					utils::demangle(fun.as<LiteralPtr>()->getStringValue()) :
					utils::demangle(fun.as<LambdaExprPtr>()->getReference()->getNameAsString());

			// check prefix of literal
			if (!boost::starts_with(name, "cba_")) return;

			// check the predicate
			testCount++;

			// alias analysis
			if (name == "cba_expect_ref_are_alias") {
			} else if (name == "cba_expect_ref_may_alias") {
			} else if (name == "cba_expect_ref_not_alias") {


			// boolean analysis
			} else if (name == "cba_expect_true" ||
			           name == "cba_expect_false" ||
			           name == "cba_expect_may_be_true" ||
			           name == "cba_expect_may_be_false")
			{
				TargetRelations targets;
				targets["Targets"].emplace(call.getArgument(0));
				string edCmds("/^result\n"
				              "s/\\.$/, Targets(n).\n"
				              "i\n"
				              ".decl Targets(ID:node) input\n"
				              ".\n"
				              "w\n"
				              "q\n");
				extractPointerFactsToFiles(targets, edCmds);


				// arithmetic analysis
			} else if (name == "cba_expect_undefined_int") {
			} else if (name == "cba_expect_eq_int") {
			} else if (name == "cba_expect_ne_int") {
			} else if (name == "cba_expect_may_eq_int") {

			// debugging
			} else if (name == "cba_print_code") {
			} else if (name == "cba_dump_json") {
			} else if (name == "cba_print_int") {

			// the rest
			} else {
				FAIL() << "Unsupported CBA expectation predicate: " << name << " - " << *core::annotations::getLocation(call);
			}
		});

		// EXPECT_TRUE(testCount > 0) << "No tests encountered within file " << file;

		if (testCount == 0) {
			TargetRelations targets;
			targets["Targets"].insert(NodeAddress(prog));
			extractPointerFactsToFiles(targets);
		}
	}

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
