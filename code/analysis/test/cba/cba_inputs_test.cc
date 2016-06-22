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

#include <fstream>
#include <sstream>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "insieme/utils/config.h"
#include "insieme/utils/petri_net/petri_net_io.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/printer/error_printer.h"

#include "insieme/frontend/frontend.h"

#include "insieme/analysis/cba/cba.h"
#include "insieme/analysis/cba/analysis.h"
#include "insieme/analysis/cba/parallel_analysis.h"

#include "insieme/analysis/cba/analysis/references.h"
#include "insieme/analysis/cba/analysis/arithmetic.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	// the directory to load input files from
	const auto ROOT_DIR = SRC_ROOT_DIR "analysis/test/cba/inputs/";

	using std::string;
	using namespace insieme::core;
	namespace fs = boost::filesystem;
	namespace fe = insieme::frontend;

	vector<string> getInputFiles();

	// the type definition (specifying the parameter type)
	class CBAInputTest : public ::testing::TestWithParam<string> { };

	namespace {

		bool containsValue(const std::set<Formula>& formula, int value) {
			for(const auto& cur : formula) {
				// if it is the unknown value => fits
				if (!cur) return true;

				// get the formula
				const core::arithmetic::Formula& f = *cur.formula;

				// check current value
				if (f.isInteger() && f.getIntegerValue() == value) return true;
				if (!f.isConstant()) return true;
			}
			return false;
		}

	}

	// define the test case pattern
	TEST_P(CBAInputTest, C_Code) {

		string file = ROOT_DIR + string(GetParam());

		SCOPED_TRACE(file);

		// check whether file is present
		EXPECT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		ASSERT_TRUE(fs::exists(file));

		// load file using the frontend
		NodeManager mgr;
        std::vector<std::string> argv = {"compiler", file, "-fopenmp", "-fcilk"};
        insieme::driver::cmd::Options options = insieme::driver::cmd::Options::parse(argv);
		auto prog = options.job.execute(mgr);

		// running semantic checks
		auto res = core::checks::check(prog);
		EXPECT_TRUE(res.empty()) << res << "\n------\n" << printer::dumpErrors(res);

		// run CBA analysis
		int testCount = 0;
		visitDepthFirst(NodeAddress(prog), [&](const CallExprAddress& call) {

			// only interested in literal calls
			auto fun = call->getFunctionExpr();
			if (!fun.isa<LiteralPtr>()) return;

			const string& name = fun.as<LiteralPtr>()->getStringValue();

			// check prefix of literal
			if (!boost::starts_with(name, "cba_")) return;

			// check the predicate
			testCount++;

			// alias analysis
			if (name == "cba_expect_is_alias") {
				EXPECT_PRED2(isAlias, call[0], call[1])
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], R) << "\n"
							<< "call[1] evaluates to " << cba::getValues(call[1], R) << "\n";
			} else if (name == "cba_expect_may_alias") {
				EXPECT_PRED2(mayAlias, call[0], call[1])
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], R) << "\n"
							<< "call[1] evaluates to " << cba::getValues(call[1], R) << "\n";
			} else if (name == "cba_expect_not_alias") {
				EXPECT_PRED2(notAlias, call[0], call[1])
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], R) << "\n"
							<< "call[1] evaluates to " << cba::getValues(call[1], R) << "\n";

			// arithmetic analysis
			} else if (name == "cba_expect_undefined_int") {
				const std::set<cba::Formula>& values = cba::getValues(call[0], A);
				EXPECT_TRUE(values.find(Formula()) != values.end())
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n";
			} else if (name == "cba_expect_eq_int") {
				EXPECT_PRED2(isArithmeticEqual, call[0], call[1])
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n"
							<< "call[1] evaluates to " << cba::getValues(call[1], A) << "\n";
			} else if (name == "cba_expect_ne_int") {
				EXPECT_PRED2(notArithmeticEqual, call[0], call[1])
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n"
							<< "call[1] evaluates to " << cba::getValues(call[1], A) << "\n";
			} else if (name == "cba_expect_may_eq_int") {
				EXPECT_PRED2(mayArithmeticEqual, call[0], call[1])
							<< *core::annotations::getLocation(call) << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n"
							<< "call[1] evaluates to " << cba::getValues(call[1], A) << "\n";

			} else if (name == "cba_expect_execution_net_num_places") {
				const auto& net = getExecutionNet(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
				EXPECT_PRED2(containsValue, cba::getValues(call[0], A), net.getNumPlaces())
							<< *core::annotations::getLocation(call) << "\n"
							<< "number of places " << net.getNumPlaces() << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n";
			} else if (name == "cba_expect_execution_net_num_transitions") {
				const auto& net = getExecutionNet(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
				EXPECT_PRED2(containsValue, cba::getValues(call[0], A), net.getNumTransitions())
							<< *core::annotations::getLocation(call) << "\n"
							<< "number of transitions " << net.getNumTransitions() << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n";
			} else if (name == "cba_expect_num_threads") {
				const auto& list = getThreadList(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
				EXPECT_PRED2(containsValue, cba::getValues(call[0], A), list.size())
							<< *core::annotations::getLocation(call) << "\n"
							<< "number of threads " << list.size() << "\n"
							<< "call[0] evaluates to " << cba::getValues(call[0], A) << "\n";

			// debugging
			} else if (name == "cba_print_code") {
				// just dump the code
				dumpPretty(prog);
			} else if (name == "cba_print_constraints") {
				// print the list of equations
				printConstraints(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
			} else if (name == "cba_print_solution") {
				// print the solution for the constraints
				printSolution(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
			} else if (name == "cba_dump_equations") {
				// dump the dot plot
				createDotDump(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
			} else if (name == "cba_dump_sync_points") {
				// dump sync points
				std::cout << "Sync Points:\n\t" << ::join("\n\t", getSyncPoints(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody())) << "\n\n";
			} else if (name == "cba_dump_thread_list") {
				// dump the list of threads
				std::cout << "List of Threads:\n\t" << join("\n\t", getThreadList(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody())) << "\n\n";
			} else if (name == "cba_dump_execution_net") {
				// dump the dot plot of the execution net
				const auto& net = getExecutionNet(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
				EXPECT_LT(1, net.getNumPlaces());
				utils::petri_net::plot(net, "execution_net.svg");
			} else if (name == "cba_dump_state_graph") {
				// dump the dot plot of the execution net
				const auto& states = getExecutionStateGraph(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());
				EXPECT_LT(1, states.getNumStates());
				utils::petri_net::plot(states, "state_graph.svg");
			} else if (name == "cba_dump_thread_regions") {
				// dump the list of thread regions
				std::cout << "Thread Regions:\n\t" << join("\n\t", getThreadRegions(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody())) << "\n\n";
			} else if (name == "cba_print_ref") {
				// print the result of the reference analysis
				std::cout << "References: " << cba::getValues(call[0], R) << " @ " << *core::annotations::getLocation(call) << "\n";
			} else if (name == "cba_print_int") {
				// print the result of the reference analysis
				std::cout << "Values:     " << cba::getValues(call[0], A) << " @ " << *core::annotations::getLocation(call) << "\n";

			// the rest
			} else {
				FAIL() << "Unsupported CBA expectation predicate: " << name << " - " << *core::annotations::getLocation(call);
			}
		});

		EXPECT_TRUE(testCount > 0) << "No tests encountered within file " << file;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, CBAInputTest, ::testing::ValuesIn(getInputFiles()));


	vector<string> getInputFiles() {
		vector<string> res;

		fs::path root(ROOT_DIR);
		assert_true(fs::is_directory(root));

		for(auto it = fs::directory_iterator(root); it != fs::directory_iterator(); ++it) {
			fs::path file = it->path();
			if (file.extension().string() == ".c") {
				res.push_back(file.filename().string());
			}
		}
		std::sort(res.begin(), res.end());

		return res;
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
