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
