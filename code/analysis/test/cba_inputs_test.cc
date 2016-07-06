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

#include <tuple>
#include <fstream>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "insieme/analysis/cba_interface.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "insieme/utils/config.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace analysis {

	using namespace core;
	namespace fs = boost::filesystem;


	// the type definition (specifying the parameter type)
	class CBA_Inputs_Test : public ::testing::TestWithParam<std::string> { };

	// the directory to load input files from
	const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "analysis/test/cba_tests/";



	/**
	 * A small helper macro: We use EXPECT_* all the time, but this fails if the
	 * selected backend throws a 'not implemented' exception. Hence, we wrap the
	 * GTest call in a try-catch block for our convenience.
	 */
	#define TRY(EXPECT_SOMETHING)                                                 \
	    try {                                                                     \
	        EXPECT_SOMETHING                                                      \
	    } catch (not_implemented_exception e) {                                   \
	        std::cerr << "Warning for " << name << ": " << e.what() << std::endl; \
	    }



	/**
	 * The actual CBA tests have been outsourced to their own class
	 * in order to handle the different CBA backends.
	 *
	 * The constructor saves function pointers to the respective CBAs
	 * as member variables. This way, a uniform test can be written for
	 * all the interface implementations.
	 */
	template <typename Backend>
	class ActualTest {
	private:
		areAliasAnalysis::fun_type areAlias;
		mayAliasAnalysis::fun_type mayAlias;
		notAliasAnalysis::fun_type notAlias;

		isTrueAnalysis::fun_type isTrue;
		isFalseAnalysis::fun_type isFalse;
		mayBeTrueAnalysis::fun_type mayBeTrue;
		mayBeFalseAnalysis::fun_type mayBeFalse;

		getIntegerValuesAnalysis::fun_type getIntegerValues;
		isIntegerConstantAnalysis::fun_type isIntegerConstant;


	public:
		ActualTest()
		        : areAlias(&(analysis<areAliasAnalysis,Backend>()))
		        , mayAlias(&(analysis<mayAliasAnalysis,Backend>()))
		        , notAlias(&(analysis<notAliasAnalysis,Backend>()))

		        , isTrue(&(analysis<isTrueAnalysis,Backend>()))
		        , isFalse(&(analysis<isFalseAnalysis,Backend>()))
		        , mayBeTrue(&(analysis<mayBeTrueAnalysis,Backend>()))
		        , mayBeFalse(&(analysis<mayBeFalseAnalysis,Backend>()))

		        , getIntegerValues(&(analysis<getIntegerValuesAnalysis,Backend>()))
		        , isIntegerConstant(&(analysis<isIntegerConstantAnalysis,Backend>())) {}


		void operator()(const std::string &filename) {
			string file = ROOT_DIR + filename;

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

				const string& name = utils::demangle(fun.as<LiteralPtr>()->getStringValue());

				// check prefix of literal
				if (!boost::starts_with(name, "cba_")) return;

				// check the predicate
				testCount++;


				// alias analysis
				if (name == "cba_expect_is_alias") {
					TRY(EXPECT_PRED2(this->areAlias, call.getArgument(0), call.getArgument(1))
					    << *core::annotations::getLocation(call) << std::endl;)
				} else if (name == "cba_expect_may_alias") {
					TRY(EXPECT_PRED2(this->mayAlias, call.getArgument(0), call.getArgument(1))
					    << *core::annotations::getLocation(call) << std::endl;)
				} else if (name == "cba_expect_not_alias") {
					TRY(EXPECT_PRED2(this->notAlias, call.getArgument(0), call.getArgument(1))
					    << *core::annotations::getLocation(call) << std::endl;)


				// boolean analysis
				} else if (name == "cba_expect_true") {
					TRY(EXPECT_PRED1(this->isTrue, call.getArgument(0))
					    << *core::annotations::getLocation(call) << std::endl;)
				} else if (name == "cba_expect_false") {
					TRY(EXPECT_PRED1(this->isFalse, call.getArgument(0))
					    << *core::annotations::getLocation(call) << std::endl;)
				} else if (name == "cba_expect_may_be_true") {
					TRY(EXPECT_PRED1(this->mayBeTrue, call.getArgument(0))
					    << *core::annotations::getLocation(call) << std::endl;)
				} else if (name == "cba_expect_may_be_false") {
					TRY(EXPECT_PRED1(this->mayBeFalse, call.getArgument(0))
					    << *core::annotations::getLocation(call) << std::endl;)


				// arithmetic analysis
				} else if (name == "cba_expect_undefined_int") {
					std::cerr << "Performing " << name << std::endl;
					IntegerSet res = this->getIntegerValues(call.getArgument(0));
					TRY(EXPECT_TRUE(res.isUniversal())
					    << *core::annotations::getLocation(call) << std::endl
					    << "IntegerSet evaluates to " << res << std::endl;)

				} else if (name == "cba_expect_eq_int") {
					std::cerr << "Performing " << name << std::endl;
					IntegerSet lhs = this->getIntegerValues(call.getArgument(0));
					IntegerSet rhs = this->getIntegerValues(call.getArgument(1));
					TRY(EXPECT_TRUE(lhs.will_equal(rhs))
					    << *core::annotations::getLocation(call) << std::endl
					    << "LHS IntegerSet evaluates to " << lhs << std::endl
					    << "RHS IntegerSet evaluates to " << rhs << std::endl;)

				} else if (name == "cba_expect_ne_int") {
					std::cerr << "Performing " << name << std::endl;
					IntegerSet lhs = this->getIntegerValues(call.getArgument(0));
					IntegerSet rhs = this->getIntegerValues(call.getArgument(1));
					TRY(EXPECT_TRUE(lhs.will_not_equal(rhs))
					    << *core::annotations::getLocation(call) << std::endl
					    << "LHS IntegerSet evaluates to " << lhs << std::endl
					    << "RHS IntegerSet evaluates to " << rhs << std::endl;)

				} else if (name == "cba_expect_may_eq_int") {
					std::cerr << "Performing " << name << std::endl;
					IntegerSet lhs = this->getIntegerValues(call.getArgument(0));
					IntegerSet rhs = this->getIntegerValues(call.getArgument(1));
					TRY(EXPECT_TRUE(lhs.may_equal(rhs))
					    << *core::annotations::getLocation(call) << std::endl
					    << "LHS IntegerSet evaluates to " << lhs << std::endl
					    << "RHS IntegerSet evaluates to " << rhs << std::endl;)


				// debugging
				} else if (name == "cba_print_code") {
					// just dump the code
					dumpPretty(prog);


				// the rest
				} else {
					FAIL() << "Unsupported CBA expectation predicate: " << name << " - " << *core::annotations::getLocation(call);
				}
			});

			EXPECT_TRUE(testCount > 0) << "No tests encountered within file " << file;
		}

	};

	/**
	 * The test cases. One for each backend.
	 * GTest is not able to mix type- and value-parametrized tests.
	 */
	TEST_P(CBA_Inputs_Test, DISABLED_Datalog) {
		ActualTest<datalogEngine> test;
		test(GetParam());
	}

	TEST_P(CBA_Inputs_Test, DISABLED_Haskell) {
		ActualTest<haskellEngine> test;
		test(GetParam());
	}

	/*
	 * Generate a list of configurations for the tests.
	 * This is a cross-product of the cba_tests files and the Datalog/Haskell backends
	 */
	vector<std::string> getFilenames() {
		vector<string> filenames;

		fs::path root(ROOT_DIR);
		assert_true(fs::is_directory(root));

		for(auto it = fs::directory_iterator(root); it != fs::directory_iterator(); ++it) {
			fs::path file = it->path();
			if (file.extension().string() == ".c") {
				filenames.push_back(file.filename().string());
			}
		}
		std::sort(filenames.begin(), filenames.end());

		return filenames;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, CBA_Inputs_Test, ::testing::ValuesIn(getFilenames()));

	// undef macros
	#undef EXPECT_OR_CATCH


} // end namespace analysis
} // end namespace insieme
