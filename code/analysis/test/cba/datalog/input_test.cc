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

#include "insieme/analysis/cba/datalog/interface.h"

#include <iostream>
#include <tuple>
#include <fstream>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "insieme/analysis/cba/interface.h"
#include "insieme/analysis/cba/common/preprocessing.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/config.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/driver/cmd/commandline_options.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;
	using testing::Types;
	using SymbolTable = std::map<std::string, NodePtr>;
	namespace fs = boost::filesystem;

	IRBuilder& getBuilder() {
		static NodeManager mgr;
		static IRBuilder builder(mgr);
		return builder;
	}

	// the directory to load input files from
	const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "analysis/test/cba/common/input_tests/";

	template <typename Backend>
	class ActualTest {

		typename Backend::context_type ctxt;

		private:

		// alias
		bool areAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y) {
			return insieme::analysis::cba::areAlias<Backend>(ctxt, x, y);
		}

		bool mayAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y) {
			return insieme::analysis::cba::mayAlias<Backend>(ctxt, x, y);
		}

		bool notAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y) {
			return insieme::analysis::cba::notAlias<Backend>(ctxt, x, y);
		}

		// boolean
		bool isTrue(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::isTrue<Backend>(ctxt,x);
		}

		bool isFalse(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::isFalse<Backend>(ctxt,x);
		}

		bool mayBeTrue(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::mayBeTrue<Backend>(ctxt,x);
		}

		bool mayBeFalse(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::mayBeFalse<Backend>(ctxt,x);
		}

		bool eqConstant(int c, const core::ExpressionAddress& x) {
			auto values = this->getValue(x);

			if (values.isUniversal()) return false;

			if (values.size() != 1) return false;

			auto& value = *values.begin();
			if (!value.isConstant()) return false;

			return c == value.getIntegerValue();
		}

		ArithmeticSet getValue(const core::ExpressionAddress& x) {
			// return insieme::analysis::cba::getArithmeticValue<Backend>(ctxt,x);
			const IntegerSet& is = insieme::analysis::cba::getIntegerValues<Backend>(x);
			return insieme::analysis::cba::IntegerToArithmeticSet(is);
		}

	public:
		ActualTest() {}


		void operator()(const std::string &filename) {
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
					EXPECT_TRUE(this->areAlias(call.getArgument(0), call.getArgument(1)))
						<< "lhs = " << call.getArgument(0) << "\n"
						<< "rhs = " << call.getArgument(1) << "\n"
						<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_ref_may_alias") {
					EXPECT_TRUE(this->mayAlias(call.getArgument(0), call.getArgument(1)))
						<< "lhs = " << call.getArgument(0) << "\n"
						<< "rhs = " << call.getArgument(1) << "\n"
						<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_ref_not_alias") {
					EXPECT_TRUE(this->notAlias(call.getArgument(0), call.getArgument(1)))
						<< "lhs = " << call.getArgument(0) << "\n"
						<< "rhs = " << call.getArgument(1) << "\n"
						<< *core::annotations::getLocation(call) << std::endl;


				// boolean analysis
				} else if (name == "cba_expect_true") {
					EXPECT_TRUE(this->isTrue(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_false") {
					EXPECT_TRUE(this->isFalse(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_may_be_true") {
					EXPECT_TRUE(this->mayBeTrue(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_may_be_false") {
					EXPECT_TRUE(this->mayBeFalse(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;


				// arithmetic analysis
				} else if (name == "cba_expect_undefined_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet res = this->getValue(call.getArgument(0));
					EXPECT_TRUE(res.isUniversal())
						<< *core::annotations::getLocation(call) << std::endl
						<< "ArithmeticSet evaluates to " << res << std::endl;

				} else if (name == "cba_expect_eq_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet lhs = this->getValue(call.getArgument(0));
					ArithmeticSet rhs = this->getValue(call.getArgument(1));
					EXPECT_FALSE(lhs.empty());
					EXPECT_FALSE(rhs.empty());
					EXPECT_TRUE(!lhs.empty() && lhs == rhs)
						<< *core::annotations::getLocation(call) << std::endl
						<< "LHS ArithmeticSet evaluates to " << lhs << std::endl
						<< "RHS ArithmeticSet evaluates to " << rhs << std::endl;

				} else if (name == "cba_expect_ne_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet lhs = this->getValue(call.getArgument(0));
					ArithmeticSet rhs = this->getValue(call.getArgument(1));
					EXPECT_FALSE(lhs.empty());
					EXPECT_FALSE(rhs.empty());
					EXPECT_TRUE(lhs != rhs)
						<< *core::annotations::getLocation(call) << std::endl
						<< "LHS ArithmeticSet evaluates to " << lhs << std::endl
						<< "RHS ArithmeticSet evaluates to " << rhs << std::endl;

				} else if (name == "cba_expect_may_eq_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet lhs = this->getValue(call.getArgument(0));
					ArithmeticSet rhs = this->getValue(call.getArgument(1));
					EXPECT_FALSE(lhs.empty());
					EXPECT_FALSE(rhs.empty());
					ArithmeticSet inter = intersect(lhs, rhs);
					EXPECT_TRUE(lhs.isUniversal() || rhs.isUniversal() || inter.size() > 0)
						<< *core::annotations::getLocation(call) << std::endl
						<< "LHS ArithmeticSet evaluates to " << lhs << std::endl
						<< "RHS ArithmeticSet evaluates to " << rhs << std::endl;

				// debugging
				} else if (name == "cba_print_code") {
					// just dump the code
//					dumpPretty(prog);

				} else if (name == "cba_dump_json") {
					// just dump the code as a json file
//					core::dump::json::dumpIR("code.json", prog);

				} else if (name == "cba_print_int") {
					// print the deduced value of the argument
					std::cout << call << " = " << this->getValue(call.getArgument(0)) << "\n";

				// the rest
				} else {
					FAIL() << "Unsupported CBA expectation predicate: " << name << " - " << *core::annotations::getLocation(call);
				}
			});

			EXPECT_TRUE(testCount > 0) << "No tests encountered within file " << file;
		}

	};

	// the type definition (specifying the parameter type)
	class CBA_Inputs_Test : public ::testing::TestWithParam<std::string> { };

	TEST_P(CBA_Inputs_Test, DISABLED_Datalog) {
		ActualTest<DatalogEngine> test;
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

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
