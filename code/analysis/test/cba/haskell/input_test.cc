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

#include "insieme/analysis/cba/haskell/interface.h"

#include <iostream>
#include <tuple>
#include <fstream>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>
#include <boost/optional.hpp>

#include "insieme/analysis/cba/interface.h"
#include "insieme/analysis/cba/common/preprocessing.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_statistic.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/printer/error_printer.h"

#include "insieme/utils/config.h"
#include "insieme/utils/gtest_utils.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/driver/cmd/commandline_options.h"


using namespace insieme::utils;

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

		// arithmetic
		boost::optional<int> getConstant(const core::ExpressionAddress& x) {
			auto values = this->getValue(x);

			if(values.isUniversal()) return {};

			if(values.size() != 1) return {};

			auto& value = *values.begin();
			if(!value.isConstant()) return {};

			return value.getIntegerValue();
		}

		bool eqConstant(int c, const core::ExpressionAddress& x) {
			auto value = getConstant(x);
			return value ? *value == c : false;
		}

		bool neConstant(int c, const core::ExpressionAddress& x) {
			auto value = getConstant(x);
			return value ? *value != c : true;
		}

		ArithmeticSet getValue(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::getArithmeticValue<Backend>(ctxt, x);
		}

		ArithmeticSet getValues(const core::ExpressionAddress& x) {
			ArithmeticSet res;
			visitDepthFirstInterruptible(x, [&](const InitExprAddress& init)->bool {
				bool first = true;
				for(const auto& a : init->getInitExprList()) {
					if (first) { first = false; continue; }
					res = merge(res, this->getValue(a));
				}
				return true;
			});
			return res;
		}

		// boolean
		bool isTrue(const core::ExpressionAddress& x) {
			return !getValue(x).contains(0);
		}

		bool isFalse(const core::ExpressionAddress& x) {
			return eqConstant(0, x);
		}

		bool mayBeTrue(const core::ExpressionAddress& x) {
			return neConstant(0, x);
		}

		bool mayBeFalse(const core::ExpressionAddress& x) {
			return getValue(x).contains(0);
		}


		// reference
		MemoryLocationSet getMemoryLocations(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::getReferencedMemoryLocations<Backend>(ctxt, x);
		}


		bool isNull(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::isNull<Backend>(ctxt, x);
		}

		bool notNull(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::notNull<Backend>(ctxt, x);
		}

		bool maybeNull(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::mayBeNull<Backend>(ctxt, x);
		}

		bool isExtern(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::isExtern<Backend>(ctxt, x);
		}

		bool notExtern(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::notExtern<Backend>(ctxt, x);
		}

		bool maybeExtern(const core::ExpressionAddress& x) {
			return insieme::analysis::cba::mayBeExtern<Backend>(ctxt, x);
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
			if (*(file.end()-1) == 'p') argv.push_back("--std=c++14");
			insieme::driver::cmd::Options options = insieme::driver::cmd::Options::parse(argv);
			options.job.addIncludeDirectory(ROOT_DIR);

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
						utils::demangle(fun.as<LambdaExprPtr>()->getReference()->getNameAsString()) ;

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

				} else if (name == "cba_expect_defined_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet res = this->getValue(call.getArgument(0));
					EXPECT_TRUE(!res.isUniversal() && !res.empty())
						<< *core::annotations::getLocation(call) << std::endl
						<< "ArithmeticSet evaluates to " << res << std::endl;

				} else if (name == "cba_expect_single_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet res = this->getValue(call.getArgument(0));
					EXPECT_TRUE(!res.isUniversal() && res.size() == 1)
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

				} else if (name == "cba_expect_one_of_int") {
					std::cerr << "Performing " << name << std::endl;
					ArithmeticSet lhs = this->getValue(call.getArgument(0));
					ArithmeticSet rhs = this->getValues(call.getArgument(1));
					EXPECT_FALSE(lhs.empty());
					EXPECT_FALSE(rhs.empty());
					EXPECT_TRUE(lhs == rhs)
						<< *core::annotations::getLocation(call) << std::endl
						<< "LHS ArithmeticSet evaluates to " << lhs << std::endl
						<< "RHS ArithmeticSet evaluates to " << rhs << std::endl;


				// reference analysis
				} else if (name == "cba_expect_undefined_ref") {
					std::cerr << "Performing " << name << std::endl;
					MemoryLocationSet res = this->getMemoryLocations(call.getArgument(0));
					EXPECT_TRUE(res.isUniversal())
						<< *core::annotations::getLocation(call) << std::endl
						<< "MemoryLocationSet evaluates to " << res << std::endl;

				} else if (name == "cba_expect_defined_ref") {
					std::cerr << "Performing " << name << std::endl;
					MemoryLocationSet res = this->getMemoryLocations(call.getArgument(0));
					EXPECT_TRUE(!res.isUniversal() && !res.empty())
						<< *core::annotations::getLocation(call) << std::endl
						<< "MemoryLocationSet evaluates to " << res << std::endl;

				} else if (name == "cba_expect_single_ref") {
					std::cerr << "Performing " << name << std::endl;
					MemoryLocationSet res = this->getMemoryLocations(call.getArgument(0));
					EXPECT_TRUE(!res.isUniversal() && res.size() == 1)
						<< *core::annotations::getLocation(call) << std::endl
						<< "MemoryLocationSet evaluates to " << res << std::endl;

				} else if (name == "cba_expect_not_single_ref") {
					std::cerr << "Performing " << name << std::endl;
					MemoryLocationSet res = this->getMemoryLocations(call.getArgument(0));
					EXPECT_TRUE(res.isUniversal() || res.size() > 1)
						<< *core::annotations::getLocation(call) << std::endl
						<< "MemoryLocationSet evaluates to " << res << std::endl;

				} else if (name == "cba_expect_null_ref") {
					std::cerr << "Performing " << name << std::endl;
					EXPECT_TRUE(this->isNull(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;

				} else if (name == "cba_expect_not_null_ref") {
					std::cerr << "Performing " << name << std::endl;
					EXPECT_TRUE(this->notNull(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;

				} else if (name == "cba_expect_maybe_null_ref") {
					std::cerr << "Performing " << name << std::endl;
					EXPECT_TRUE(this->maybeNull(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;

				} else if (name == "cba_expect_extern_ref") {
					std::cerr << "Performing " << name << std::endl;
					EXPECT_TRUE(this->isExtern(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;

				} else if (name == "cba_expect_not_extern_ref") {
					std::cerr << "Performing " << name << std::endl;
					EXPECT_TRUE(this->notExtern(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;

				} else if (name == "cba_expect_maybe_extern_ref") {
					std::cerr << "Performing " << name << std::endl;
					EXPECT_TRUE(this->maybeExtern(call.getArgument(0)))
						<< *core::annotations::getLocation(call) << std::endl;


				// debugging
				} else if (name == "cba_print_code") {
					// just dump the code
					dumpPretty(prog);

				} else if (name == "cba_dump_json") {
					// dump the code as a json file
					core::dump::json::dumpIR("code.json", prog);
					core::dump::binary::haskell::dumpIR(filename+".binir", prog);

				} else if (name == "cba_dump_statistic") {
					// dump the current statistic
					ctxt.dumpStatistics();

				} else if (name == "cba_dump_solution") {
					// dump the current solution
					ctxt.dumpSolution();

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

	TEST_P(CBA_Inputs_Test, Haskell) {
		ActualTest<HaskellEngine> test;
		test(GetParam());
	}

	namespace {

		void collectFiles(const fs::path& dir, const std::string& prefix, std::vector<string>& res) {

			fs::path root(dir);
			assert_true(fs::is_directory(root));

			for(auto it = fs::directory_iterator(root); it != fs::directory_iterator(); ++it) {
				fs::path file = it->path();
				// collect c files
				auto ext = file.extension().string();
				if (ext == ".c" || ext == ".cpp") {
					res.push_back(prefix + file.filename().string());
				}
				// collect files recursively
				if (fs::is_directory(file)) {
					const auto& name = file.filename().string();
					if (name != "_disabled") {
						collectFiles(file, prefix + name + "/", res);
					}
				}
			}

		}

	}

	/*
	 * Generate a list of configurations for the tests.
	 * This is a cross-product of the cba_tests files and the Datalog/Haskell backends
	 */
	vector<std::string> getFilenames() {
		vector<string> filenames;

		// collect input files
		collectFiles(fs::path(ROOT_DIR), "", filenames);

		// sort files
		std::sort(filenames.begin(), filenames.end());

		// done
		return filenames;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, CBA_Inputs_Test, ::testing::ValuesIn(getFilenames()), TestCaseNamePrinter());

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
