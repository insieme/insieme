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
#include "insieme/analysis/cba/haskell/symbolic_value_analysis.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_statistic.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"


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


		// -- symbolic values --

		using symbolic_value_list = std::vector<std::string>;

		NodePtr groundFreeVariables(const NodePtr& node) {
			IRBuilder builder(node.getNodeManager());

			// get free variables
			auto freeVars = core::analysis::getFreeVariables(node);

			// compute replacements
			unsigned i = 0;
			core::NodeMap replacements;
			for(const auto& var : freeVars) {
				replacements[var] = builder.variable(var.getType(),i++);
			}

			// apply replacements
			return core::transform::replaceAll(node.getNodeManager(),node,replacements);

		}

		symbolic_value_list toList(const insieme::analysis::cba::haskell::SymbolicValueSet& values) {
			symbolic_value_list res;

			// handle universal sets
			if (values.isUniversal()) {
				res.push_back("-all-");
				return res;
			}

			// convert values
			for(const auto& cur : values) {
				// check that they do not contain semantic errors
				EXPECT_TRUE(cur && insieme::core::checks::check(cur).empty())
					<< insieme::core::printer::dumpErrors(insieme::core::checks::check(cur));
				// add print-out to result values
				res.push_back(toString(dumpOneLine(groundFreeVariables(cur))));
			}

			// sort values
			std::sort(res.begin(),res.end());

			// done
			return res;
		}

		// symbolic value
		std::pair<std::string,std::string> checkSymbolicValue(const core::ExpressionPtr& _should, const core::ExpressionAddress& e) {
			NodeManager& mgr = _should.getNodeManager();
			auto& ptrExt = mgr.getLangExtension<core::lang::PointerExtension>();

			// extract the value we should get
			auto should = _should;

			// strip of optional pointer cast
			if (ptrExt.isCallOfPtrCast(should)) should = should.as<CallExprPtr>().getArgument(0);

			// remove ptr-from-array call and extract literal
			assert_true(ptrExt.isCallOfPtrFromArray(should)) << "Should is not a string constant: " << dumpPretty(_should);
			auto lit = should.as<CallExprPtr>().getArgument(0).as<LiteralPtr>();
			assert_true(lit) << "Should is not a string constant: " << dumpPretty(_should);
			assert_le(2,lit->getStringValue().size());
			auto expected = lit->getStringValue().substr(1,lit->getStringValue().size()-2);

			// run the analysis
			auto values = insieme::analysis::cba::haskell::getSymbolicValue(ctxt,e);

			// compare results
			return std::make_pair(expected,toString(toList(values)));
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
			ASSERT_TRUE(res.empty()) << res << "\n------\n" << printer::dumpErrors(res);

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
					EXPECT_TRUE(res.empty() || res.isUniversal())
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
						<< "RHS ArithmeticSet evaluates to " << rhs << std::endl
						<< "LHS: " << call.getArgument(0) << std::endl
						<< "RHS: " << call.getArgument(1) << std::endl;

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

				// symbolic value analysis
				} else if (name.substr(0,25) == "cba_expect_symbolic_value") {
					std::cerr << "Performing " << name << std::endl;
					auto res = this->checkSymbolicValue(call.getArgument(0),call.getArgument(1));
					EXPECT_EQ(res.first,res.second)
						<< *core::annotations::getLocation(call) << std::endl;

				} else if (name.substr(0,32) == "cba_expect_symbolic_single_value") {
					std::cerr << "Performing " << name << std::endl;
					auto values = insieme::analysis::cba::haskell::getSymbolicValue(ctxt,call.getArgument(0));
					EXPECT_TRUE(!values.isUniversal() && values.size()==1)
						<< *core::annotations::getLocation(call) << "@" << call.getArgument(0) << ":" << values << std::endl;;

				// debugging
				} else if (name == "cba_print_code") {
					// just dump the code
					dumpReadable(prog);

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

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks,
	                        CBA_Inputs_Test,
	                        ::testing::ValuesIn(utils::collectInputFiles(ROOT_DIR, {".c", ".cpp"})),
	                        utils::TestCaseNamePrinter());

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
