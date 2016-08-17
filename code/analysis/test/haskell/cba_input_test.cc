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

#include "insieme/analysis/haskell_interface.h"

#include <iostream>
#include <tuple>
#include <fstream>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "insieme/analysis/cba_interface.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"

#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "insieme/utils/config.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace analysis {

	class CBAInputTestExt : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		CBAInputTestExt(core::NodeManager& manager) : core::lang::Extension(manager) {}

	public:

		// this extension is based upon the symbols defined by the pointer module
		IMPORT_MODULE(core::lang::PointerExtension);

		LANG_EXT_LITERAL(RefAreAlias,"cba_expect_ref_are_alias","(ref<'a>,ref<'a>)->unit");
		LANG_EXT_LITERAL(RefMayAlias,"cba_expect_ref_may_alias","(ref<'a>,ref<'a>)->unit");
		LANG_EXT_LITERAL(RefNotAlias,"cba_expect_ref_not_alias","(ref<'a>,ref<'a>)->unit");

		LANG_EXT_DERIVED(PtrAreAlias,
				"  (a : ptr<'a>, b : ptr<'a>) -> unit {                         "
				"		cba_expect_ref_are_alias(ptr_to_ref(a),ptr_to_ref(b));  "
				"  }                                                            "
		)

		LANG_EXT_DERIVED(PtrMayAlias,
				"  (a : ptr<'a>, b : ptr<'a>) -> unit {                         "
				"		cba_expect_ref_may_alias(ptr_to_ref(a),ptr_to_ref(b));  "
				"  }                                                            "
		)

		LANG_EXT_DERIVED(PtrNotAlias,
				"  (a : ptr<'a>, b : ptr<'a>) -> unit {                         "
				"		cba_expect_ref_not_alias(ptr_to_ref(a),ptr_to_ref(b));  "
				"  }                                                            "
		)

	};

	core::ProgramPtr postProcessing(const core::ProgramPtr& prog) {
		const auto& ext = prog.getNodeManager().getLangExtension<CBAInputTestExt>();
		return core::transform::transformBottomUpGen(prog, [&](const core::LiteralPtr& lit)->core::ExpressionPtr {
			const string& name = utils::demangle(lit->getStringValue());
			if (name == "cba_expect_is_alias") {
				return ext.getPtrAreAlias();
			}
			if (name == "cba_expect_may_alias") {
				return ext.getPtrMayAlias();
			}
			if (name == "cba_expect_not_alias") {
				return ext.getPtrNotAlias();
			}
			return lit;
		}, core::transform::globalReplacement);
	}

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
	const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "analysis/test/common/cba_input_tests/";

	template <typename Backend>
	class ActualTest {

		private:

		// alias
		bool areAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y) {
			return insieme::analysis::areAlias<Backend>(x, y);
		}

		bool mayAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y) {
			return insieme::analysis::mayAlias<Backend>(x, y);
		}

		bool notAlias(const core::ExpressionAddress& x, const core::ExpressionAddress& y) {
			return insieme::analysis::notAlias<Backend>(x, y);
		}

		// boolean
		bool isTrue(const core::ExpressionAddress& x) {
			return eqConstant(1,x);
		}

		bool isFalse(const core::ExpressionAddress& x) {
			return eqConstant(0,x);
		}

		bool mayBeTrue(const core::ExpressionAddress& x) {
			return getValue(x).contains(1);
		}

		bool mayBeFalse(const core::ExpressionAddress& x) {
			return getValue(x).contains(0);
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
			return insieme::analysis::getArithmeticValue<Backend>(x);
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
			prog = postProcessing(prog);

			std::cout << "done" << std::endl;

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
				if (name == "cba_expect_ref_are_alias") {
					std::cout << *call.getArgument(0) << "\n";
					EXPECT_TRUE(this->areAlias(call.getArgument(0), call.getArgument(1)))
					<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_ref_may_alias") {
					EXPECT_TRUE(this->mayAlias(call.getArgument(0), call.getArgument(1)))
						<< *core::annotations::getLocation(call) << std::endl;
				} else if (name == "cba_expect_ref_not_alias") {
					EXPECT_TRUE(this->notAlias(call.getArgument(0), call.getArgument(1)))
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
					dumpPretty(prog);

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

} // end namespace analysis
} // end namespace insieme
