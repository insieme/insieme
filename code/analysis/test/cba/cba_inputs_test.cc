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

#include <fstream>
#include <sstream>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "insieme/utils/test/test_config.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/frontend/frontend.h"

#include "insieme/analysis/cba/analysis.h"

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

	// define the test case pattern
	TEST_P(CBAInputTest, C_Code) {

		string file = ROOT_DIR + string(GetParam());

		SCOPED_TRACE(file);

		// check whether file is present
		EXPECT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		ASSERT_TRUE(fs::exists(file));

		// load file using the frontend
		NodeManager mgr;
		fe::ConversionJob job(file);
		auto prog = job.execute(mgr);

		// running semantic checks
		auto res = core::checks::check(prog);
		EXPECT_TRUE(res.empty()) << res;

		// run CBA analysis
		int testCount = 0;
		visitDepthFirst(NodeAddress(prog), [&](const CallExprAddress& call) {

			// only interested in literal calls
			auto fun = call->getFunctionExpr();
			if (!fun.isa<LiteralPtr>()) return;

			const string& name = fun.as<LiteralPtr>()->getStringValue();

			// check prefix of literal
			if (!boost::starts_with(name, "cba_expect")) return;

			// check the predicate
			testCount++;
			void cba_expect_true(bool b);
			void cba_expect_false(bool b);
			void cba_expect_maybe(bool b);
			void cba_expect_maybe_not(bool b);

			if (name == "cba_expect_true") {
				EXPECT_PRED1(isTrue, call[0]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_false") {
				EXPECT_PRED1(isFalse, call[0]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_may_be_true") {
				EXPECT_PRED1(mayBeTrue, call[0]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_may_be_false") {
				EXPECT_PRED1(mayBeFalse, call[0]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_is_alias") {
				EXPECT_PRED2(isAlias, call[0], call[1]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_may_alias") {
				EXPECT_PRED2(mayAlias, call[0], call[1]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_not_alias") {
				EXPECT_PRED2(notAlias, call[0], call[1]) << *core::annotations::getLocation(call);;
			} else if (name == "cba_expect_eq_int") {
				EXPECT_PRED2(isArithmeticEqual, call[0], call[1]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_ne_int") {
				EXPECT_PRED2(notArithmeticEqual, call[0], call[1]) << *core::annotations::getLocation(call);
			} else if (name == "cba_expect_may_eq_int") {
				EXPECT_PRED2(mayArithmeticEqual, call[0], call[1]) << *core::annotations::getLocation(call);
			} else {
				FAIL() << "Unsupported CBA expectation predicate: " << name << " - " << *core::annotations::getLocation(call);
			}
		});

		// for debugging
		createDotDump(ProgramAddress(prog)[0].as<LambdaExprAddress>()->getBody());

		EXPECT_TRUE(testCount > 0) << "No tests encountered within file " << file;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, CBAInputTest, ::testing::ValuesIn(getInputFiles()));


	vector<string> getInputFiles() {
		vector<string> res;

		fs::path root(ROOT_DIR);
		assert(fs::is_directory(root));

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
