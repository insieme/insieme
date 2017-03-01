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

#include <string>
#include <vector>

#include <boost/filesystem.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/config.h"
#include "insieme/utils/gtest_utils.h"

using namespace insieme::utils;

namespace insieme {
namespace analysis {
namespace cba {

	using std::string;
	using std::vector;

	using testing::Types;
	namespace fs = boost::filesystem;

	// ---- Check the Compiler ----

	namespace {

		void checkCompiler(const utils::compiler::Compiler& compiler) {

			// create some code examples
			auto code = R"(
					#include <assert.h>

					int main() {
						assert(%d);
					}
				)";

			auto OK  = format(code, 1);
			auto ERR = format(code, 0);

			// check the OK version first
			{
				auto binary = utils::compiler::compileToBinary(OK, compiler);
				EXPECT_NE("",binary);
				if (binary.empty()) return;

				auto exitCode = system((binary + " > /dev/null 2> /dev/null").c_str());
				EXPECT_EQ(0,exitCode);

				fs::remove(binary);
				EXPECT_FALSE(fs::exists(binary));
			}


			// check the ERR version first
			{
				auto binary = utils::compiler::compileToBinary(ERR, compiler);
				EXPECT_NE("",binary);
				if (binary.empty()) return;

				auto exitCode = system((binary + " > /dev/null 2> /dev/null").c_str());
				EXPECT_NE(0,exitCode);

				fs::remove(binary);
				EXPECT_FALSE(fs::exists(binary));
			}
		}

	}

	TEST(InputTestVerification, AssertionCheckC99) {
		checkCompiler(utils::compiler::Compiler::getDefaultC99Compiler());
	}

	TEST(InputTestVerification, AssertionCheckCpp) {
		checkCompiler(utils::compiler::Compiler::getDefaultCppCompiler());
	}


	// ---- Check the Input Files ----

	// the directory to load input files from
	const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "analysis/test/common/input_tests/";

	// the type definition (specifying the parameter type)
	class InputTestVerifyer : public ::testing::TestWithParam<std::string> { };

	TEST_P(InputTestVerifyer, VerifyTest) {

		// load the input file
		auto file = GetParam();

		// setup compiler
		auto compiler = (*(file.end()-1) == 'p') ?
				utils::compiler::Compiler::getDefaultCppCompiler() :
				utils::compiler::Compiler::getDefaultC99Compiler() ;
		compiler.addIncludeDir(ROOT_DIR);

		// compile the input program
		auto binary = utils::compiler::compile(ROOT_DIR + "/" + file, compiler);
		ASSERT_NE("",binary) << "Compilation failed!";

		// run the binary
		auto exitCode = system(binary.c_str());

		// check the exit code
		EXPECT_EQ(0, exitCode);

		// delete the binary
		fs::remove(binary);
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
	INSTANTIATE_TEST_CASE_P(InputFileChecks, InputTestVerifyer, ::testing::ValuesIn(getFilenames()), TestCaseNamePrinter());

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
