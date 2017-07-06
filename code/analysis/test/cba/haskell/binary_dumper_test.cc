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

#include <cstdlib>
#include <sstream>
#include <string>
#include <vector>

#include <boost/filesystem.hpp>

#include "insieme/analysis/cba/common/preprocessing.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/binary_haskell.h"

#include "insieme/driver/cmd/commandline_options.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/config.h"
#include "insieme/utils/gtest_utils.h"

extern "C" {

	extern void hat_hs_test_binary_dumper_mirror(const char* data_c, std::size_t size_c,
	                                             char** data_hs, std::size_t* size_hs);

}

using namespace std;
using namespace insieme::core;
using namespace insieme::utils;

namespace fs = boost::filesystem;

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	// the directory to load input files from
	const auto ROOT_DIR = getInsiemeSourceRootDir() + "analysis/test/cba/common/input_tests/";

	class Haskell_Binary_Dumper_Test : public ::testing::TestWithParam<string> {};

	TEST_P(Haskell_Binary_Dumper_Test, Mirror) {
		string file = ROOT_DIR + GetParam();

		SCOPED_TRACE(file);

		ASSERT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		cout << "Loading " << file << " ..." << flush;

		// load file using the frontend
		NodeManager mgr;
		vector<string> argv = {"compiler", file, "-fopenmp", "-fcilk"};
		if (*(file.end()-1) == 'p') argv.push_back("--std=c++14");
		insieme::driver::cmd::Options options = insieme::driver::cmd::Options::parse(argv);
		options.job.addIncludeDirectory(ROOT_DIR);

		auto prog = options.job.execute(mgr);
		prog = preProcessing(prog);

		cout << "done" << endl;

		// create in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);
		dump::binary::haskell::dumpIR(buffer, prog);
		const string data_c = buffer.str();

		// setup receiving data
		char *data_hs;
		size_t size_hs;

		// C++ dumper -> Haskell parser -> INSPIRE -> Haskell dumper -> C++ parser
		hat_hs_test_binary_dumper_mirror(data_c.c_str(), data_c.size(), &data_hs, &size_hs);

		ASSERT_TRUE(size_hs > 0) << "receiving size should not be zero";

		// put data into buffer for reading
		buffer.str({data_hs, size_hs});

		NodeManager mgr2;
		auto prog2 = dump::binary::loadIR(buffer, mgr2);
		ASSERT_TRUE(prog2) << "could not load received data";

		// data_hs has been allocated in Haskell using malloc, we don't need it anymore after loadIR
		free(data_hs);

		// The original tree should be equal to the received one.
		ASSERT_EQ(*prog, *prog2);
	}

	namespace {

		void collectFiles(const fs::path& dir, const string& prefix, vector<string>& res) {
			fs::path root(dir);
			assert_true(fs::is_directory(root));

			for(auto it = fs::directory_iterator(root); it != fs::directory_iterator(); ++it) {
				fs::path file = it->path();
				// collect c files
				auto ext = file.extension().string();
				if(ext == ".c" || ext == ".cpp") {
					res.push_back(prefix + file.filename().string());
				}
				// collect files recursively
				if(fs::is_directory(file)) {
					const auto& name = file.filename().string();
					if(name != "_disabled") {
						collectFiles(file, prefix + name + "/", res);
					}
				}
			}
		}

		vector<string> getFilenames() {
			vector<string> filenames;

			// collect input files
			collectFiles(fs::path(ROOT_DIR), "", filenames);

			// sort files
			std::sort(filenames.begin(), filenames.end());

			// done
			return filenames;
		}

	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(BinaryDumperChecks, Haskell_Binary_Dumper_Test, ::testing::ValuesIn(getFilenames()), TestCaseNamePrinter());

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
