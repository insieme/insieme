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

#include <string>
#include <vector>

#include <boost/filesystem.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/config.h"
#include "insieme/utils/compiler/compiler.h"


namespace insieme {
namespace analysis {

	using std::string;
	using std::vector;

	using testing::Types;
	namespace fs = boost::filesystem;


	// the directory to load input files from
	const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "analysis/test/common/cba_input_tests/";


	// the type definition (specifying the parameter type)
	class CBA_Inputs_Test : public ::testing::TestWithParam<std::string> { };

	TEST_P(CBA_Inputs_Test, Haskell) {

		// load the input file
		auto file = GetParam();

		// setup comput
		auto compiler = (*(file.end()-1) == 'p') ?
				utils::compiler::Compiler::getDefaultCppCompiler() :
				utils::compiler::Compiler::getDefaultC99Compiler() ;
		compiler.addIncludeDir(ROOT_DIR);


		auto binary = utils::compiler::compile(ROOT_DIR + "/" + file, compiler);

		ASSERT_NE("",binary) << "Compilation failed!";

		// run the binary
		auto exitCode = system(binary.c_str());

		// check the exit code
		EXPECT_EQ(exitCode, 0);

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

	/**
	 * A printer for test case names
	 */
	struct TestCaseNamePrinter {
	  template <class ParamType>
	  std::string operator()(const ::testing::TestParamInfo<ParamType>& info) const {
		  std::stringstream out;

		  // foramt the index
		  out << format("%3d", info.index);

		  // format the name
		  std::string name = info.param;
		  name = name.substr(0, name.find_last_of('.'));
		  out << format("_%-40s", name);

		  // sanitize the resulting string
		  auto res = out.str();
		  std::replace(res.begin(), res.end(), ' ','_');
		  std::replace(res.begin(), res.end(), '/','_');
		  std::replace(res.begin(), res.end(), '.','_');
		  std::replace(res.begin(), res.end(), '-','_');
		  return res;
	  }
	};

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, CBA_Inputs_Test, ::testing::ValuesIn(getFilenames()), TestCaseNamePrinter());

} // end namespace analysis
} // end namespace insieme
