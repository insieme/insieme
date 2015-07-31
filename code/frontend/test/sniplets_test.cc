/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/frontend.h"
#include "insieme/utils/config.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include <boost/filesystem.hpp>

using namespace insieme::driver;

namespace insieme {
namespace frontend {


	std::vector<std::string> listSources(){
		namespace fs = boost::filesystem;
		fs::path someDir(FRONTEND_TEST_DIR "/inputs/sniplets");
		fs::directory_iterator end_iter;

		std::vector<std::string> result;

		if ( fs::exists(someDir) && fs::is_directory(someDir)) {
			for( fs::directory_iterator dir_iter(someDir) ; dir_iter != end_iter ; ++dir_iter) {
				if (fs::is_regular_file(dir_iter->status()) &&
				    (dir_iter->path().extension().compare(".c") || dir_iter->path().extension().compare(".cpp"))) {
					result.push_back(dir_iter->path().string());
				}
			}
		}
		return result;
	}



	// the type definition (specifying the parameter type)
	class FrontendSnipletTestCase : public ::testing::TestWithParam<std::string> { };

	// define the test case pattern
	TEST_P(FrontendSnipletTestCase, SemanticChecks) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

			std::cout << "testing: " <<  GetParam() << std::endl;
		// parse temporary file
        std::vector<std::string> argv = { "compiler",  GetParam() };
        cmd::Options options = cmd::Options::parse(argv);

		auto res = builder.normalize(options.job.execute(mgr));

		EXPECT_TRUE ( core::checks::check(res).empty()) << core::checks::check(res);
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(FrontendSnipletTest, FrontendSnipletTestCase, ::testing::ValuesIn(listSources()));


} // fe namespace
}// insieme namespace
