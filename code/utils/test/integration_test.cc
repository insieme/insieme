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

#include "insieme/utils/test/integration_tests.h"

#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>


#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

using namespace insieme::utils;

namespace insieme {
namespace utils {
namespace test {


TEST(TestUtilsTest, getList) {

	namespace fs = boost::filesystem;

	auto res = getAllCases();

	LOG(log::INFO) << join("\n", res);

	// check the existens of the referenced files
	for_each(res, [](const IntegrationTestCase& cur) {
		SCOPED_TRACE(cur.getName());

		EXPECT_GE(cur.getFiles().size(), static_cast<std::size_t>(1));
		for_each(cur.getFiles(), [](const string& cur){
			EXPECT_TRUE(fs::exists( cur )) << "Testing existens of file " << cur;
			EXPECT_FALSE(fs::is_directory( cur )) << "Checking whether " << cur << " is a directory.";
		});

		for_each(cur.getIncludeDirs(), [](const string& cur){
			EXPECT_TRUE(fs::exists( cur )) << "Testing existens of directory " << cur;
			EXPECT_TRUE(fs::is_directory( cur )) << "Checking whether " << cur << " is a directory.";
		});
	});

	// should also work a second time
	auto numTests = res.size();
	res = getAllCases();
	EXPECT_EQ(numTests, res.size());
}



} // end namespace test
} // end namespace utils
} // end namespace insieme
