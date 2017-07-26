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

#include <boost/filesystem/operations.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "insieme/utils/net/net_path.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/iterator_utils.h"

namespace insieme {
namespace utils {
namespace net {

	TEST(NetworkPath, Dummy) {
		// there has to be something for some gtest version
	}

	#ifdef SSH_TO_LOCAL_ENABLED

	TEST(NetworkPath, Basic) {
		// test some parameter without dependency
		NetworkPath path = bfs::path("/tmp");

		EXPECT_EQ("/tmp", toString(path));
		EXPECT_EQ("/tmp/hello.txt", toString(path / "hello.txt"));
		EXPECT_EQ("/tmp/hello/world.txt", toString(path / "hello" / "world.txt"));

		// including hosts
		EXPECT_EQ("localhost:/tmp", toString(NetworkPath("localhost", "/tmp")));
		EXPECT_EQ("localhost:/tmp/hello", toString(NetworkPath("localhost", "/tmp") / "hello"));

		// include username as well
		EXPECT_EQ("somebody@localhost:/tmp", toString(NetworkPath("localhost", "somebody", "/tmp")));
		EXPECT_EQ("somebody@localhost:/tmp/hello", toString(NetworkPath("localhost", "somebody", "/tmp") / "hello"));
	}

	TEST(NetworkPath, Manipulation) {
		// test whether a remote session to the local host can be created
		if(system("ssh localhost pwd > /dev/null")) {
			return; // skip this test
		}

		NetworkPath ltmp = NetworkPath("/tmp");
		NetworkPath rtmp = NetworkPath("localhost", "/tmp");

		EXPECT_TRUE(exists(ltmp));
		EXPECT_TRUE(exists(rtmp));

		EXPECT_TRUE(is_directory(ltmp));
		EXPECT_TRUE(is_directory(ltmp));

		boost::uuids::random_generator generator;
		string testDir = "_ut_dir" + toString(generator());

		bfs::path ut_dir = testDir;
		ut_dir /= "sub";

		ASSERT_FALSE(bfs::exists(bfs::path("/tmp") / testDir)) << "Test directory already exits - you won the lottery!";

		// create local directory
		for_each(toVector(ltmp, rtmp), [&](const NetworkPath& cur) {
			SCOPED_TRACE("Path: " + toString(cur));

			ASSERT_FALSE(exists(cur / ut_dir));

			EXPECT_TRUE(create_directories(cur / ut_dir));
			EXPECT_TRUE(exists(cur / ut_dir));

			EXPECT_FALSE(create_directories(cur / ut_dir));
			EXPECT_TRUE(exists(cur / ut_dir));

			EXPECT_TRUE(is_directory(cur / ut_dir));

			// create a file inside the directory
			system(("touch /tmp/" + testDir + "/sub/file").c_str());
			EXPECT_TRUE(exists(cur / ut_dir / "file"));
			EXPECT_TRUE(copy(cur / ut_dir / "file", cur / ut_dir / "copy"));
			EXPECT_TRUE(exists(cur / ut_dir / "copy"));

			EXPECT_TRUE(remove(cur / ut_dir / "file"));
			EXPECT_FALSE(exists(cur / ut_dir / "file"));
			EXPECT_FALSE(remove(cur / ut_dir / "file"));

			EXPECT_TRUE(remove_all(cur / ut_dir));
			EXPECT_FALSE(exists(cur / ut_dir));

			EXPECT_FALSE(remove_all(cur / ut_dir));
			EXPECT_FALSE(exists(cur / ut_dir));

		});

		bfs::remove(bfs::path("/tmp") / testDir);
		EXPECT_FALSE(bfs::exists(bfs::path("/tmp") / testDir));
	}

	TEST(NetworkPath, Copy) {
		// test whether a remote session to the local host can be created
		if(system("ssh localhost pwd > /dev/null")) {
			return; // skip this test
		}

		NetworkPath ltmp = NetworkPath("/tmp");
		NetworkPath rtmp = NetworkPath("localhost", "/tmp");

		auto lsrcFile = ltmp / "_ut_copy_test.src";
		auto ltrgFile = ltmp / "_ut_copy_test.trg";

		auto rsrcFile = rtmp / "_ut_copy_test.src";
		auto rtrgFile = rtmp / "_ut_copy_test.trg";

		ASSERT_FALSE(exists(lsrcFile)) << "Test file " << lsrcFile << " already present - please delete file manually!";

		ASSERT_FALSE(exists(ltrgFile)) << "Test file " << ltrgFile << " already present - please delete file manually!";

		// create source file
		system("echo Hello > /tmp/_ut_copy_test.src");
		EXPECT_TRUE(exists(lsrcFile));
		EXPECT_TRUE(exists(rsrcFile));


		// copy local to local
		for_each(make_product_range(toVector(lsrcFile, rsrcFile), toVector(ltrgFile, rtrgFile)), [&](const std::pair<NetworkPath, NetworkPath>& cur) {
			SCOPED_TRACE(cur);

			EXPECT_TRUE(exists(lsrcFile));
			EXPECT_FALSE(exists(ltrgFile));

			copy(cur.first, cur.second);
			copy(lsrcFile, ltrgFile);

			EXPECT_TRUE(exists(lsrcFile));
			EXPECT_TRUE(exists(ltrgFile));

			EXPECT_TRUE(remove(ltrgFile));
		});

		// delete files
		remove(lsrcFile);
		remove(ltrgFile);
	}

	#endif

} // end namespace measure
} // end namespace driver
} // end namespace insieme
