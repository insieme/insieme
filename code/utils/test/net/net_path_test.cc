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

#include <boost/filesystem/operations.hpp>

#include "insieme/utils/net/net_path.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/iterator_utils.h"

namespace insieme {
namespace utils {
namespace net {

	TEST(NetworkPath, Basic) {
		Logger::setLevel(WARNING);

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
		Logger::setLevel(WARNING);

		// test whether a remote session to the local host can be created
		if (system("ssh localhost pwd > /dev/null")) {
			return;		// skip this test
		}

		NetworkPath ltmp = NetworkPath("/tmp");
		NetworkPath rtmp = NetworkPath("localhost", "/tmp");

		EXPECT_TRUE(exists(ltmp));
		EXPECT_TRUE(exists(rtmp));

		EXPECT_TRUE(is_directory(ltmp));
		EXPECT_TRUE(is_directory(ltmp));

		bfs::path ut_dir = "_ut_dir";
		ut_dir /= "sub";

		ASSERT_FALSE(bfs::exists(bfs::path("/tmp") / "_ut_dir"))
			<< "Test directory already exits - please delete /tmp/_ut_dir manually!";

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
			system("touch /tmp/_ut_dir/sub/file");
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

		bfs::remove(bfs::path("/tmp") / "_ut_dir");
		EXPECT_FALSE(bfs::exists(bfs::path("/tmp") / "_ut_dir"));
	}

	TEST(NetworkPath, Copy) {
		Logger::setLevel(WARNING);

		// test whether a remote session to the local host can be created
		if (system("ssh localhost pwd > /dev/null")) {
			return;		// skip this test
		}

		NetworkPath ltmp = NetworkPath("/tmp");
		NetworkPath rtmp = NetworkPath("localhost", "/tmp");

		auto lsrcFile = ltmp / "_ut_copy_test.src";
		auto ltrgFile = ltmp / "_ut_copy_test.trg";

		auto rsrcFile = rtmp / "_ut_copy_test.src";
		auto rtrgFile = rtmp / "_ut_copy_test.trg";

		ASSERT_FALSE(exists(lsrcFile))
			<< "Test file " << lsrcFile << " already present - please delete file manually!";

		ASSERT_FALSE(exists(ltrgFile))
			<< "Test file " << ltrgFile << " already present - please delete file manually!";

		// create source file
		system("echo Hello > /tmp/_ut_copy_test.src");
		EXPECT_TRUE(exists(lsrcFile));
		EXPECT_TRUE(exists(rsrcFile));


		// copy local to local
		for_range(make_product_range(toVector(lsrcFile, rsrcFile), toVector(ltrgFile, rtrgFile)),
				[&](const std::pair<NetworkPath, NetworkPath>& cur) {
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

} // end namespace measure
} // end namespace driver
} // end namespace insieme
