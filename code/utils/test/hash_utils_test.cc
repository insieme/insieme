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

#include <map>
#include <boost/functional/hash.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/hash_utils.h"

using namespace insieme::utils;

TEST(HashUtils, combineHashes) {

	// create some "objects"

	int x = 15;
	long y = 60;
	bool b = true;
	std::string str = "Hello World!";

	std::size_t hash;
	// compare manually and automatically combined hashes
	hash = 0;
	EXPECT_EQ(hash, combineHashes());

	boost::hash_combine(hash, x);
	EXPECT_EQ(hash, combineHashes(x));

	boost::hash_combine(hash, y);
	EXPECT_EQ(hash, combineHashes(x, y));

	boost::hash_combine(hash, b);
	EXPECT_EQ(hash, combineHashes(x,y,b));

	boost::hash_combine(hash, str);
	EXPECT_EQ(hash, combineHashes(x,y,b,str));

}

TEST(HashUtils, hashLists) {

	// create some list of elements
	auto list = toVector(15, 16, 22);

	// hash list manually
	std::size_t hash = 0;
	boost::hash_combine(hash, list[0]);
	boost::hash_combine(hash, list[1]);
	boost::hash_combine(hash, list[2]);

	EXPECT_EQ(hash, hashList(list));

}
