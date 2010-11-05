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

#include <vector>
#include <sstream>
#include <algorithm>

#include <gtest/gtest.h>

#include "utils/iterator_utils.h"

using namespace std;

TEST(IteratorUtils, PairedIterator) {

	vector<int> testInt;
	testInt.push_back(15);
	testInt.push_back(26);
	vector<string> testString;
	testString.push_back("A");
	testString.push_back("B");

	auto start = make_paired_iterator(testInt.begin(), testString.begin());
	auto end = make_paired_iterator(testInt.end(), testString.end());

	stringstream ss;
	for_each(start, end, [&ss](pair<int, string> elem) { ss << elem.first << ":" << elem.second << "--"; } );

	EXPECT_EQ(ss.str(), "15:A--26:B--");
}

