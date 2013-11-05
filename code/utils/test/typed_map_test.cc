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

#include "insieme/utils/typed_map.h"

#include <string>
#include <vector>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace utils {

	using namespace std;

	TEST(HeterogenousContainer, Basic) {

		HeterogenousContainer c;

		int& a = c.getInstance<int>();
		a = 10;

		EXPECT_EQ(10, c.getInstance<int>());

		a = 12;
		EXPECT_EQ(12, c.getInstance<int>());

		string s = c.getInstance<string>("hello");
		EXPECT_EQ("hello", s);

		auto& v = c.getInstance<vector<int>>();
		v.push_back(12);
		v.push_back(14);

		EXPECT_EQ("[12,14]", toString(c.getInstance<vector<int>>()));
	}


} // end namespace utils
} // end namespace insieme
