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

#include "insieme/utils/functional_chain.h"

#include <vector>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"


namespace insieme {
namespace utils {

	using namespace std;

	TEST(FunctionChain, Basic) {

		vector<int> data;

		auto f1 = [&]() { data.push_back(1); };
		auto f2 = [&]() { data.push_back(2); };

		auto f = chain(f1, f2);
		f();
		EXPECT_EQ("[1,2]", toString(data));

		data.clear();
		chain(f2,f1,f1,f2)();
		EXPECT_EQ("[2,1,1,2]", toString(data));

	}

	TEST(FunctionChain, Advanced) {

		vector<int> data;

		auto f1 = [&](int x) { data.push_back(x+1); };
		auto f2 = [&](int x) { data.push_back(x+2); };

		chain(f1, f2)(3);
		EXPECT_EQ("[4,5]", toString(data));

		data.clear();
		chain(f2,f1,f1,f2)(10);
		EXPECT_EQ("[12,11,11,12]", toString(data));
	}

} // end namespace utils
} // end namespace insieme
