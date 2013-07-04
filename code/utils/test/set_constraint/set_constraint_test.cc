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

#include "insieme/utils/set_constraint/solver.h"

namespace insieme {
namespace utils {
namespace set_constraint {

	TEST(Constraint, Basic) {

		// simple set tests
		Set a = 1;
		Set b = 2;
		Set c = 3;

		std::cout << a << "\n";

		std::cout << subset(a,b) << "\n";

		std::cout << subsetIf(5, a, b, c) << "\n";


		// constraint test
		Constraints problem = {
				elem(3, a),
				subset(a,b),
				subsetIf(5,a,b,c)
		};

		std::cout << problem << "\n";

		// assignment test
		Assignment as;
		as[1] = { 1, 2, 3 };
		as[2] = { 1, 3 };

		std::cout << as << "\n";
	}

	TEST(Constraint, Check) {

		Assignment a;
		a[1] = { 1, 2 };
		a[2] = { 1, 2, 3 };

		Constraint c = subset(1, 2);
		EXPECT_TRUE(c.check(a)) << c;

		c = subset(2,1);
		EXPECT_FALSE(c.check(a)) << c;

		c = elem( 3, 1);
		EXPECT_FALSE(c.check(a)) << c;

		c = elem( 3, 2);
		EXPECT_TRUE(c.check(a)) << c;

		c = subsetIf( 3, 2, 1, 2);
		EXPECT_TRUE(c.check(a)) << c;

		c = subsetIf( 3, 2, 2, 1);
		EXPECT_FALSE(c.check(a)) << c;

		c = subsetIf( 3, 1, 1, 2);
		EXPECT_TRUE(c.check(a)) << c;

		c = subsetIf( 3, 1, 2, 1);
		EXPECT_TRUE(c.check(a)) << c;

	}


	TEST(Solver, Basic) {

		Constraints problem = {
				elem(5,1),
				elem(6,1),
				subset(1,2),
				subset(2,3),
				subset(4,3),

				elem(7,5),
				subsetIf(6,3,5,3)
		};

		auto res = solve(problem);
		std::cout << res << "\n";

		EXPECT_EQ("{5,6}", toString(res[1])) << res;
		EXPECT_EQ("{5,6}", toString(res[2])) << res;
		EXPECT_EQ("{5,6,7}", toString(res[3])) << res;
		EXPECT_TRUE(res[4].empty()) << res;
		EXPECT_EQ("{7}", toString(res[5])) << res;

		// check the individual constraints
		for (const auto& cur : problem) {
			EXPECT_TRUE(cur.check(res)) << cur;
		}

	}

} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
