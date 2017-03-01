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
 *
 */
#include <gtest/gtest.h>

#include "insieme/utils/constraint/assignment.h"

namespace insieme {
namespace utils {
namespace constraint {

	TEST(Assignment, Check) {
		Assignment a;

		TypedSetVariable<int> i1(1);
		TypedSetVariable<float> i2(2);

		EXPECT_EQ("{}", toString(a));

		a[i1].insert(1);
		a[i2].insert(2.3f);

		EXPECT_EQ("{v2={2.3},v1={1}}", toString(a));

		a[i1] = {1, 2, 3};
		a[i2] = {1, 3};
		EXPECT_EQ("{v2={1,3},v1={1,2,3}}", toString(a));
	}

} // end namespace set_constraint
} // end namespace utils
} // end namespace insieme
