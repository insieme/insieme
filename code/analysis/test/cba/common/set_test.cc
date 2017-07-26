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

#include "insieme/core/ir_builder.h"

#include "insieme/analysis/cba/common/set.h"
#include "insieme/analysis/cba/common/arithmetic_set.h"

namespace insieme {
namespace analysis {
namespace cba {

	TEST(Set,ArithmeticSet) {
		using namespace core;

		NodeManager mgr;
		IRBuilder builder(mgr);

		// check whether the insertion works
		ArithmeticSet s;
		s.insert(arithmetic::Formula(12));

	}

	TEST(Set, LessThan) {

		using set = Set<int>;

		set a;
		set b({ 1, 2, 3 });
		set c = set::getUniversal();

		EXPECT_EQ(a,a);
		EXPECT_EQ(b,b);
		EXPECT_EQ(c,c);

		EXPECT_NE(a,b);
		EXPECT_NE(b,c);
		EXPECT_NE(a,c);

		EXPECT_LT(a,b);
		EXPECT_LT(b,c);
		EXPECT_LT(a,c);

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

