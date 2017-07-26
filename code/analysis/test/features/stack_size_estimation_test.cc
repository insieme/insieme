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

#include "insieme/analysis/features/stack_size_estimation.h"

#include "insieme/core/ir_builder.h"


namespace insieme {
namespace analysis {
namespace features {

	TEST(StackSize, Basic) {
		core::NodeManager mgr;
		auto& basic = mgr.getLangBasic();
		core::IRBuilder builder(mgr);

		EXPECT_EQ(4,  getTypeSize(basic.getInt4()));
		EXPECT_EQ(8,  getTypeSize(basic.getUInt8()));
		EXPECT_EQ(16, getTypeSize(basic.getInt16()));
		EXPECT_EQ(4,  getTypeSize(basic.getReal4()));
		EXPECT_EQ(8,  getTypeSize(basic.getReal8()));
		EXPECT_EQ(12, getTypeSize(builder.parseType("def struct A { b : int<4>; c : real<8>; }; A")));
		EXPECT_EQ(8,  getTypeSize(builder.parseType("def union A { b : int<4>; c : real<8>; }; A")));
		EXPECT_EQ(40, getTypeSize(builder.parseType("array<int<4>, 10u>")));
	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme
