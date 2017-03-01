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

#include "insieme/core/lang/type_map.h"

namespace insieme {
namespace core {
namespace lang {

	template <typename T>
	bool isSigned() {
		T x = 0;
		return ((T)(x - 1)) < x;
	}

	TEST(LangBasic, Generation) {
		EXPECT_EQ(1, (type_map<INT, 1>::bits));

		EXPECT_EQ(static_cast<std::size_t>(1), sizeof(type_map<INT, 1>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(2), sizeof(type_map<INT, 2>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(4), sizeof(type_map<INT, 4>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(8), sizeof(type_map<INT, 8>::value_type));

		EXPECT_EQ(static_cast<std::size_t>(1), sizeof(type_map<UINT, 1>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(2), sizeof(type_map<UINT, 2>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(4), sizeof(type_map<UINT, 4>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(8), sizeof(type_map<UINT, 8>::value_type));

		EXPECT_EQ(static_cast<std::size_t>(4), sizeof(type_map<REAL, 4>::value_type));
		EXPECT_EQ(static_cast<std::size_t>(8), sizeof(type_map<REAL, 8>::value_type));

		EXPECT_TRUE((isSigned<type_map<INT, 1>::value_type>()));
		EXPECT_TRUE((isSigned<type_map<INT, 2>::value_type>()));
		EXPECT_TRUE((isSigned<type_map<INT, 4>::value_type>()));
		EXPECT_TRUE((isSigned<type_map<INT, 8>::value_type>()));

		EXPECT_FALSE((isSigned<type_map<UINT, 1>::value_type>()));
		EXPECT_FALSE((isSigned<type_map<UINT, 2>::value_type>()));
		EXPECT_FALSE((isSigned<type_map<UINT, 4>::value_type>()));
		EXPECT_FALSE((isSigned<type_map<UINT, 8>::value_type>()));

		EXPECT_TRUE((isSigned<type_map<REAL, 4>::value_type>()));
		EXPECT_TRUE((isSigned<type_map<REAL, 8>::value_type>()));
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
