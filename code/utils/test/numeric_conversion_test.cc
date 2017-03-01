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

#include <iostream>

#include "insieme/utils/numeric_cast.h"

using namespace std;
using namespace insieme::utils;


TEST(NumericConversion, FromString) {
	EXPECT_EQ(static_cast<int>(16), numeric_cast<int>(16));
	EXPECT_EQ("16", numeric_cast<std::string>(16));

	EXPECT_EQ(static_cast<unsigned int>(16), numeric_cast<unsigned int>("16"));
	// hexadecimal number
	EXPECT_EQ(static_cast<unsigned short>(16), numeric_cast<unsigned short>("0x10"));

	EXPECT_EQ(-8, numeric_cast<int>("-8"));
	// octal number
	EXPECT_EQ(-8, numeric_cast<short>("-010"));

	EXPECT_EQ(0u, numeric_cast<unsigned>("0u"));
	EXPECT_EQ(0, numeric_cast<int>("0u"));
	EXPECT_EQ(0, numeric_cast<int64_t>("0u"));
	EXPECT_EQ(0, numeric_cast<int64_t>("0l"));
	EXPECT_EQ(0, numeric_cast<int64_t>("-0"));
	EXPECT_EQ(0, numeric_cast<int64_t>("-0u"));
	EXPECT_EQ(0, numeric_cast<int64_t>("-0l"));

	EXPECT_EQ(0, numeric_cast<int64_t>("-0ll"));
	EXPECT_EQ(0, numeric_cast<int64_t>("-0ull"));


	EXPECT_EQ(10u, numeric_cast<unsigned>("10u"));
	EXPECT_EQ(10, numeric_cast<int>("10u"));
	EXPECT_EQ(10, numeric_cast<int64_t>("10u"));
	EXPECT_EQ(10, numeric_cast<int64_t>("10l"));
	EXPECT_EQ(-10, numeric_cast<int64_t>("-10"));
	EXPECT_EQ(10, numeric_cast<int64_t>("10u"));
	EXPECT_EQ(-10, numeric_cast<int64_t>("-10l"));

	EXPECT_EQ(-10, numeric_cast<int64_t>("-10ll"));
	EXPECT_EQ(10, numeric_cast<int64_t>("10ull"));


	// memory address
	//	int a = 0, *ptr = &a;
	//	EXPECT_EQ("cc", numeric_cast<std::string>((size_t)ptr));

	EXPECT_EQ("8", numeric_cast<std::string>('8'));

	EXPECT_EQ(53876.0f, numeric_cast<float>("5.3876e4f"));
	EXPECT_EQ("53876.0f", numeric_cast<std::string>(5.3876e4f));

	EXPECT_EQ(321000l, numeric_cast<long>("321000l"));

	EXPECT_EQ(2000LL, numeric_cast<long long>("2000LL"));
	EXPECT_EQ(2000ll, numeric_cast<long long>("2000LL"));

	EXPECT_EQ("1.0f", numeric_cast<std::string>(1.0f));
	EXPECT_EQ("1.5f", numeric_cast<std::string>(1.5f));
	EXPECT_EQ(1.0f, numeric_cast<float>("1.0f"));

	EXPECT_EQ(32, numeric_cast<int>("' '"));
	EXPECT_EQ(65, numeric_cast<int>("'A'"));
	EXPECT_EQ(0, numeric_cast<int>("'\\0'"));
}
