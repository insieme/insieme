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

	// memory address
//	int a = 0, *ptr = &a;
//	EXPECT_EQ("cc", numeric_cast<std::string>((size_t)ptr));

	EXPECT_EQ("8", numeric_cast<std::string>('8'));

	EXPECT_EQ(53876.0f, numeric_cast<float>("5.3876e4f"));
	EXPECT_EQ("53876", numeric_cast<std::string>(5.3876e4f));

	EXPECT_EQ(321000l, numeric_cast<long>("321000l"));

	EXPECT_EQ(2000LL, numeric_cast<long long>("2000LL"));
	EXPECT_EQ(2000ll, numeric_cast<long long>("2000LL"));


}
