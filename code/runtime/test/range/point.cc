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

#include "range/point.h"
#include "range/impl/point.impl.h"

char _buffer_str[500];
#define toString(printer, object) ((*printer)(_buffer_str, 500, (object)), _buffer_str)
#define toStrP1(point) toString(irt_range_point_1d_snprint, (point))
#define toStrP2(point) toString(irt_range_point_2d_snprint, (point))
#define toStrP3(point) toString(irt_range_point_3d_snprint, (point))

TEST(Point, Basic) {
	irt_range_point_3d p = {{1, 2, 3}};

	EXPECT_EQ(1, p.x);
	EXPECT_EQ(2, p.y);
	EXPECT_EQ(3, p.z);

	EXPECT_EQ(1, p.s[0]);
	EXPECT_EQ(2, p.s[1]);
	EXPECT_EQ(3, p.s[2]);

	EXPECT_STREQ("[1,2,3]", toStrP3(p));

	// update the value
	p = irt_range_point_3d_create(2, 4, 6);

	EXPECT_EQ(2, p.x);
	EXPECT_EQ(4, p.y);
	EXPECT_EQ(6, p.z);

	EXPECT_EQ(2, p.s[0]);
	EXPECT_EQ(4, p.s[1]);
	EXPECT_EQ(6, p.s[2]);

	EXPECT_STREQ("[2,4,6]", toStrP3(p));

	// use a direct instantiation
	p = (irt_range_point_3d){{5, 10, 20}};

	EXPECT_EQ(5, p.x);
	EXPECT_EQ(10, p.y);
	EXPECT_EQ(20, p.z);

	EXPECT_EQ(5, p.s[0]);
	EXPECT_EQ(10, p.s[1]);
	EXPECT_EQ(20, p.s[2]);

	EXPECT_STREQ("[5,10,20]", toStrP3(p));
}

TEST(Point, Print) {
	irt_range_point_1d p1 = {{1}};
	irt_range_point_2d p2 = {{2, 3}};
	irt_range_point_3d p3 = {{4, 5, 6}};

	EXPECT_STREQ("1", toStrP1(p1));
	EXPECT_STREQ("[2,3]", toStrP2(p2));
	EXPECT_STREQ("[4,5,6]", toStrP3(p3));
}


TEST(Point, Equal) {
	irt_range_point_1d p1 = {{1}};
	irt_range_point_2d p2 = {{2, 3}};
	irt_range_point_3d p3 = {{4, 5, 6}};

	EXPECT_EQ(1, p1.x);
	EXPECT_EQ(2, p2.x);
	EXPECT_EQ(3, p2.y);
	EXPECT_EQ(4, p3.x);
	EXPECT_EQ(5, p3.y);
	EXPECT_EQ(6, p3.z);

	EXPECT_TRUE(irt_range_point_1d_eq(p1, irt_range_point_1d_create(1)));
	EXPECT_TRUE(irt_range_point_2d_eq(p2, irt_range_point_2d_create(2, 3)));
	EXPECT_TRUE(irt_range_point_3d_eq(p3, irt_range_point_3d_create(4, 5, 6)));
}

TEST(Point, Additon) {
	irt_range_point_2d a = {{1, 2}};
	irt_range_point_2d b = {{3, 4}};

	EXPECT_STREQ("[4,6]", toStrP2(irt_range_point_2d_add(a, b)));
}
