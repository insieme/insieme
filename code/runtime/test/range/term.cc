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

#include "range/term.h"
#include "range/impl/term.impl.h"

char _buffer_str[500];
#define toString(printer, object) ((*printer)(_buffer_str, 500, (object)), _buffer_str)
#define toStrP1(point) toString(irt_range_point_1d_snprint, (point))
#define toStrP2(point) toString(irt_range_point_2d_snprint, (point))
#define toStrP3(point) toString(irt_range_point_3d_snprint, (point))
#define toStrT1(term) toString(irt_range_term_1d_snprint, (term))
#define toStrT2(term) toString(irt_range_term_2d_snprint, (term))
#define toStrT3(term) toString(irt_range_term_3d_snprint, (term))


TEST(Term, Basic) {
	irt_range_point_3d a = {{1, 2, 3}};
	irt_range_point_3d b = {{4, 5, 6}};
	irt_range_point_3d c = {{7, 8, 9}};

	irt_range_term_3d term = irt_range_term_3d_create(a, b, c);

	EXPECT_STREQ("[1,2,3] .. [4,5,6] : [7,8,9]", toStrT3(&term));
}


TEST(Term, Contains) {
	irt_range_term_2d term = irt_range_term_2d_create(irt_range_point_2d_create(0, 1), irt_range_point_2d_create(10, 10), irt_range_point_2d_create(2, 3));


	EXPECT_STREQ("[0,1] .. [10,10] : [2,3]", toStrT2(&term));

	EXPECT_TRUE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(0, 1)));
	EXPECT_TRUE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(2, 4)));
	EXPECT_TRUE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(4, 4)));
	EXPECT_TRUE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(6, 7)));

	EXPECT_FALSE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(10, 10)));
	EXPECT_FALSE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(10, 7)));
	EXPECT_FALSE(irt_range_term_2d_contains(&term, irt_range_point_2d_create(4, 10)));
}

TEST(Term, Empty) {
	irt_range_term_1d te1 = irt_range_term_1d_create(irt_range_point_1d_create(0), irt_range_point_1d_create(-10), irt_range_point_1d_create(1));

	irt_range_term_2d te2 = irt_range_term_2d_create(irt_range_point_2d_create(0, 0), irt_range_point_2d_create(0, 5), irt_range_point_2d_create(1, 1));

	irt_range_term_3d te3 =
	    irt_range_term_3d_create(irt_range_point_3d_create(0, 0, 0), irt_range_point_3d_create(0, 5, 7), irt_range_point_3d_create(1, 1, 2));

	EXPECT_TRUE(irt_range_term_1d_is_empty(&te1));
	EXPECT_TRUE(irt_range_term_2d_is_empty(&te2));
	EXPECT_TRUE(irt_range_term_3d_is_empty(&te3));


	// -- non-empty ranges --

	irt_range_term_1d tn1 = irt_range_term_1d_create(irt_range_point_1d_create(0), irt_range_point_1d_create(3), irt_range_point_1d_create(1));

	irt_range_term_2d tn2 = irt_range_term_2d_create(irt_range_point_2d_create(0, 0), irt_range_point_2d_create(3, 5), irt_range_point_2d_create(1, 1));

	irt_range_term_3d tn3 =
	    irt_range_term_3d_create(irt_range_point_3d_create(0, 0, 0), irt_range_point_3d_create(3, 5, 7), irt_range_point_3d_create(1, 1, 2));

	EXPECT_FALSE(irt_range_term_1d_is_empty(&tn1));
	EXPECT_FALSE(irt_range_term_2d_is_empty(&tn2));
	EXPECT_FALSE(irt_range_term_3d_is_empty(&tn3));
}


TEST(Term, Cardinality) {
	irt_range_term_1d te1 = irt_range_term_1d_create(irt_range_point_1d_create(0), irt_range_point_1d_create(-10), irt_range_point_1d_create(1));

	irt_range_term_2d te2 = irt_range_term_2d_create(irt_range_point_2d_create(0, 0), irt_range_point_2d_create(0, 5), irt_range_point_2d_create(1, 1));

	irt_range_term_3d te3 =
	    irt_range_term_3d_create(irt_range_point_3d_create(0, 0, 0), irt_range_point_3d_create(0, 5, 7), irt_range_point_3d_create(1, 1, 2));

	irt_range_term_1d tn1 = irt_range_term_1d_create(irt_range_point_1d_create(0), irt_range_point_1d_create(3), irt_range_point_1d_create(1));

	irt_range_term_2d tn2 = irt_range_term_2d_create(irt_range_point_2d_create(0, 0), irt_range_point_2d_create(3, 5), irt_range_point_2d_create(1, 1));

	irt_range_term_3d tn3 =
	    irt_range_term_3d_create(irt_range_point_3d_create(0, 0, 0), irt_range_point_3d_create(3, 5, 7), irt_range_point_3d_create(1, 1, 2));

	EXPECT_EQ(0, irt_range_term_1d_cardinality(&te1));
	EXPECT_EQ(0, irt_range_term_2d_cardinality(&te2));
	EXPECT_EQ(0, irt_range_term_3d_cardinality(&te3));

	EXPECT_EQ(3, irt_range_term_1d_cardinality(&tn1));
	EXPECT_EQ(3 * 5, irt_range_term_2d_cardinality(&tn2));
	EXPECT_EQ(3 * 5 * 4, irt_range_term_3d_cardinality(&tn3));
}


TEST(Term, Cardinality1d) {
	irt_range_term_1d t;

	t = irt_range_term_1d_create_direct(0, 3, 1);
	EXPECT_STREQ("0 .. 3 : 1", toStrT1(&t));
	EXPECT_EQ(3, irt_range_term_1d_cardinality(&t));

	t = irt_range_term_1d_create_direct(0, 3, 2);
	EXPECT_EQ(2, irt_range_term_1d_cardinality(&t));

	t = irt_range_term_1d_create_direct(1, 3, 2);
	EXPECT_EQ(1, irt_range_term_1d_cardinality(&t));

	t = irt_range_term_1d_create_direct(1, 3, 2);
	EXPECT_EQ(1, irt_range_term_1d_cardinality(&t));

	t = irt_range_term_1d_create_direct(1, 9, 3);
	EXPECT_EQ(3, irt_range_term_1d_cardinality(&t));

	t = irt_range_term_1d_create_direct(0, 9, 3);
	EXPECT_EQ(3, irt_range_term_1d_cardinality(&t));


	for(int i = 0; i < 100; i++) {
		for(int j = 0; j < 100; j++) {
			for(int k = 1; k < 100; k++) {
				// compute cardinality
				t = irt_range_term_1d_create_direct(i, j, k);
				uint64 size = irt_range_term_1d_cardinality(&t);

				uint64 count = 0;
				for(int h = 0; h < 100; h++) {
					if(irt_range_term_1d_contains(&t, irt_range_point_1d_create(h))) { count++; }
				}

				EXPECT_EQ(count == 0, irt_range_term_1d_is_empty(&t));
				EXPECT_EQ(count, size) << "Location: (" << i << "," << j << "," << k << ")";
			}
		}
	}
}

TEST(Term, Cardinality2d) {
	irt_range_term_2d t;

	int N = 12;

	// just check all kind of combinations
	for(int i1 = 0; i1 < N; i1++) {
		for(int i2 = 0; i2 < N; i2++) {
			for(int j1 = 0; j1 < N; j1++) {
				for(int j2 = 0; j2 < N; j2++) {
					for(int k1 = 1; k1 < N; k1++) {
						for(int k2 = 1; k2 < N; k2++) {
							// compute cardinality
							t = irt_range_term_2d_create(irt_range_point_2d_create(i1, i2), irt_range_point_2d_create(j1, j2),
							                             irt_range_point_2d_create(k1, k2));
							uint64 size = irt_range_term_2d_cardinality(&t);

							uint64 count = 0;
							for(int h1 = 0; h1 < N; h1++) {
								for(int h2 = 0; h2 < N; h2++) {
									if(irt_range_term_2d_contains(&t, irt_range_point_2d_create(h1, h2))) { count++; }
								}
							}

							EXPECT_EQ(count == 0, irt_range_term_2d_is_empty(&t));
							EXPECT_EQ(count, size) << "Location: ("
							                          "("
							                       << i1 << "," << i2 << ")"
							                                             ","
							                                             "("
							                       << j1 << "," << j2 << ")"
							                                             ","
							                                             "("
							                       << k1 << "," << k2 << ")"
							                                             ")";
						}
					}
				}
			}
		}
	}
}


TEST(Term, Intersect1D) {
	//	irt_range_term_1d t1 = irt_range_term_1d_create_direct(-12,-9,2);
	//	irt_range_term_1d t2 = irt_range_term_1d_create_direct(-11,-9,1);
	//
	//	EXPECT_STREQ("-10 .. -9 : 2", toStrT1(irt_range_term_1d_intersect(t1,t2)));


	irt_range_term_1d t1 = irt_range_term_1d_create_direct(0, 10, 2);
	irt_range_term_1d t2 = irt_range_term_1d_create_direct(2, 10, 3);
	irt_range_term_1d t3 = irt_range_term_1d_create_direct(5, 9, 5);

	irt_range_term_1d t;

	t = irt_range_term_1d_intersect(&t1, &t1);
	EXPECT_STREQ("0 .. 10 : 2", toStrT1(&t));
	t = irt_range_term_1d_intersect(&t2, &t2);
	EXPECT_STREQ("2 .. 10 : 3", toStrT1(&t));
	t = irt_range_term_1d_intersect(&t3, &t3);
	EXPECT_STREQ("5 .. 9 : 5", toStrT1(&t));

	t = irt_range_term_1d_intersect(&t1, &t2);
	EXPECT_STREQ("2 .. 10 : 6", toStrT1(&t));
	t = irt_range_term_1d_intersect(&t2, &t3);
	EXPECT_STREQ("5 .. 9 : 15", toStrT1(&t));
	t = irt_range_term_1d_intersect(&t1, &t3);
	EXPECT_STREQ("0 .. 0 : 1", toStrT1(&t));

	t = irt_range_term_1d_intersect(&t1, &t2);
	t = irt_range_term_1d_intersect(&t, &t3);
	EXPECT_STREQ("0 .. 0 : 1", toStrT1(&t));

	// check all kind of combinations
	int N = 6;

	// just check all kind of combinations
	for(int i1 = -N; i1 < N; i1++) {
		for(int i2 = -N; i2 < N; i2++) {
			for(int j1 = -N; j1 < N; j1++) {
				for(int j2 = -N; j2 < N; j2++) {
					for(int k1 = 1; k1 < N; k1++) {
						for(int k2 = 1; k2 < N; k2++) {
							// create ranges
							t1 = irt_range_term_1d_create_direct(i1, j1, k1);
							t2 = irt_range_term_1d_create_direct(i2, j2, k2);

							// compute intersection
							t3 = irt_range_term_1d_intersect(&t1, &t2);


							// check intersection
							int count = 0;
							for(int h = -2 * N; h < 2 * N; h++) {
								irt_range_point_1d p = irt_range_point_1d_create(h);

								bool should = irt_range_term_1d_contains(&t1, p) && irt_range_term_1d_contains(&t2, p);
								bool is = irt_range_term_1d_contains(&t3, p);

								if(should != is) {
									printf("Invalid intersection: \n");
									irt_range_term_1d_print(&t1);
									printf(" * ");
									irt_range_term_1d_print(&t2);
									printf(" = ");
									irt_range_term_1d_print(&t3);
									printf("\n");
									printf("Wrong membership of: %d\n", h);
								}


								ASSERT_EQ(should, is);

								if(is) { count++; }
							}

							// check cardinality
							EXPECT_EQ(count, irt_range_term_1d_cardinality(&t3));
						}
					}
				}
			}
		}
	}
}
