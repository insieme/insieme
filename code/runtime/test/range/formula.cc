/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "range/formula.h"
#include "range/impl/formula.impl.h"

#include "context/impl/capture.impl.h"
#include "standalone.h"

char _buffer_str[500];
#define toString(printer, object) ((*printer)(_buffer_str, 500, (object)), _buffer_str)
#define toStrP1(point) toString(irt_range_point_1d_snprint, (point))
#define toStrP2(point) toString(irt_range_point_2d_snprint, (point))
#define toStrP3(point) toString(irt_range_point_3d_snprint, (point))
#define toStrT1(range) toString(irt_range_term_1d_snprint, (range))
#define toStrT2(range) toString(irt_range_term_2d_snprint, (range))
#define toStrT3(range) toString(irt_range_term_3d_snprint, (range))
#define toStrR1(range) toString(irt_range_formula_1d_snprint, (range))
#define toStrR2(range) toString(irt_range_formula_2d_snprint, (range))
#define toStrR3(range) toString(irt_range_formula_3d_snprint, (range))

uint64 countMembers(irt_range_formula_1d* f, int lb = 0, int ub = 30) {
	uint64 count = 0;
	for(int i = lb; i < ub; ++i) {
		if(irt_range_formula_1d_contains(f, irt_range_point_1d_create(i))) { count++; }
	}
	return count;
}

uint64 countMembers(irt_range_formula_2d* f, int lb = 0, int ub = 30) {
	uint64 count = 0;
	for(int i = lb; i < ub; ++i) {
		for(int j = lb; j < ub; ++j) {
			if(irt_range_formula_2d_contains(f, irt_range_point_2d_create(i, j))) { count++; }
		}
	}
	return count;
}


TEST(Range, Range2D) {
	// create an empty formula
	irt_range_formula_2d* r = irt_range_formula_2d_empty();
	EXPECT_STREQ("0", toStrR2(r));
	irt_range_formula_2d_clear(r);


	// create a formula containing a term
	irt_range_term_2d term = irt_range_term_2d_create((irt_range_point_2d){1, 2}, (irt_range_point_2d){3, 4}, (irt_range_point_2d){5, 6});

	r = irt_range_formula_2d_create(&term);
	EXPECT_STREQ("[1,2] .. [3,4] : [5,6]", toStrR2(r));
	irt_range_formula_2d_clear(r);
}

TEST(Range, Contains2D) {
	// create an empty formula
	irt_range_formula_2d* r = irt_range_formula_2d_empty();
	EXPECT_STREQ("0", toStrR2(r));
	EXPECT_FALSE(irt_range_formula_2d_contains(r, irt_range_point_2d_create(10, 10)));
	irt_range_formula_2d_clear(r);

	// create a formula containing a term
	irt_range_term_2d term = irt_range_term_2d_create((irt_range_point_2d){1, 2}, (irt_range_point_2d){3, 4}, (irt_range_point_2d){5, 6});

	r = irt_range_formula_2d_create(&term);
	EXPECT_STREQ("[1,2] .. [3,4] : [5,6]", toStrR2(r));
	EXPECT_TRUE(irt_range_formula_2d_contains(r, irt_range_point_2d_create(1, 2)));
	irt_range_formula_2d_clear(r);
}

TEST(Range, Union2D) {
	// form a complex 2D set
	irt_range_formula_2d* a =
	    irt_range_formula_2d_create_from(irt_range_term_2d_create((irt_range_point_2d){0, 1}, (irt_range_point_2d){10, 12}, (irt_range_point_2d){2, 3}));

	irt_range_formula_2d* b =
	    irt_range_formula_2d_create_from(irt_range_term_2d_create((irt_range_point_2d){15, 19}, (irt_range_point_2d){20, 24}, (irt_range_point_2d){1, 2}));

	irt_range_formula_2d* c = irt_range_formula_2d_union(a, b);
	EXPECT_STREQ("[0,1] .. [10,12] : [2,3] v [15,19] .. [20,24] : [1,2]", toStrR2(c));

	for(int i = -10; i < 50; i++) {
		for(int j = -10; j < 50; j++) {
			EXPECT_EQ((0 <= i && i < 10 && (i - 0) % 2 == 0 && 1 <= j && j < 12 && (j - 1) % 3 == 0)
			              || (15 <= i && i < 20 && (i - 15) % 1 == 0 && 19 <= j && j < 24 && (j - 19) % 2 == 0),
			          irt_range_formula_2d_contains(c, irt_range_point_2d_create(i, j)))
			    << "(i,j) = "
			    << "(" << i << "," << j << ")";
		}
	}

	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);
	irt_range_formula_2d_clear(c);
}

TEST(Range, Intersect2D) {
	// form a complex 2D set
	irt_range_formula_2d* a =
	    irt_range_formula_2d_create_from(irt_range_term_2d_create((irt_range_point_2d){0, 1}, (irt_range_point_2d){10, 12}, (irt_range_point_2d){2, 3}));

	irt_range_formula_2d* b =
	    irt_range_formula_2d_create_from(irt_range_term_2d_create((irt_range_point_2d){5, 7}, (irt_range_point_2d){20, 24}, (irt_range_point_2d){1, 2}));

	irt_range_formula_2d* c = irt_range_formula_2d_intersect(a, b);
	EXPECT_STREQ("[6,7] .. [10,12] : [2,6]", toStrR2(c));

	for(int i = -10; i < 50; i++) {
		for(int j = -10; j < 50; j++) {
			EXPECT_EQ((0 <= i && i < 10 && (i - 0) % 2 == 0 && 1 <= j && j < 12 && (j - 1) % 3 == 0)
			              && (5 <= i && i < 20 && (i - 5) % 1 == 0 && 7 <= j && j < 24 && (j - 7) % 2 == 0),
			          irt_range_formula_2d_contains(c, irt_range_point_2d_create(i, j)))
			    << "(i,j) = "
			    << "(" << i << "," << j << ")";
		}
	}


	// a little bit harder (more, yet identical terms)
	irt_range_formula_2d* d = irt_range_formula_2d_union(a, b);
	irt_range_formula_2d* e = irt_range_formula_2d_intersect(d, d);
	EXPECT_STREQ("[0,1] .. [10,12] : [2,3] v [5,7] .. [20,24] : [1,2]", toStrR2(d));
	EXPECT_STREQ("[0,1] .. [10,12] : [2,3] v [5,7] .. [20,24] : [1,2]", toStrR2(e));

	// more, different terms
	irt_range_formula_2d* f = irt_range_formula_2d_union(b, a);
	EXPECT_STREQ("[5,7] .. [20,24] : [1,2] v [0,1] .. [10,12] : [2,3]", toStrR2(f));

	irt_range_formula_2d* g = irt_range_formula_2d_intersect(d, f);
	EXPECT_STREQ("[6,7] .. [10,12] : [2,6] v [0,1] .. [10,12] : [2,3] v [5,7] .. [20,24] : [1,2] v [6,7] .. [10,12] : [2,6]", toStrR2(g));


	for(int i = -10; i < 50; i++) {
		for(int j = -10; j < 50; j++) {
			EXPECT_EQ(irt_range_formula_2d_contains(d, irt_range_point_2d_create(i, j)) && irt_range_formula_2d_contains(f, irt_range_point_2d_create(i, j)),
			          irt_range_formula_2d_contains(g, irt_range_point_2d_create(i, j)))
			    << "(i,j) = "
			    << "(" << i << "," << j << ")";
		}
	}


	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);
	irt_range_formula_2d_clear(c);
	irt_range_formula_2d_clear(d);
	irt_range_formula_2d_clear(e);
	irt_range_formula_2d_clear(f);
	irt_range_formula_2d_clear(g);
}


TEST(Range, Cardinality2D) {
	// form a complex 2D set
	irt_range_formula_2d* a =
	    irt_range_formula_2d_create_from(irt_range_term_2d_create((irt_range_point_2d){0, 1}, (irt_range_point_2d){10, 12}, (irt_range_point_2d){2, 3}));
	EXPECT_EQ(5 * 4, irt_range_formula_2d_cardinality(a));
	EXPECT_EQ(countMembers(a), irt_range_formula_2d_cardinality(a));

	irt_range_formula_2d* b =
	    irt_range_formula_2d_create_from(irt_range_term_2d_create((irt_range_point_2d){5, 7}, (irt_range_point_2d){20, 24}, (irt_range_point_2d){1, 2}));

	EXPECT_EQ(15 * 9, irt_range_formula_2d_cardinality(b));
	EXPECT_EQ(countMembers(b), irt_range_formula_2d_cardinality(b));

	irt_range_formula_2d* c = irt_range_formula_2d_intersect(a, b);
	EXPECT_STREQ("[6,7] .. [10,12] : [2,6]", toStrR2(c));
	EXPECT_EQ(2 * 1, irt_range_formula_2d_cardinality(c));
	EXPECT_EQ(countMembers(c), irt_range_formula_2d_cardinality(c));


	// a little bit harder (more, yet identical terms)
	irt_range_formula_2d* d = irt_range_formula_2d_union(a, b);
	irt_range_formula_2d* e = irt_range_formula_2d_intersect(d, d);
	EXPECT_STREQ("[0,1] .. [10,12] : [2,3] v [5,7] .. [20,24] : [1,2]", toStrR2(d));
	EXPECT_STREQ("[0,1] .. [10,12] : [2,3] v [5,7] .. [20,24] : [1,2]", toStrR2(e));

	EXPECT_EQ(153, irt_range_formula_2d_cardinality(d));
	EXPECT_EQ(153, irt_range_formula_2d_cardinality(e));
	EXPECT_EQ(countMembers(d), irt_range_formula_2d_cardinality(d));
	EXPECT_EQ(countMembers(e), irt_range_formula_2d_cardinality(e));

	// more, different terms
	irt_range_formula_2d* f = irt_range_formula_2d_union(b, a);
	EXPECT_STREQ("[5,7] .. [20,24] : [1,2] v [0,1] .. [10,12] : [2,3]", toStrR2(f));
	EXPECT_EQ(153, irt_range_formula_2d_cardinality(f));
	EXPECT_EQ(countMembers(f), irt_range_formula_2d_cardinality(f));


	irt_range_formula_2d* g = irt_range_formula_2d_intersect(d, f);
	EXPECT_STREQ("[6,7] .. [10,12] : [2,6] v [0,1] .. [10,12] : [2,3] v [5,7] .. [20,24] : [1,2] v [6,7] .. [10,12] : [2,6]", toStrR2(g));
	EXPECT_EQ(153, irt_range_formula_2d_cardinality(g));
	EXPECT_EQ(countMembers(g), irt_range_formula_2d_cardinality(g));


	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);
	irt_range_formula_2d_clear(c);
	irt_range_formula_2d_clear(d);
	irt_range_formula_2d_clear(e);
	irt_range_formula_2d_clear(f);
	irt_range_formula_2d_clear(g);
}


TEST(Range, SetDiff1D) {
	// create a large and a small set, the larger overlapping the small set
	irt_range_formula_1d* a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(5, 20, 1));

	EXPECT_STREQ("5 .. 20 : 1", toStrR1(a));
	EXPECT_EQ(15, irt_range_formula_1d_cardinality(a));

	irt_range_formula_1d* b = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(7, 15, 3));

	EXPECT_STREQ("7 .. 15 : 3", toStrR1(b));
	EXPECT_EQ(3, irt_range_formula_1d_cardinality(b));

	irt_range_formula_1d* c = irt_range_formula_1d_set_diff(a, b);
	EXPECT_STREQ("5 .. 7 : 1 v 15 .. 20 : 1 v 8 .. 15 : 3 v 9 .. 15 : 3", toStrR1(c));
	EXPECT_EQ(12, irt_range_formula_1d_cardinality(c));
	EXPECT_EQ(countMembers(c), irt_range_formula_1d_cardinality(c));

	irt_range_formula_1d_clear(a);
	irt_range_formula_1d_clear(b);
	irt_range_formula_1d_clear(c);

	// -- subtract all kind of sub-sets --
	a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(5, 20, 1));

	EXPECT_STREQ("5 .. 20 : 1", toStrR1(a));
	EXPECT_EQ(15, irt_range_formula_1d_cardinality(a));

	for(int i = 0; i < 30; i++) {
		for(int j = 0; j < 30; j++) {
			for(int k = 1; k < 30; k++) {
				// create temporary b
				b = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(i, j, k));

				// subtract b from a
				c = irt_range_formula_1d_set_diff(a, b);

				// check memberships
				int count = 0;
				for(int h = 0; h < 30; h++) {
					irt_range_point_1d p = {h};
					ASSERT_EQ(irt_range_formula_1d_contains(a, p) && !irt_range_formula_1d_contains(b, p), irt_range_formula_1d_contains(c, p));
					if(irt_range_formula_1d_contains(c, p)) { count++; }
				}

				// check cardinality
				EXPECT_EQ(count, irt_range_formula_1d_cardinality(c));

				// free temporary values
				irt_range_formula_1d_clear(b);
				irt_range_formula_1d_clear(c);
			}
		}
	}

	irt_range_formula_1d_clear(a);


	// -- use construct with more than one term to be subtracted from --
	a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(5, 10, 1));

	b = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(15, 20, 1));

	c = irt_range_formula_1d_union(a, b);
	irt_range_formula_1d_clear(a);
	irt_range_formula_1d_clear(b);

	a = c;

	EXPECT_STREQ("5 .. 10 : 1 v 15 .. 20 : 1", toStrR1(a));
	EXPECT_EQ(10, irt_range_formula_1d_cardinality(a));

	for(int i = 0; i < 30; i++) {
		for(int j = 0; j < 30; j++) {
			for(int k = 1; k < 30; k++) {
				// create temporary b
				b = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(i, j, k));

				// subtract b from a
				c = irt_range_formula_1d_set_diff(a, b);

				// check memberships
				int count = 0;
				for(int h = 0; h < 30; h++) {
					irt_range_point_1d p = {h};

					ASSERT_EQ(irt_range_formula_1d_contains(a, p) && !irt_range_formula_1d_contains(b, p), irt_range_formula_1d_contains(c, p));
					if(irt_range_formula_1d_contains(c, p)) { count++; }
				}

				// check cardinality
				EXPECT_EQ(count, irt_range_formula_1d_cardinality(c));

				// free temporary values
				irt_range_formula_1d_clear(b);
				irt_range_formula_1d_clear(c);
			}
		}
	}

	irt_range_formula_1d_clear(a);
}


TEST(Range, SetDiff2D) {
	// create a large and a small set, the larger overlapping the small set
	irt_range_formula_2d* a = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(5, 3), irt_range_point_2d_create(20, 22), irt_range_point_2d_create(1, 2)));

	EXPECT_STREQ("[5,3] .. [20,22] : [1,2]", toStrR2(a));
	EXPECT_EQ(15 * 10, irt_range_formula_2d_cardinality(a));

	irt_range_formula_2d* b = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(7, 5), irt_range_point_2d_create(15, 20), irt_range_point_2d_create(3, 4)));

	EXPECT_STREQ("[7,5] .. [15,20] : [3,4]", toStrR2(b));
	EXPECT_EQ(3 * 4, irt_range_formula_2d_cardinality(b));

	irt_range_formula_2d* c = irt_range_formula_2d_set_diff(a, b);
	//	EXPECT_STREQ("[5,3] .. [7,20] : [1,2] v 15 .. 20 : 1 v 8 .. 15 : 3 v 9 .. 15 : 3", toStrR2(c));
	//	EXPECT_EQ(12, irt_range_formula_2d_cardinality(c));
	EXPECT_EQ(countMembers(c), irt_range_formula_2d_cardinality(c));

	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);
	irt_range_formula_2d_clear(c);

	// -- subtract all kind of sub-sets --

	// check all kind of combinations

	a = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(3, 2), irt_range_point_2d_create(7, 8), irt_range_point_2d_create(1, 2)));

	int N = 10;
	for(int i1 = 0; i1 < N; i1++) {
		for(int i2 = 0; i2 < N; i2++) {
			for(int j1 = i1; j1 < N; j1++) {
				for(int j2 = i2; j2 < N; j2++) {
					for(int k1 = 1; k1 < N; k1++) {
						for(int k2 = 1; k2 < N; k2++) {
							// create temporary b
							b = irt_range_formula_2d_create_from(irt_range_term_2d_create(irt_range_point_2d_create(i1, i2), irt_range_point_2d_create(j1, j2),
							                                                              irt_range_point_2d_create(k1, k2)));

							// subtract b from a
							c = irt_range_formula_2d_set_diff(a, b);

							//				// debugging ...
							//				if (c->num_terms > maxTerms) {
							//					maxTerms = c->num_terms;
							//					printf("New max-terms: %d\n", maxTerms);
							//					irt_range_formula_2d_print(c);
							//					printf("\n");
							//				}

							// check memberships
							int count = 0;
							for(int h1 = 0; h1 < N; h1++) {
								for(int h2 = 0; h2 < N; h2++) {
									irt_range_point_2d p = {h1, h2};

									//					if (irt_range_formula_2d_contains(a,p) && !irt_range_formula_2d_contains(b,p) !=
									//irt_range_formula_2d_contains(c,p)) {
									//						printf(" a = "); irt_range_formula_2d_print(a); printf("\n");
									//						printf(" b = "); irt_range_formula_2d_print(b); printf("\n");
									//						printf(" c = "); irt_range_formula_2d_print(c); printf("\n");
									//						printf(" h = (%d,%d)\n", h1, h2);
									//					}

									ASSERT_EQ(irt_range_formula_2d_contains(a, p) && !irt_range_formula_2d_contains(b, p), irt_range_formula_2d_contains(c, p));
									if(irt_range_formula_2d_contains(c, p)) { count++; }
								}
							}

							// check cardinality
							EXPECT_EQ(count, irt_range_formula_2d_cardinality(c));

							// free temporary values
							irt_range_formula_2d_clear(b);
							irt_range_formula_2d_clear(c);
						}
					}
				}
			}
		}
	}

	irt_range_formula_2d_clear(a);


	// -- use construct with more than one term to be subtracted from --
	a = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(3, 2), irt_range_point_2d_create(5, 6), irt_range_point_2d_create(1, 2)));

	b = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(7, 5), irt_range_point_2d_create(9, 8), irt_range_point_2d_create(2, 1)));

	c = irt_range_formula_2d_union(a, b);
	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);

	a = c;

	EXPECT_EQ(2 * 2 + 1 * 3, irt_range_formula_2d_cardinality(a));


	N = 10;
	for(int i1 = 0; i1 < N; i1++) {
		for(int i2 = 0; i2 < N; i2++) {
			for(int j1 = i1; j1 < N; j1++) {
				for(int j2 = i2; j2 < N; j2++) {
					for(int k1 = 1; k1 < N; k1++) {
						for(int k2 = 1; k2 < N; k2++) {
							// create temporary b
							b = irt_range_formula_2d_create_from(irt_range_term_2d_create(irt_range_point_2d_create(i1, i2), irt_range_point_2d_create(j1, j2),
							                                                              irt_range_point_2d_create(k1, k2)));

							// subtract b from a
							c = irt_range_formula_2d_set_diff(a, b);

							//				// debugging ...
							//				if (c->num_terms > maxTerms) {
							//					maxTerms = c->num_terms;
							//					printf("New max-terms: %d\n", maxTerms);
							//					irt_range_formula_2d_print(c);
							//					printf("\n");
							//				}

							// check memberships
							int count = 0;
							for(int h1 = 0; h1 < N; h1++) {
								for(int h2 = 0; h2 < N; h2++) {
									irt_range_point_2d p = {h1, h2};

									//					if (irt_range_formula_2d_contains(a,p) && !irt_range_formula_2d_contains(b,p) !=
									//irt_range_formula_2d_contains(c,p)) {
									//						printf(" a = "); irt_range_formula_2d_print(a); printf("\n");
									//						printf(" b = "); irt_range_formula_2d_print(b); printf("\n");
									//						printf(" c = "); irt_range_formula_2d_print(c); printf("\n");
									//						printf(" h = (%d,%d)\n", h1, h2);
									//					}

									ASSERT_EQ(irt_range_formula_2d_contains(a, p) && !irt_range_formula_2d_contains(b, p), irt_range_formula_2d_contains(c, p));
									if(irt_range_formula_2d_contains(c, p)) { count++; }
								}
							}

							// check cardinality
							EXPECT_EQ(count, irt_range_formula_2d_cardinality(c));

							// free temporary values
							irt_range_formula_2d_clear(b);
							irt_range_formula_2d_clear(c);
						}
					}
				}
			}
		}
	}

	irt_range_formula_2d_clear(a);
}


TEST(Range, Bounds1D) {
	// create a large and a small set, the larger overlapping the small set
	irt_range_formula_1d* a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(4, 15, 4));

	irt_range_term_1d bounds;

	bounds = irt_range_formula_1d_bounds(a);
	EXPECT_STREQ("4 .. 15 : 4", toStrR1(a));
	EXPECT_STREQ("4 .. 13 : 1", toStrT1(&bounds));
	irt_range_formula_1d_clear(a);


	a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(4, 16, 4));
	bounds = irt_range_formula_1d_bounds(a);
	EXPECT_STREQ("4 .. 16 : 4", toStrR1(a));
	EXPECT_STREQ("4 .. 13 : 1", toStrT1(&bounds));
	irt_range_formula_1d_clear(a);

	a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(4, 17, 4));
	bounds = irt_range_formula_1d_bounds(a);
	EXPECT_STREQ("4 .. 17 : 4", toStrR1(a));
	EXPECT_STREQ("4 .. 17 : 1", toStrT1(&bounds));
	irt_range_formula_1d_clear(a);

	a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(4, 18, 4));
	bounds = irt_range_formula_1d_bounds(a);
	EXPECT_STREQ("4 .. 18 : 4", toStrR1(a));
	EXPECT_STREQ("4 .. 17 : 1", toStrT1(&bounds));
	irt_range_formula_1d_clear(a);

	a = irt_range_formula_1d_create_from(irt_range_term_1d_create_direct(4, 19, 4));
	bounds = irt_range_formula_1d_bounds(a);
	EXPECT_STREQ("4 .. 19 : 4", toStrR1(a));
	EXPECT_STREQ("4 .. 17 : 1", toStrT1(&bounds));
	irt_range_formula_1d_clear(a);
}

TEST(Range, Bounds2D) {
	// create a large and a small set, the larger overlapping the small set
	irt_range_formula_2d* a = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(3, 5), irt_range_point_2d_create(5, 8), irt_range_point_2d_create(1, 2)));

	irt_range_formula_2d* b = irt_range_formula_2d_create_from(
	    irt_range_term_2d_create(irt_range_point_2d_create(7, 2), irt_range_point_2d_create(9, 5), irt_range_point_2d_create(2, 1)));

	irt_range_formula_2d* c = irt_range_formula_2d_union(a, b);

	irt_range_term_2d bounds = irt_range_formula_2d_bounds(c);
	EXPECT_STREQ("[3,5] .. [5,8] : [1,2] v [7,2] .. [9,5] : [2,1]", toStrR2(c));
	EXPECT_STREQ("[3,2] .. [8,8] : [1,1]", toStrT2(&bounds));

	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);
	irt_range_formula_2d_clear(c);
}
