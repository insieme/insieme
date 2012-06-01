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

#include "range/formula.h"
#include "range/impl/formula.impl.h"

char _buffer_str[500];
#define toString(printer, object) ((*printer)(_buffer_str,500,(object)), _buffer_str)
#define toStrP1(point) toString(irt_range_point_1d_snprint, (point))
#define toStrP2(point) toString(irt_range_point_2d_snprint, (point))
#define toStrP3(point) toString(irt_range_point_3d_snprint, (point))
#define toStrR1(range) toString(irt_range_formula_1d_snprint, (range))
#define toStrR2(range) toString(irt_range_formula_2d_snprint, (range))
#define toStrR3(range) toString(irt_range_formula_3d_snprint, (range))


TEST(Range, Range2D) {

	// create an empty formula
	irt_range_formula_2d* r = irt_range_formula_2d_empty();
	EXPECT_STREQ("0", toStrR2(r));
	irt_range_formula_2d_clear(r);


	// create a formula containing a term
	irt_range_term_2d term = irt_range_term_2d_create(
		(irt_range_point_2d){ 1, 2 },
		(irt_range_point_2d){ 3, 4 },
		(irt_range_point_2d){ 5, 6 }
	);

	r = irt_range_formula_2d_create(term);
	EXPECT_STREQ("[1,2] .. [3,4] : [5,6]", toStrR2(r));
	irt_range_formula_2d_clear(r);

}

TEST(Range, Contains2D) {

	// create an empty formula
	irt_range_formula_2d* r = irt_range_formula_2d_empty();
	EXPECT_STREQ("0", toStrR2(r));
	EXPECT_FALSE(irt_range_formula_2d_contains(r, irt_range_point_2d_create(10,10)));
	irt_range_formula_2d_clear(r);

	// create a formula containing a term
	irt_range_term_2d term = irt_range_term_2d_create(
		(irt_range_point_2d){ 1, 2 },
		(irt_range_point_2d){ 3, 4 },
		(irt_range_point_2d){ 5, 6 }
	);

	r = irt_range_formula_2d_create(term);
	EXPECT_STREQ("[1,2] .. [3,4] : [5,6]", toStrR2(r));
	EXPECT_TRUE(irt_range_formula_2d_contains(r, irt_range_point_2d_create(1,2)));
	irt_range_formula_2d_clear(r);

}

TEST(Range, Union2D) {

	// form a complex 2D set
	irt_range_formula_2d* a = irt_range_formula_2d_create(
			irt_range_term_2d_create(
					(irt_range_point_2d){  0,  1 },
					(irt_range_point_2d){ 10, 12 },
					(irt_range_point_2d){  2,  3 }
			)
	);

	irt_range_formula_2d* b = irt_range_formula_2d_create(
			irt_range_term_2d_create(
					(irt_range_point_2d){ 15, 19 },
					(irt_range_point_2d){ 20, 24 },
					(irt_range_point_2d){  1,  2 }
			)
	);

	irt_range_formula_2d* c = irt_range_formula_2d_union(a, b);
	EXPECT_STREQ("[0,1] .. [10,12] : [2,3] v [15,19] .. [20,24] : [1,2]", toStrR2(c));

	for(int i=-10; i<50; i++) {
		for(int j=-10; j<50; j++) {
			EXPECT_EQ(
					( 0 <= i && i < 10 && (i- 0) % 2 == 0 &&  1 <= j && j < 12 && (j- 1) % 3 == 0) ||
					(15 <= i && i < 20 && (i-15) % 1 == 0 && 19 <= j && j < 24 && (j-19) % 2 == 0),
					irt_range_formula_2d_contains(c, irt_range_point_2d_create(i,j))
			) << "(i,j) = " << "(" << i << "," << j << ")";
		}
	}

	irt_range_formula_2d_clear(a);
	irt_range_formula_2d_clear(b);
	irt_range_formula_2d_clear(c);

}
