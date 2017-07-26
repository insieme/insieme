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

#include <stdio.h>
#include "range/term.h"

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))


irt_range_term_1d irt_range_term_1d_create(irt_range_point_1d start, irt_range_point_1d end, irt_range_point_1d step) {
	return (irt_range_term_1d){start, end, step};
}

irt_range_term_2d irt_range_term_2d_create(irt_range_point_2d start, irt_range_point_2d end, irt_range_point_2d step) {
	return (irt_range_term_2d){start, end, step};
}

irt_range_term_3d irt_range_term_3d_create(irt_range_point_3d start, irt_range_point_3d end, irt_range_point_3d step) {
	return (irt_range_term_3d){start, end, step};
}


irt_range_term_1d irt_range_term_1d_create_direct(irt_range_int start, irt_range_int end, irt_range_int step) {
	return irt_range_term_1d_create(irt_range_point_1d_create(start), irt_range_point_1d_create(end), irt_range_point_1d_create(step));
}


bool irt_range_term_1d_contains(irt_range_term_1d* a, irt_range_point_1d point) {
	return (a->start.x <= point.x && point.x < a->end.x && (point.x - a->start.x) % a->step.x == 0);
}
bool irt_range_term_2d_contains(irt_range_term_2d* a, irt_range_point_2d point) {
	return (a->start.x <= point.x && point.x < a->end.x && (point.x - a->start.x) % a->step.x == 0)
	       && (a->start.y <= point.y && point.y < a->end.y && (point.y - a->start.y) % a->step.y == 0);
}
bool irt_range_term_3d_contains(irt_range_term_3d* a, irt_range_point_3d point) {
	return (a->start.x <= point.x && point.x < a->end.x && (point.x - a->start.x) % a->step.x == 0)
	       && (a->start.y <= point.y && point.y < a->end.y && (point.y - a->start.y) % a->step.y == 0)
	       && (a->start.z <= point.z && point.z < a->end.z && (point.z - a->start.z) % a->step.z == 0);
}


bool irt_range_term_1d_is_empty(irt_range_term_1d* a) {
	return a->start.x >= a->end.x;
}
bool irt_range_term_2d_is_empty(irt_range_term_2d* a) {
	return a->start.x >= a->end.x || a->start.y >= a->end.y;
}
bool irt_range_term_3d_is_empty(irt_range_term_3d* a) {
	return a->start.x >= a->end.x || a->start.y >= a->end.y || a->start.z >= a->end.z;
}

uint64 irt_range_term_1d_cardinality(irt_range_term_1d* a) {
	if(irt_range_term_1d_is_empty(a)) { return 0; }
	return ((a->end.x - a->start.x - 1) / a->step.x) + 1;
}
uint64 irt_range_term_2d_cardinality(irt_range_term_2d* a) {
	if(irt_range_term_2d_is_empty(a)) { return 0; }
	return ((a->end.x - a->start.x - 1) / a->step.x + 1) * ((a->end.y - a->start.y - 1) / a->step.y + 1);
}
uint64 irt_range_term_3d_cardinality(irt_range_term_3d* a) {
	if(irt_range_term_3d_is_empty(a)) { return 0; }
	return ((a->end.x - a->start.x - 1) / a->step.x + 1) * ((a->end.y - a->start.y - 1) / a->step.y + 1) * ((a->end.z - a->start.z - 1) / a->step.z + 1);
}


// --- intersection ---

typedef struct {
	irt_range_int a;
	irt_range_int b;
} irt_int2;

typedef struct {
	irt_range_int a;
	irt_range_int b;
	irt_range_int c;
} irt_int3;

static irt_int3 _ext_euclidean_algorithm(irt_range_int a, irt_range_int b) {
	// based on: http://de.wikipedia.org/wiki/Erweiterter_euklidischer_Algorithmus

	// base case
	if(b == 0) return (irt_int3){a, 1, 0};
	// step case
	irt_int3 res = _ext_euclidean_algorithm(b, a % b);
	return (irt_int3){res.a, res.c, res.b - a / b * res.c};
}


/**
 * Let
 * 		m | x + a
 * be the set of all x which, if adding a to them, can be evenly
 * divided by m. This function is computing the conjunction (intersection)
 * of two such sets - if it exist (might be empty)
 *
 * It computes
 *    m | x + a  and  n | x + b
 * by returning an o and a c such that
 *       o | x + c  ==  m | x + a   and  n | x + b
 *   and 0 <= c < o
 * if such parameters exist. If not, (o,c) == (0,0).
 */
irt_int2 _irt_range_mod_constraint_conjunction(irt_range_int m, irt_range_int a, irt_range_int n, irt_range_int b) {
	// compute g = gcd(n,m) and coefficients p and q such that
	//   g = p * n + q * m
	irt_int3 res = _ext_euclidean_algorithm(n, m);
	irt_range_int g = res.a;
	irt_range_int p = res.b;
	irt_range_int q = res.c;

	// check whether intersection is empty
	if((a - b) % g != 0) {
		// empty intersection
		return (irt_int2){0, 0};
	}

	// make sure internal constraint is satisfied
	assert(((p * n * a + q * m * b) % g) == 0 && "Offset not multiple of gcd!");

	// compute new step size o and offset c
	irt_range_int o = (m * n) / g;
	irt_range_int c = -(((p * n * a + q * m * b) / g) % o);

	// normalize offset (0 <= c < step_size)
	if(c < 0) { c += o; }
	assert(0 <= c && c < o);

	// return aggregated set
	return (irt_int2){o, c};
}


static irt_int3 _irt_range_term_intersect(irt_range_int a_start, irt_range_int a_end, irt_range_int a_step, irt_range_int b_start, irt_range_int b_end,
                                          irt_range_int b_step) {
	// estimate new new start / end positions
	irt_range_int r_start = MAX(a_start, b_start);
	irt_range_int r_end = MIN(a_end, b_end);

	// check whether result is empty
	if(r_start >= r_end) { return (irt_int3){0, 0, 1}; }

	// Lemma:
	//    m | ax + b  and  n | cx + d  ==  mn | gx + pnb + qmd  and  g | cd - ad
	//  where
	//    g = gcd(an,cm) = pan + qcm
	//
	// Here, we use a relaxed version by setting up an equation system
	//   m | x + a  and  n | x + b
	// and computing
	//   o | x + c  ==  mn/g | x + (pna + gmb) / g
	// in case
	//	 g | a - b
	// is satisfied.

	// compute o and c
	irt_int2 oc = _irt_range_mod_constraint_conjunction(a_step, -a_start % a_step, b_step, -b_start % b_step);
	irt_range_int o = oc.a;
	irt_range_int c = oc.b;

	// check whether intersection is empty
	if(o == 0) {
		return (irt_int3){0, 0, 1}; // conjunction is empty!
	}

	// get normalized current offset of start value
	irt_range_int cur_offset = r_start % o;
	if(cur_offset < 0) { cur_offset += o; }
	assert(0 <= cur_offset && cur_offset < o);

	// move start to correct offset
	r_start += -cur_offset + c + ((c < cur_offset) ? (o) : 0);

	// check whether result is empty
	if(r_start >= r_end) { return (irt_int3){0, 0, 1}; }

	// return result
	return (irt_int3){r_start, r_end, o};
}

irt_range_term_1d irt_range_term_1d_intersect(irt_range_term_1d* a, irt_range_term_1d* b) {
	irt_int3 x = _irt_range_term_intersect(a->start.x, a->end.x, a->step.x, b->start.x, b->end.x, b->step.x);

	return irt_range_term_1d_create(irt_range_point_1d_create(x.a), irt_range_point_1d_create(x.b), irt_range_point_1d_create(x.c));
}

irt_range_term_2d irt_range_term_2d_intersect(irt_range_term_2d* a, irt_range_term_2d* b) {
	irt_int3 x = _irt_range_term_intersect(a->start.x, a->end.x, a->step.x, b->start.x, b->end.x, b->step.x);
	irt_int3 y = _irt_range_term_intersect(a->start.y, a->end.y, a->step.y, b->start.y, b->end.y, b->step.y);

	return irt_range_term_2d_create(irt_range_point_2d_create(x.a, y.a), irt_range_point_2d_create(x.b, y.b), irt_range_point_2d_create(x.c, y.c));
}

irt_range_term_3d irt_range_term_3d_intersect(irt_range_term_3d* a, irt_range_term_3d* b) {
	irt_int3 x = _irt_range_term_intersect(a->start.x, a->end.x, a->step.x, b->start.x, b->end.x, b->step.x);
	irt_int3 y = _irt_range_term_intersect(a->start.y, a->end.y, a->step.y, b->start.y, b->end.y, b->step.y);
	irt_int3 z = _irt_range_term_intersect(a->start.z, a->end.z, a->step.z, b->start.z, b->end.z, b->step.z);

	return irt_range_term_3d_create(irt_range_point_3d_create(x.a, y.a, z.a), irt_range_point_3d_create(x.b, y.b, z.b),
	                                irt_range_point_3d_create(x.c, y.c, z.c));
}


int irt_range_term_1d_print(irt_range_term_1d* a) {
	int sum = 0;
	sum += irt_range_point_1d_print(a->start);
	sum += printf(" .. ");
	sum += irt_range_point_1d_print(a->end);
	sum += printf(" : ");
	sum += irt_range_point_1d_print(a->step);
	return sum;
}

int irt_range_term_2d_print(irt_range_term_2d* a) {
	int sum = 0;
	sum += irt_range_point_2d_print(a->start);
	sum += printf(" .. ");
	sum += irt_range_point_2d_print(a->end);
	sum += printf(" : ");
	sum += irt_range_point_2d_print(a->step);
	return sum;
}

int irt_range_term_3d_print(irt_range_term_3d* a) {
	int sum = 0;
	sum += irt_range_point_3d_print(a->start);
	sum += printf(" .. ");
	sum += irt_range_point_3d_print(a->end);
	sum += printf(" : ");
	sum += irt_range_point_3d_print(a->step);
	return sum;
}


int irt_range_term_1d_snprint(char* str, size_t size, irt_range_term_1d* a) {
	int written = 0;
	written += irt_range_point_1d_snprint(str + written, size - written, a->start);
	written += snprintf(str + written, size - written, " .. ");
	written += irt_range_point_1d_snprint(str + written, size - written, a->end);
	written += snprintf(str + written, size - written, " : ");
	written += irt_range_point_1d_snprint(str + written, size - written, a->step);
	return written;
}

int irt_range_term_2d_snprint(char* str, size_t size, irt_range_term_2d* a) {
	int written = 0;
	written += irt_range_point_2d_snprint(str + written, size - written, a->start);
	written += snprintf(str + written, size - written, " .. ");
	written += irt_range_point_2d_snprint(str + written, size - written, a->end);
	written += snprintf(str + written, size - written, " : ");
	written += irt_range_point_2d_snprint(str + written, size - written, a->step);
	return written;
}

int irt_range_term_3d_snprint(char* str, size_t size, irt_range_term_3d* a) {
	int written = 0;
	written += irt_range_point_3d_snprint(str + written, size - written, a->start);
	written += snprintf(str + written, size - written, " .. ");
	written += irt_range_point_3d_snprint(str + written, size - written, a->end);
	written += snprintf(str + written, size - written, " : ");
	written += irt_range_point_3d_snprint(str + written, size - written, a->step);
	return written;
}
