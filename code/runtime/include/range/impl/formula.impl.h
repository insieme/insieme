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

#include "range/formula.h"
#include "range/impl/term.impl.h"

static irt_range_formula_1d* _irt_range_formula_1d_alloc(uint32 num_terms) {
	// allocate the required memory
	irt_range_formula_1d* res =
			(irt_range_formula_1d*)malloc(
					sizeof(irt_range_formula_1d)
					+ sizeof(irt_range_term_1d) * num_terms
			);
	res->num_terms = num_terms;
	res->terms = res->localTerms;
	return res;
}

static irt_range_formula_2d* _irt_range_formula_2d_alloc(uint32 num_terms) {
	// allocate the required memory
	irt_range_formula_2d* res =
			(irt_range_formula_2d*)malloc(
					sizeof(irt_range_formula_2d)
					+ sizeof(irt_range_term_2d) * num_terms
			);
	res->num_terms = num_terms;
	res->terms = res->localTerms;
	return res;
}


irt_range_formula_1d* irt_range_formula_1d_empty() {
	return _irt_range_formula_1d_alloc(0);
}

irt_range_formula_2d* irt_range_formula_2d_empty() {
	return _irt_range_formula_2d_alloc(0);
}

irt_range_formula_1d* irt_range_formula_1d_create(irt_range_term_1d term) {
	irt_range_formula_1d* res = _irt_range_formula_1d_alloc(1);
	res->terms[0] = term;
	return res;
}

irt_range_formula_2d* irt_range_formula_2d_create(irt_range_term_2d term) {
	irt_range_formula_2d* res = _irt_range_formula_2d_alloc(1);
	res->terms[0] = term;
	return res;
}

void irt_range_formula_1d_clear(irt_range_formula_1d* formula) {
	if (formula->terms != formula->localTerms) {
		free(formula->terms);
	}
	free(formula);
}

void irt_range_formula_2d_clear(irt_range_formula_2d* formula) {
	if (formula->terms != formula->localTerms) {
		free(formula->terms);
	}
	free(formula);
}

bool irt_range_formula_1d_contains(irt_range_formula_1d* a, irt_range_point_1d point) {
	// search within clauses
	for(int i=0; i<a->num_terms; ++i) {
		if (irt_range_term_1d_contains(a->terms + i, point)) return true;		// found it!
	}
	return false;
}

bool irt_range_formula_2d_contains(irt_range_formula_2d* a, irt_range_point_2d point) {
	// search within clauses
	for(int i=0; i<a->num_terms; ++i) {
		if (irt_range_term_2d_contains(a->terms + i, point)) return true;		// found it!
	}
	return false;
}

bool irt_range_formula_1d_is_empty(irt_range_formula_1d* a) {
	// search within clauses
	for(int i=0; i<a->num_terms; ++i) {
		if (!irt_range_term_1d_is_empty(a->terms + i)) return false;	// contains some elements
	}

	// so ... it is empty => drop all terms!
	a->num_terms = 0;

	return true;  // no, its empty
}

bool irt_range_formula_2d_is_empty(irt_range_formula_2d* a) {
	// search within clauses
	for(int i=0; i<a->num_terms; ++i) {
		if (!irt_range_term_2d_is_empty(a->terms + i)) return false;	// contains some elements
	}

	// so ... it is empty => drop all terms!
	a->num_terms = 0;

	return true;  // no, its empty
}

uint64 irt_range_formula_1d_cardinality(irt_range_formula_1d* a) {

	// handle empty set
	if (a->num_terms == 0) {
		return 0;
	}

	// handle convex sets (only one term)
	if (a->num_terms == 1) {
		return irt_range_term_1d_cardinality(&(a->terms[0]));
	}

	// handle unions of convex sets
	// | A + B1 + .. + Bn | = | A | + | B1 + .. + Bn | - | A * ( B1 + .. + Bn ) |

	// compute cardRest = | B1 + .. Bn |
	irt_range_formula_1d tmp = { a->num_terms - 1, a->terms + 1 };
	uint64 cardRest = irt_range_formula_1d_cardinality(&tmp);

	// if A is empty, term 1 and 3 is 0
	if (irt_range_term_1d_is_empty(&a->terms[0])) {
		return cardRest;
	}

	// compute cardIntersect = | A * (B1 + .. + Bn ) |
	irt_range_term_1d intersected[a->num_terms - 1];
	for(int i=1; i<a->num_terms; ++i) {
		intersected[i-1] = irt_range_term_1d_intersect(a->terms[0], a->terms[i]);
	}
	tmp = (irt_range_formula_1d){ a->num_terms - 1, intersected };
	uint64 cardIntersect = irt_range_formula_1d_cardinality(&tmp);

	// compute cardinality using formula
	return irt_range_term_1d_cardinality(&(a->terms[0])) + cardRest - cardIntersect;
}

uint64 irt_range_formula_2d_cardinality(irt_range_formula_2d* a) {

	// handle empty set
	if (a->num_terms == 0) {
		return 0;
	}

	// handle convex sets (only one term)
	if (a->num_terms == 1) {
		return irt_range_term_2d_cardinality(&(a->terms[0]));
	}

	// handle unions of convex sets
	// | A + B1 + .. + Bn | = | A | + | B1 + .. + Bn | - | A * ( B1 + .. + Bn ) |

	// compute cardRest = | B1 + .. Bn |
	irt_range_formula_2d tmp = { a->num_terms - 1, a->terms + 1 };
	uint64 cardRest = irt_range_formula_2d_cardinality(&tmp);

	// if A is empty, term 1 and 3 is 0
	if (irt_range_term_2d_is_empty(&a->terms[0])) {
		return cardRest;
	}

	// compute cardIntersect = | A * (B1 + .. + Bn ) |
	irt_range_term_2d intersected[a->num_terms - 1];
	for(int i=1; i<a->num_terms; ++i) {
		intersected[i-1] = irt_range_term_2d_intersect(a->terms[0], a->terms[i]);
	}
	tmp = (irt_range_formula_2d){ a->num_terms - 1, intersected };
	uint64 cardIntersect = irt_range_formula_2d_cardinality(&tmp);

	// compute cardinality using formula
	return irt_range_term_2d_cardinality(&(a->terms[0])) + cardRest - cardIntersect;
}

irt_range_formula_1d* irt_range_formula_1d_union(irt_range_formula_1d* a, irt_range_formula_1d* b) {
	irt_range_formula_1d* res = _irt_range_formula_1d_alloc(a->num_terms + b->num_terms);
	int c = 0;
	for(int i=0; i<a->num_terms; ++i) {
		res->terms[c++] = a->terms[i];
	}
	for(int i=0; i<b->num_terms; ++i) {
		res->terms[c++] = b->terms[i];
	}
	return res;
}

irt_range_formula_2d* irt_range_formula_2d_union(irt_range_formula_2d* a, irt_range_formula_2d* b) {
	irt_range_formula_2d* res = _irt_range_formula_2d_alloc(a->num_terms + b->num_terms);
	int c = 0;
	for(int i=0; i<a->num_terms; ++i) {
		res->terms[c++] = a->terms[i];
	}
	for(int i=0; i<b->num_terms; ++i) {
		res->terms[c++] = b->terms[i];
	}
	return res;
}

irt_range_formula_1d* irt_range_formula_1d_intersect(irt_range_formula_1d* a, irt_range_formula_1d* b) {

	// shortcut in case both sets are the same (no increase in number of terms necessary)
	if (a == b) {
		irt_range_formula_1d* res = _irt_range_formula_1d_alloc(a->num_terms);
		for(int i=0; i<a->num_terms; i++) {
			res->terms[i] = a->terms[i];
		}
		return res;
	}

	// compute cross-product
	irt_range_formula_1d* res = _irt_range_formula_1d_alloc(a->num_terms * b->num_terms);
	int c = 0;
	for(int i=0; i<a->num_terms; i++) {
		for(int j=0; j<b->num_terms; j++) {
			irt_range_term_1d cur = irt_range_term_1d_intersect(a->terms[i], b->terms[j]);
			if (!irt_range_term_1d_is_empty(&cur)) {  // filter empty terms
				res->terms[c++] = cur;
			}
		}
	}
	res->num_terms = c;
	return res;
}

irt_range_formula_2d* irt_range_formula_2d_intersect(irt_range_formula_2d* a, irt_range_formula_2d* b) {

	// shortcut in case both sets are the same (no increase in number of terms necessary)
	if (a == b) {
		irt_range_formula_2d* res = _irt_range_formula_2d_alloc(a->num_terms);
		for(int i=0; i<a->num_terms; i++) {
			res->terms[i] = a->terms[i];
		}
		return res;
	}

	// compute cross-product
	irt_range_formula_2d* res = _irt_range_formula_2d_alloc(a->num_terms * b->num_terms);
	int c = 0;
	for(int i=0; i<a->num_terms; i++) {
		for(int j=0; j<b->num_terms; j++) {
			irt_range_term_2d cur = irt_range_term_2d_intersect(a->terms[i], b->terms[j]);
			if (!irt_range_term_2d_is_empty(&cur)) {  // filter empty terms
				res->terms[c++] = cur;
			}
		}
	}
	res->num_terms = c;
	return res;
}

// TODO: move this structures to utilities


typedef struct {
	uint32 num_entries;
	irt_int3* entries;
	irt_int3 local_entries[];
} irt_int3_list;

static irt_int3_list* _irt_int3_list_alloc(uint32 num_entries) {
	irt_int3_list* res = (irt_int3_list*)malloc(sizeof(irt_int3_list) + num_entries*sizeof(irt_int3));
	res->num_entries = num_entries;
	res->entries = res->local_entries;
	return res;
}

static irt_int3_list* _irt_range_term_diff (
		irt_range_int a_start, irt_range_int a_end, irt_range_int a_step,
		irt_range_int b_start, irt_range_int b_end, irt_range_int b_step
) {

	assert(a_step > 0 && "Step size A must not be 0!");
	assert(b_step > 0 && "Step size B must not be 0!");

	// test whether sets are even intersecting
	irt_int3 intersection = _irt_range_term_intersect(a_start, a_end, a_step, b_start, b_end, b_step);
	if (intersection.a == 0 && intersection.b == 0) {	// intersection is empty => return a
		// no intersection (based on ranges)
		irt_int3_list* res = _irt_int3_list_alloc(1);
		res->entries[0] = (irt_int3){ a_start, a_end, a_step };
		return res;
	}

	// there is an intersection => multiple terms might have to be returned

	// allocate memory for resulting list of terms (2 for lower / upper part + 2 per step of b)
	irt_int3_list* res = _irt_int3_list_alloc(2 + b_step - 1);

	// fill resulting list
	int counter = 0;

	// 1) part of A that is below any element of b
	if (a_end > b_start) {
		res->entries[counter++] = (irt_int3){a_start, b_start, a_step};
	} else {
		res->num_entries--;		// there is no such fraction
	}

	// 2) part of A that is above any element of b
	if (b_end < a_end) {
		irt_range_int start = b_end - ((b_end - a_start) % a_step);
		if (start < b_end) start += a_step;
		res->entries[counter++] = (irt_int3){start, a_end, a_step};
	} else {
		res->num_entries--;		// there is no such fraction
	}

	// 3) overlapping parts of A and B (multiple)
	for(int i=1; i<b_step; i++) {

		// compute intersection of a and b shifted by i
		irt_int3 cur = _irt_range_term_intersect(a_start, a_end, a_step, b_start + i, b_end, b_step);

		// if intersection is not empty => add
		if (cur.a != 0 && cur.b != 0) {
			res->entries[counter++] = cur;
		} else {
			res->num_entries--;
		}
	}

	// check whether number of terms was right
	assert(counter == res->num_entries);

	// return result
	return res;
}


irt_range_formula_1d* irt_range_formula_1d_set_diff(irt_range_formula_1d* a, irt_range_formula_1d* b) {

	// To compute the difference of unions, the difference can be distributed among
	// the terms as follows:
	//      ( A u B ) \ ( C u D ) = ( A \ C * A \ D ) u ( B \ C * B \ D )
	//
	// This way, the set difference has to be computed on a term-level (e.g. A \ C)

	// TODO: improve memory allocation within this function

	// compute the union
	irt_range_formula_1d* res = irt_range_formula_1d_empty();
	for(int i=0; i<a->num_terms; i++) {
		irt_range_term_1d* A = &(a->terms[i]);

		// compute intersection of the following terms
		irt_range_formula_1d* inner = irt_range_formula_1d_create(a->terms[i]);
		for(int j=0; j<b->num_terms; j++) {
			irt_range_term_1d* B = &(b->terms[j]);

			// compute current term-intersection
			irt_int3_list* diffList = _irt_range_term_diff(
					A->start.x, A->end.x, A->step.x,
					B->start.x, B->end.x, B->step.x
			);

			// convert to term list
			irt_range_formula_1d* cur = _irt_range_formula_1d_alloc(diffList->num_entries);
			for(int k=0; k<diffList->num_entries; k++) {
				cur->terms[k] = (irt_range_term_1d){diffList->entries[k].a, diffList->entries[k].b, diffList->entries[k].c};
			}

			// update inner intersection
			irt_range_formula_1d* newInner = irt_range_formula_1d_intersect(inner, cur);
			irt_range_formula_1d_clear(inner);
			inner = newInner;

			// clear temporal values
			free(diffList);
			irt_range_formula_1d_clear(cur);
		}

		// aggregate result
		irt_range_formula_1d* newRes = irt_range_formula_1d_union(res, inner);
		irt_range_formula_1d_clear(res);
		res = newRes;
	}

	// return result
	return res;
}


irt_range_formula_2d* irt_range_formula_2d_set_diff(irt_range_formula_2d* a, irt_range_formula_2d* b) {

	// For 2D ranges it is a little more complex. The basic formula
	//      ( A u B ) \ ( C u D ) = ( A \ C * A \ D ) u ( B \ C * B \ D )
	// still holds. However, A\C for terms A,C is computed component wise. Hence,
	// 		A\C = (A.x\B.x) x A.y  u  A.x x (A.y\B.y)
	//

	// TODO: improve memory allocation within this function

	// compute the union
	irt_range_formula_2d* res = irt_range_formula_2d_empty();
	for(int i=0; i<a->num_terms; i++) {
		irt_range_term_2d* A = &(a->terms[i]);

		// compute intersection of the following terms
		irt_range_formula_2d* inner = irt_range_formula_2d_create(a->terms[i]);
		for(int j=0; j<b->num_terms; j++) {
			irt_range_term_2d* B = &(b->terms[j]);

			// compute union along dimensions
			irt_range_formula_2d* dimUnion = irt_range_formula_2d_empty();
			{
				// --- Dimension X ---

				// compute current term-intersection
				irt_int3_list* diffList = _irt_range_term_diff(
						A->start.x, A->end.x, A->step.x,
						B->start.x, B->end.x, B->step.x
				);

				// convert to term list
				irt_range_formula_2d* cur = _irt_range_formula_2d_alloc(diffList->num_entries);
				for(int k=0; k<diffList->num_entries; k++) {
					cur->terms[k] = (irt_range_term_2d){
						irt_range_point_2d_create(diffList->entries[k].a, A->start.y),
						irt_range_point_2d_create(diffList->entries[k].b, A->end.y),
						irt_range_point_2d_create(diffList->entries[k].c, A->step.y)
					};
				}

				// update union along dimensions
				irt_range_formula_2d* newDimUnion = irt_range_formula_2d_union(dimUnion, cur);
				irt_range_formula_2d_clear(dimUnion);
				dimUnion = newDimUnion;

				// clear temporal values
				free(diffList);
				irt_range_formula_2d_clear(cur);
			}

			{
				// --- Dimension Y ---

				// compute current term-intersection
				irt_int3_list* diffList = _irt_range_term_diff(
						A->start.y, A->end.y, A->step.y,
						B->start.y, B->end.y, B->step.y
				);

				// convert to term list
				irt_range_formula_2d* cur = _irt_range_formula_2d_alloc(diffList->num_entries);
				for(int k=0; k<diffList->num_entries; k++) {
					cur->terms[k] = (irt_range_term_2d){
						irt_range_point_2d_create(A->start.x, diffList->entries[k].a),
						irt_range_point_2d_create(A->end.x,   diffList->entries[k].b),
						irt_range_point_2d_create(A->step.x,  diffList->entries[k].c)
					};
				}

				// update union along dimensions
				irt_range_formula_2d* newDimUnion = irt_range_formula_2d_union(dimUnion, cur);
				irt_range_formula_2d_clear(dimUnion);
				dimUnion = newDimUnion;

				// clear temporal values
				free(diffList);
				irt_range_formula_2d_clear(cur);
			}

			// update inner intersection
			irt_range_formula_2d* newInner = irt_range_formula_2d_intersect(inner, dimUnion);
			irt_range_formula_2d_clear(inner);
			inner = newInner;

			// clear temporal inner union
			irt_range_formula_2d_clear(dimUnion);
		}

		// aggregate result
		irt_range_formula_2d* newRes = irt_range_formula_2d_union(res, inner);
		irt_range_formula_2d_clear(res);
		res = newRes;
	}

	// return result
	return res;

}

int irt_range_formula_1d_snprint(char* str, size_t size, const irt_range_formula_1d* formula) {

	// handle empty formulas
	if (formula->num_terms == 0) {
		return snprintf(str, size, "0");
	}

	// handle all others
	int written = 0;
	int i=0;
	for(i=0; i<formula->num_terms-1; i++) {
		written += irt_range_term_1d_snprint(str + written, size - written, formula->terms[i]);
		written += snprintf(str + written, size - written, " v ");
	}
	written += irt_range_term_1d_snprint(str + written, size - written, formula->terms[i]);
	return written;
}

int irt_range_formula_2d_snprint(char* str, size_t size, const irt_range_formula_2d* formula) {

	// handle empty formulas
	if (formula->num_terms == 0) {
		return snprintf(str, size, "0");
	}

	// handle all others
	int written = 0;
	int i=0;
	for(i=0; i<formula->num_terms-1; i++) {
		written += irt_range_term_2d_snprint(str + written, size - written, formula->terms[i]);
		written += snprintf(str + written, size - written, " v ");
	}
	written += irt_range_term_2d_snprint(str + written, size - written, formula->terms[i]);
	return written;
}

int irt_range_formula_1d_print(const irt_range_formula_1d* formula) {
	// handle empty formulas
	if (formula->num_terms == 0) {
		return printf("0");
	}

	// handle all others
	int written = 0;
	int i=0;
	for(i=0; i<formula->num_terms-1; i++) {
		written += irt_range_term_1d_print(formula->terms[i]);
		written += printf(" v ");
	}
	written += irt_range_term_1d_print(formula->terms[i]);
	return written;
}

int irt_range_formula_2d_print(const irt_range_formula_2d* formula) {
	// handle empty formulas
	if (formula->num_terms == 0) {
		return printf("0");
	}

	// handle all others
	int written = 0;
	int i=0;
	for(i=0; i<formula->num_terms-1; i++) {
		written += irt_range_term_2d_print(formula->terms[i]);
		written += printf(" v ");
	}
	written += irt_range_term_2d_print(formula->terms[i]);
	return written;
}
