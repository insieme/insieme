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

static irt_range_formula_2d* irt_range_formula_2d_alloc(size_t num_terms) {
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


irt_range_formula_2d* irt_range_formula_2d_empty() {
	return irt_range_formula_2d_alloc(0);
}

irt_range_formula_2d* irt_range_formula_2d_create(irt_range_term_2d term) {
	irt_range_formula_2d* res = irt_range_formula_2d_alloc(1);
	res->terms[0] = term;
	return res;
}

void irt_range_formula_2d_clear(irt_range_formula_2d* formula) {
	if (formula->terms != formula->localTerms) {
		free(formula->terms);
	}
	free(formula);
}


bool irt_range_formula_2d_contains(irt_range_formula_2d* a, irt_range_point_2d point) {
	// search within clauses
	for(int i=0; i<a->num_terms; ++i) {
		if (irt_range_term_2d_contains(a->terms + i, point)) return true;		// found it!
	}
	return false;
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


irt_range_formula_2d* irt_range_formula_2d_union(irt_range_formula_2d* a, irt_range_formula_2d* b) {
	irt_range_formula_2d* res = irt_range_formula_2d_alloc(a->num_terms + b->num_terms);
	int c = 0;
	for(int i=0; i<a->num_terms; ++i) {
		res->terms[c++] = a->terms[i];
	}
	for(int i=0; i<b->num_terms; ++i) {
		res->terms[c++] = b->terms[i];
	}
	return res;
}

irt_range_formula_2d* irt_range_formula_2d_intersect(irt_range_formula_2d* a, irt_range_formula_2d* b) {

	// shortcut in case both sets are the same (no increase in number of terms necessary)
	if (a == b) {
		irt_range_formula_2d* res = irt_range_formula_2d_alloc(a->num_terms);
		for(int i=0; i<a->num_terms; i++) {
			res->terms[i] = a->terms[i];
		}
		return res;
	}

	// compute cross-product
	irt_range_formula_2d* res = irt_range_formula_2d_alloc(a->num_terms * b->num_terms);
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


irt_range_formula_2d* irt_range_formula_2d_set_diff(irt_range_formula_2d* a, irt_range_formula_2d* b);



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
