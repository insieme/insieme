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

#pragma once

#include <stdio.h>
#include "range/point.h"
#include "range/term.h"


typedef struct {
	uint32 num_terms;
	irt_range_term_1d* terms;
	irt_range_term_1d localTerms[];
} irt_range_formula_1d;


typedef struct {
	uint32 num_terms;
	irt_range_term_2d* terms;
	irt_range_term_2d localTerms[];
} irt_range_formula_2d;

typedef struct {
	uint32 num_terms;
	irt_range_term_3d* terms;
	irt_range_term_3d localTerms[];
} irt_range_formula_3d;


// ---- Creation -------
irt_range_formula_1d* irt_range_formula_1d_empty();
irt_range_formula_2d* irt_range_formula_2d_empty();

irt_range_formula_1d* irt_range_formula_1d_create_from(irt_range_term_1d term);
irt_range_formula_1d* irt_range_formula_1d_create(const irt_range_term_1d* term);

irt_range_formula_2d* irt_range_formula_2d_create_from(irt_range_term_2d term);
irt_range_formula_2d* irt_range_formula_2d_create(const irt_range_term_2d* term);


// ---- Destruction -------
void irt_range_formula_1d_clear(irt_range_formula_1d* formula);
void irt_range_formula_2d_clear(irt_range_formula_2d* formula);


// ---- Operations -------

bool irt_range_formula_1d_contains(irt_range_formula_1d* a, irt_range_point_1d point);
bool irt_range_formula_2d_contains(irt_range_formula_2d* a, irt_range_point_2d point);

bool irt_range_formula_1d_is_empty(irt_range_formula_1d* a);
bool irt_range_formula_2d_is_empty(irt_range_formula_2d* a);

uint64 irt_range_formula_1d_cardinality(irt_range_formula_1d* a);
uint64 irt_range_formula_2d_cardinality(irt_range_formula_2d* a);

irt_range_formula_1d* irt_range_formula_1d_union(irt_range_formula_1d* a, irt_range_formula_1d* b);
irt_range_formula_2d* irt_range_formula_2d_union(irt_range_formula_2d* a, irt_range_formula_2d* b);

irt_range_formula_1d* irt_range_formula_1d_intersect(irt_range_formula_1d* a, irt_range_formula_1d* b);
irt_range_formula_2d* irt_range_formula_2d_intersect(irt_range_formula_2d* a, irt_range_formula_2d* b);

irt_range_formula_1d* irt_range_formula_1d_set_diff(irt_range_formula_1d* a, irt_range_formula_1d* b);
irt_range_formula_2d* irt_range_formula_2d_set_diff(irt_range_formula_2d* a, irt_range_formula_2d* b);

irt_range_term_1d irt_range_formula_1d_bounds(irt_range_formula_1d* a);
irt_range_term_2d irt_range_formula_2d_bounds(irt_range_formula_2d* a);

// ---- Printing -------

int irt_range_formula_1d_snprint(char* str, size_t size, const irt_range_formula_1d* formula);
int irt_range_formula_2d_snprint(char* str, size_t size, const irt_range_formula_2d* formula);
int irt_range_formula_3d_snprint(char* str, size_t size, const irt_range_formula_3d* formula);

int irt_range_formula_1d_print(const irt_range_formula_1d* formula);
int irt_range_formula_2d_print(const irt_range_formula_2d* formula);
int irt_range_formula_3d_print(const irt_range_formula_3d* formula);
