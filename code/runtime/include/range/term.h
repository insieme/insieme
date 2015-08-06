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
#ifndef __GUARD_RANGE_TERM_H
#define __GUARD_RANGE_TERM_H

#include <stdio.h>
#include "range/point.h"

typedef struct {
	irt_range_point_1d start;
	irt_range_point_1d end;
	irt_range_point_1d step;
} irt_range_term_1d;

typedef struct {
	irt_range_point_2d start;
	irt_range_point_2d end;
	irt_range_point_2d step;
} irt_range_term_2d;

typedef struct {
	irt_range_point_3d start;
	irt_range_point_3d end;
	irt_range_point_3d step;
} irt_range_term_3d;


// ---- Creation -------

irt_range_term_1d irt_range_term_1d_create(irt_range_point_1d start, irt_range_point_1d end, irt_range_point_1d step);
irt_range_term_2d irt_range_term_2d_create(irt_range_point_2d start, irt_range_point_2d end, irt_range_point_2d step);
irt_range_term_3d irt_range_term_3d_create(irt_range_point_3d start, irt_range_point_3d end, irt_range_point_3d step);

irt_range_term_1d irt_range_term_1d_create_direct(irt_range_int start, irt_range_int end, irt_range_int step);

// ---- Operations -------

bool irt_range_term_1d_contains(irt_range_term_1d* a, irt_range_point_1d point);
bool irt_range_term_2d_contains(irt_range_term_2d* a, irt_range_point_2d point);
bool irt_range_term_3d_contains(irt_range_term_1d* a, irt_range_point_3d point);

uint64 irt_range_term_1d_cardinality(irt_range_term_1d* a);
uint64 irt_range_term_2d_cardinality(irt_range_term_2d* a);
uint64 irt_range_term_3d_cardinality(irt_range_term_3d* a);

bool irt_range_term_1d_is_empty(irt_range_term_1d* a);
bool irt_range_term_2d_is_empty(irt_range_term_2d* a);
bool irt_range_term_3d_is_empty(irt_range_term_3d* a);

irt_range_term_1d irt_range_term_1d_intersect(irt_range_term_1d* a, irt_range_term_1d* b);
irt_range_term_2d irt_range_term_2d_intersect(irt_range_term_2d* a, irt_range_term_2d* b);
irt_range_term_3d irt_range_term_3d_intersect(irt_range_term_3d* a, irt_range_term_3d* b);

// ---- Printing -------

int irt_range_term_1d_print(irt_range_term_1d* a);
int irt_range_term_2d_print(irt_range_term_2d* a);
int irt_range_term_3d_print(irt_range_term_3d* a);

int irt_range_term_1d_snprint(char* str, size_t size, irt_range_term_1d* a);
int irt_range_term_2d_snprint(char* str, size_t size, irt_range_term_2d* a);
int irt_range_term_3d_snprint(char* str, size_t size, irt_range_term_3d* a);

//// ---- Equality -------
//
//inline bool irt_range_term_1d_eq(irt_range_term_1d a, irt_range_term_1d b) {
//	return irt_range_point_1d_eq(a.start, b.start) && irt_range_point_1d_eq(a.end, b.end) && irt_range_point_1d_eq(a.step, b.step);
//}
//inline bool irt_range_term_2d_eq(irt_range_term_2d a, irt_range_term_2d b) {
//	return irt_range_point_2d_eq(a.start, b.start) && irt_range_point_2d_eq(a.end, b.end) && irt_range_point_2d_eq(a.step, b.step);
//}
//inline bool irt_range_term_3d_eq(irt_range_term_3d a, irt_range_term_3d b) {
//	return irt_range_point_3d_eq(a.start, b.start) && irt_range_point_3d_eq(a.end, b.end) && irt_range_point_3d_eq(a.step, b.step);
//}



#endif // ifndef __GUARD_RANGE_TERM_H
