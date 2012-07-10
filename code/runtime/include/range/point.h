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
#include "irt_inttypes.h"

typedef int64_t irt_range_int;


typedef union {
	irt_range_int s[1];
	struct { irt_range_int x; };
} irt_range_point_1d;

typedef union {
	irt_range_int s[2];
	struct { irt_range_int x; irt_range_int y; };
} irt_range_point_2d;

typedef union {
	irt_range_int s[3];
	struct { irt_range_int x; irt_range_int y; irt_range_int z; };
} irt_range_point_3d;


// ---- Creation -------

#define irt_range_point_1d_create(A)     (irt_range_point_1d){(A)}
#define irt_range_point_2d_create(A,B)   (irt_range_point_2d){(A),(B)}
#define irt_range_point_3d_create(A,B,C) (irt_range_point_3d){(A),(B),(C)}


// ---- Printing -------

inline int irt_range_point_1d_print(irt_range_point_1d a) {
	return printf("%d", a.x);
}
inline int irt_range_point_2d_print(irt_range_point_2d a) {
	return printf("[%d,%d]", a.x, a.y);
}
inline int irt_range_point_3d_print(irt_range_point_3d a) {
	return printf("[%d,%d,%d]", a.x, a.y, a.z);
}

inline int irt_range_point_1d_snprint(char* str, size_t size, irt_range_point_1d a) {
	return snprintf(str, size, "%d", a.x);
}
inline int irt_range_point_2d_snprint(char* str, size_t size, irt_range_point_2d a) {
	return snprintf(str, size, "[%d,%d]", a.x, a.y);
}
inline int irt_range_point_3d_snprint(char* str, size_t size, irt_range_point_3d a) {
	return snprintf(str, size, "[%d,%d,%d]", a.x, a.y, a.z);
}

// ---- Equality -------

inline bool irt_range_point_1d_eq(irt_range_point_1d a, irt_range_point_1d b) {
	return a.x == b.x;
}
inline bool irt_range_point_2d_eq(irt_range_point_2d a, irt_range_point_2d b) {
	return a.x == b.x && a.y == b.y;
}
inline bool irt_range_point_3d_eq(irt_range_point_3d a, irt_range_point_3d b) {
	return a.x == b.x && a.y == b.y && a.z == b.z;
}


// ---- Addition -------

inline irt_range_point_1d irt_range_point_1d_add(irt_range_point_1d a, irt_range_point_1d b) {
	return (irt_range_point_1d){a.x + b.x};
}
inline irt_range_point_2d irt_range_point_2d_add(irt_range_point_2d a, irt_range_point_2d b) {
	return (irt_range_point_2d){a.x + b.x, a.y + b.y};
}
inline irt_range_point_3d irt_range_point_3d_add(irt_range_point_3d a, irt_range_point_3d b) {
	return (irt_range_point_3d){a.x + b.x, a.y + b.y, a.z + b.z};
}
