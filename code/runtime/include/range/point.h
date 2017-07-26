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

#pragma once
#ifndef __GUARD_RANGE_POINT_H
#define __GUARD_RANGE_POINT_H

#include <stdio.h>
#include "irt_inttypes.h"

typedef int64_t irt_range_int;


typedef union {
	irt_range_int s[1];
	struct {
		irt_range_int x;
	};
} irt_range_point_1d;

typedef union {
	irt_range_int s[2];
	struct {
		irt_range_int x;
		irt_range_int y;
	};
} irt_range_point_2d;

typedef union {
	irt_range_int s[3];
	struct {
		irt_range_int x;
		irt_range_int y;
		irt_range_int z;
	};
} irt_range_point_3d;


// ---- Creation -------

#define irt_range_point_1d_create(A)                                                                                                                           \
	(irt_range_point_1d) {{                                                                                                                                     \
		(A)                                                                                                                                                    \
	}}
#define irt_range_point_2d_create(A, B)                                                                                                                        \
	(irt_range_point_2d) {{                                                                                                                                     \
		(A), (B)                                                                                                                                               \
	}}
#define irt_range_point_3d_create(A, B, C)                                                                                                                     \
	(irt_range_point_3d) {{                                                                                                                                     \
		(A), (B), (C)                                                                                                                                          \
	}}


// ---- Printing -------

inline int irt_range_point_1d_print(irt_range_point_1d a) {
	return printf("%ld", a.x);
}
inline int irt_range_point_2d_print(irt_range_point_2d a) {
	return printf("[%ld,%ld]", a.x, a.y);
}
inline int irt_range_point_3d_print(irt_range_point_3d a) {
	return printf("[%ld,%ld,%ld]", a.x, a.y, a.z);
}

inline int irt_range_point_1d_snprint(char* str, size_t size, irt_range_point_1d a) {
	return snprintf(str, size, "%ld", a.x);
}
inline int irt_range_point_2d_snprint(char* str, size_t size, irt_range_point_2d a) {
	return snprintf(str, size, "[%ld,%ld]", a.x, a.y);
}
inline int irt_range_point_3d_snprint(char* str, size_t size, irt_range_point_3d a) {
	return snprintf(str, size, "[%ld,%ld,%ld]", a.x, a.y, a.z);
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
	return (irt_range_point_1d){{a.x + b.x}};
}
inline irt_range_point_2d irt_range_point_2d_add(irt_range_point_2d a, irt_range_point_2d b) {
	return (irt_range_point_2d){{a.x + b.x, a.y + b.y}};
}
inline irt_range_point_3d irt_range_point_3d_add(irt_range_point_3d a, irt_range_point_3d b) {
	return (irt_range_point_3d){{a.x + b.x, a.y + b.y, a.z + b.z}};
}


#endif // ifndef __GUARD_RANGE_POINT_H
