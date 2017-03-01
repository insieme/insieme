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
#pragma once
#ifndef __GUARD_IRT_TYPES_H
#define __GUARD_IRT_TYPES_H

#include "declarations.h"

/* ------------------------------ data structures ----- */

typedef enum _irt_type_kind {
	IRT_T_VOID,
	IRT_T_BOOL,
	IRT_T_INT8,
	IRT_T_INT16,
	IRT_T_INT32,
	IRT_T_INT64,
	IRT_T_UINT8,
	IRT_T_UINT16,
	IRT_T_UINT32,
	IRT_T_UINT64,
	IRT_T_REAL16,
	IRT_T_REAL32,
	IRT_T_REAL64,
	IRT_T_STRUCT = 0xFF00, // complex type start
	IRT_T_UNION,
	IRT_T_FUNC,
	IRT_T_POINTER,
	IRT_T_REFERENCE,
	IRT_T_ARRAY,
	IRT_T_VECTOR,
	IRT_T_VAR_VECTOR,
	IRT_T_CHANNEL,
	IRT_T_BASIC
} irt_type_kind;

// TODO structs for complex types / union

struct _irt_type {
	irt_type_kind kind;
	uint32 bytes;
	uint32 num_components;   // 0 for basic types
	irt_type_id* components; // num_components entries
	size_t* components_offset;
};

/* ------------------------------ operations ----- */

static inline uint32 irt_type_get_bytes(irt_context* context, irt_type_id type_id) {
	if(type_id < 0) { // if <0, it's not an id but -(size)
		return -type_id;
	} else {
		return context->type_table[type_id].bytes;
	}
}

static inline const char* irt_type_kind_get_name(irt_type_kind kind) {
	switch(kind) {
	case IRT_T_VOID: return "IRT_T_VOID";
	case IRT_T_BOOL: return "IRT_T_BOOL";
	case IRT_T_INT8: return "IRT_T_INT8";
	case IRT_T_INT16: return "IRT_T_INT16";
	case IRT_T_INT32: return "IRT_T_INT32";
	case IRT_T_INT64: return "IRT_T_INT64";
	case IRT_T_UINT8: return "IRT_T_UINT8";
	case IRT_T_UINT16: return "IRT_T_UINT16";
	case IRT_T_UINT32: return "IRT_T_UINT32";
	case IRT_T_UINT64: return "IRT_T_UINT64";
	case IRT_T_REAL16: return "IRT_T_REAL16";
	case IRT_T_REAL32: return "IRT_T_REAL32";
	case IRT_T_REAL64: return "IRT_T_REAL64";
	case IRT_T_STRUCT: return "IRT_T_STRUCT";
	case IRT_T_UNION: return "IRT_T_UNION";
	case IRT_T_FUNC: return "IRT_T_FUNC";
	case IRT_T_POINTER: return "IRT_T_POINTER";
	case IRT_T_REFERENCE: return "IRT_T_REFERENCE";
	case IRT_T_ARRAY: return "IRT_T_ARRAY";
	case IRT_T_VECTOR: return "IRT_T_VECTOR";
	case IRT_T_VAR_VECTOR: return "IRT_T_VAR_VECTOR";
	case IRT_T_CHANNEL: return "IRT_T_CHANNEL";
	case IRT_T_BASIC:
		return "IRT_T_BASIC";
	// to silence compiler warnings
	default: return "IRT_T_UNKNOWN";
	}
	return "IRT_T_UNKNOWN_TYPE";
}

#endif // ifndef __GUARD_IRT_TYPES_H
