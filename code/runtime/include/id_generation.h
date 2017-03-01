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
#ifndef __GUARD_ID_GENERATION_H
#define __GUARD_ID_GENERATION_H

#include "irt_globals.h"

typedef enum {
	IRT_ID_id_gen_test,
	IRT_ID_lookup_test,
	IRT_ID_channel,
	IRT_ID_client_app,
	IRT_ID_context,
	IRT_ID_data_item,
	IRT_ID_wi_event_register,
	IRT_ID_wg_event_register,
	IRT_ID_work_group,
	IRT_ID_work_item,
	IRT_ID_worker,
} irt_id_type;

#ifdef _GEMS_SIM
#define ID_TYPE_BIT_FIELD uint8 id_type
#else
#define ID_TYPE_BIT_FIELD irt_id_type id_type : 8
#endif

#define IRT_DECLARE_ID_TYPE(__type)                                                                                                                            \
	struct _irt_##__type;                                                                                                                                      \
	struct _irt_##__type##_id {                                                                                                                                \
		union {                                                                                                                                                \
			uint64 full;                                                                                                                                       \
			struct {                                                                                                                                           \
				uint32 index;                                                                                                                                  \
				uint16 thread;                                                                                                                                 \
				uint8 node;                                                                                                                                    \
				ID_TYPE_BIT_FIELD;                                                                                                                             \
			};                                                                                                                                                 \
		};                                                                                                                                                     \
		struct _irt_##__type* cached;                                                                                                                          \
	};                                                                                                                                                         \
	typedef struct _irt_##__type##_id irt_##__type##_id;

#define IRT_MAKE_ID_TYPE(__type)                                                                                                                               \
	static inline irt_##__type##_id irt_generate_##__type##_id(void* generator_id_ptr) {                                                                       \
		irt_##__type##_id id;                                                                                                                                  \
		irt_##__type##_id* gen_id = (irt_##__type##_id*)generator_id_ptr;                                                                                      \
		id.full = gen_id->full;                                                                                                                                \
		gen_id->index++;                                                                                                                                       \
		id.id_type = IRT_ID_##__type;                                                                                                                          \
		id.cached = NULL;                                                                                                                                      \
		return id;                                                                                                                                             \
	}                                                                                                                                                          \
	static inline irt_##__type##_id irt_##__type##_null_id() {                                                                                                 \
		irt_##__type##_id null_id = {{0}, NULL};                                                                                                               \
		/* Note: not setting the id_type here, since the null_id is generic for any id */                                                                      \
		return null_id;                                                                                                                                        \
	}

#define IRT_LOOKUP_GENERATOR_ID_PTR (&(irt_worker_get_current()->generator_id))

#endif // ifndef __GUARD_ID_GENERATION_H
