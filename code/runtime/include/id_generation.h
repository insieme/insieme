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
#ifndef __GUARD_ID_GENERATION_H
#define __GUARD_ID_GENERATION_H

#include "irt_globals.h"

typedef enum {
	IRT_ID_id_gen_test, IRT_ID_lookup_test,
	IRT_ID_channel, IRT_ID_client_app, IRT_ID_context, IRT_ID_data_item, IRT_ID_wi_event_register,
	IRT_ID_wg_event_register, IRT_ID_work_group, IRT_ID_work_item, IRT_ID_worker,
} irt_id_type;

#ifdef _GEMS
    #define ID_TYPE_BIT_FIELD uint8 id_type 
#else 
    #define ID_TYPE_BIT_FIELD irt_id_type id_type : 8
#endif

#define IRT_DECLARE_ID_TYPE(__type) \
struct _irt_##__type; \
struct _irt_##__type##_id { \
	union { \
		uint64 full; \
		struct { \
			uint32 index; \
			uint16 thread; \
			uint8 node; \
			ID_TYPE_BIT_FIELD; \
		}; \
	}; \
	struct _irt_##__type* cached; \
}; \
typedef struct _irt_##__type##_id irt_##__type##_id;

#define IRT_MAKE_ID_TYPE(__type) \
static inline irt_##__type##_id irt_generate_##__type##_id(void *generator_id_ptr) { \
	irt_##__type##_id id; \
	irt_##__type##_id *gen_id = (irt_##__type##_id*)generator_id_ptr; \
	id.full = gen_id->full; \
	id.index = gen_id->index++; \
	id.node = gen_id->node; \
	id.id_type = IRT_ID_##__type; \
	id.cached = NULL; \
	return id; \
} \
static inline irt_##__type##_id irt_##__type##_null_id() { \
	irt_##__type##_id null_id = { { 0 }, NULL }; \
	return null_id; \
}

#define IRT_LOOKUP_GENERATOR_ID_PTR (&(irt_worker_get_current()->generator_id))

#endif // ifndef __GUARD_ID_GENERATION_H
