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
#ifndef __GUARD_IRT_CONTEXT_H
#define __GUARD_IRT_CONTEXT_H

#include "declarations.h"
#include "client_app.h"
#include "instrumentation_regions.h"
#ifdef IRT_ENABLE_OPENCL
#include "irt_opencl.h"
#endif

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(context)

typedef void(init_context_fun)(irt_context* context);
typedef void(cleanup_context_fun)(irt_context* context);

struct _irt_context {
	irt_context_id id;
	irt_client_app* client_app;
	init_context_fun* init_fun;
	cleanup_context_fun* cleanup_fun;
	uint32 type_table_size;
	irt_type* type_table;
	uint32 impl_table_size;
	irt_wi_implementation* impl_table;
	uint32 info_table_size;
	irt_meta_info_table_entry* info_table;

	// TODO: moved this into an #ifdef and have the backend only set it when actually required
	uint32 num_regions; // initialized by compiler
	#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_inst_region_context_data* inst_region_data;                             // initialized by runtime
	irt_inst_region_context_declarations inst_region_metric_group_support_data; // initialized by runtime
	#endif

	#ifdef IRT_ENABLE_OPENCL
	irt_opencl_context opencl_context;
	#endif
	// private implementation detail
	struct _irt_context* lookup_table_next;
};


/* ------------------------------ operations ----- */

static inline irt_context* irt_context_get_current();

irt_context* irt_context_create(irt_client_app*, init_context_fun*, cleanup_context_fun*);
irt_context* irt_context_create_standalone(init_context_fun*, cleanup_context_fun*);
void irt_context_initialize(irt_context* context);
void irt_context_destroy(irt_context* context);


#endif // ifndef __GUARD_IRT_CONTEXT_H
