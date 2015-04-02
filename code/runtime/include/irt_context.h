/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_IRT_CONTEXT_H
#define __GUARD_IRT_CONTEXT_H

#include "declarations.h"
#include "client_app.h"
#include "instrumentation_regions.h"

#ifdef USE_OPENCL
#include "irt_ocl.h"
#endif

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(context)

typedef void (init_context_fun)(irt_context* context);
typedef void (cleanup_context_fun)(irt_context* context);

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
	uint32 num_regions;													// initialized by compiler
#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_inst_region_context_data* inst_region_data;							// initialized by runtime
	irt_inst_region_context_declarations inst_region_metric_group_support_data;					// initialized by runtime
#endif

#ifdef USE_OPENCL
	irt_ocl_kernel_code* kernel_code_table;
	irt_ocl_kernel** kernel_binary_table;
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
