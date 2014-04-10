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
#ifndef __GUARD_IRT_OPTIMIZER_H
#define __GUARD_IRT_OPTIMIZER_H

#include "declarations.h"
//#define IRT_RUNTIME_TUNING
//#define IRT_RUNTIME_TUNING_EXTENDED

#ifdef USE_OPENCL
	#define IRT_RUNTIME_TUNING
#endif

void irt_optimizer_context_startup(irt_context *context);
void irt_optimizer_context_destroy(irt_context *context);

void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group);

#ifndef IRT_RUNTIME_TUNING_EXTENDED
void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time, irt_loop_sched_data* sched_data);
#else
void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data);
#endif

/* OpenMP+ */

typedef struct _irt_optimizer_resources {
    double energy;
    float power;
    float time;
} irt_optimizer_resources;

typedef irt_optimizer_resources irt_optimizer_objective_weights;

typedef struct _irt_optimizer_objective_constraints {
    irt_optimizer_resources min;
    irt_optimizer_resources max;
} irt_optimizer_objective_constraints;

typedef struct _irt_optimizer_objective {
    irt_optimizer_objective_weights weights;    
    irt_optimizer_objective_constraints constraints;    
    irt_inst_region_id region_id;
} irt_optimizer_objective;

// Data types for runtime collected data 

typedef struct _irt_optimizer_wi_data {
    int frequency;
    int outer_frequency;
    int workers_count;
    int *param_values;  // the list of picked values (indexes) for the variables in a param clause
    irt_optimizer_resources resources;
} irt_optimizer_wi_data;

typedef struct _irt_optimizer_runtime_data {
    irt_optimizer_wi_data* data;
    size_t data_size;
    size_t data_last;
    size_t completed_wi_count;
    irt_spinlock spinlock;
} irt_optimizer_runtime_data;

uint64_t irt_optimizer_pick_in_range(uint64_t max);
void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant);
void irt_optimizer_apply_optimizations(irt_wi_implementation_variant* variant);
void irt_optimizer_remove_optimizations(irt_wi_implementation_variant* variant, int pos, bool wi_finalized);

#endif // ifndef __GUARD_IRT_OPTIMIZER_H
