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
#ifndef __GUARD_WI_IMPLEMENTATION_H
#define __GUARD_WI_IMPLEMENTATION_H

#include "declarations.h"

#include "data_item.h"

#include "irt_optimizer.h"

/* ------------------------------ data structures ----- */

struct _irt_wi_di_requirement {
	irt_data_item_id di_id;
	irt_data_range range;
};

struct _irt_wi_implementation {
	int32 id;
	uint32 num_variants;
	irt_wi_implementation_variant* variants;
};

typedef enum _irt_wi_implementation_type { IRT_WI_IMPL_SHARED_MEM, IRT_WI_IMPL_DISTRIBUTED, IRT_WI_IMPL_OPENCL } irt_wi_implementation_type;

struct _irt_wi_implementation_runtime_data {
	bool flat_profile;
	bool tested;
	bool force_dyn;
	double distribution[IRT_MAX_WORKERS];
	#ifdef IRT_ENABLE_OMPP_OPTIMIZER
	irt_optimizer_runtime_data optimizer_rt_data;
	irt_optimizer_runtime_data* wrapping_optimizer_rt_data;
	uint32 completed_wi_count;
	#endif
	uint32 chunk_size;
};

struct _irt_wi_implementation_variant {
	wi_implementation_func* implementation;
	uint32 num_required_data_items;
	wi_di_req_func* data_requirements;
	uint32 num_required_channels;
	wi_channel_req_func* channel_requirements;
	irt_meta_info_table_entry* meta_info;
	irt_wi_implementation_runtime_data rt_data;
};

/* ------------------------------ operations ----- */


#endif // ifndef __GUARD_WI_IMPLEMENTATION_H
