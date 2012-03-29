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

#include "declarations.h"

#include "data_item.h"

/* ------------------------------ data structures ----- */

struct _irt_wi_di_requirement {
	irt_data_item_id di_id;
	irt_data_range range;
};

struct _irt_wi_implementation {
	uint32 num_variants;
	irt_wi_implementation_variant* variants;
};

typedef enum _irt_wi_implementation_type {
	IRT_WI_IMPL_SHARED_MEM, IRT_WI_IMPL_DISTRIBUTED, IRT_WI_IMPL_OPENCL
} irt_wi_implementation_type;

struct _irt_wi_implementation_variant_features {
	uint64 effort;
	bool opencl;
	int64 implicit_region_id;
	int64 suggested_thread_num;
};

struct _irt_wi_implementation_runtime_data {
	bool flat_profile;
	bool tested;
	bool force_dyn;
	double distribution[IRT_MAX_WORKERS];
	uint32 chunk_size;
};

struct _irt_wi_implementation_variant {
	irt_wi_implementation_type type;
	wi_implementation_func* implementation;
	wi_effort_estimation_func* effort_estimator;
	uint32 num_required_data_items;
	wi_di_req_func* data_requirements;
	uint32 num_required_channels;
	wi_channel_req_func* channel_requirements;
	irt_wi_implementation_variant_features features;
	irt_wi_implementation_runtime_data rt_data;
};

/* ------------------------------ operations ----- */


