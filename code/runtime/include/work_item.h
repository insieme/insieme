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

#include <inttypes.h>

/* ------------------------------ data structures ----- */

typedef union _irt_work_item_id {
	uint64 id;
	struct {
		uint16 node;
		uint16 thread;
		uint32 index;
	};
} irt_work_item_id;

typedef struct _irt_work_item_range {
	int64 begin, end, step;
} irt_work_item_range;

typedef struct _irt_work_item {
	irt_work_item_id id;
	irt_work_item_range range;
	irt_implementation_id impl_id;
	irt_work_group* group;
	unsigned priority; // ?
} irt_work_item;


/* ------------------------------ operations ----- */

irt_errcode irt_wi_create(irt_work_item** out_wi);
irt_errcode irt_wi_destroy(irt_work_item* wi);

irt_errcode irt_wi_enqueue(irt_work_item* wi);
irt_errcode irt_wi_enqueue_remote(irt_work_item* wi, irt_worker* worker);

irt_errcode irt_wi_yield();

irt_errcode irt_wi_split(irt_work_item* wi, uint32 elements; irt_work_item** out_wis);
