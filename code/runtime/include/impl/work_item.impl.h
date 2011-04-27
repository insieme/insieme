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

#include "work_item.h"

#include <stdlib.h>

static inline irt_work_item* _irt_wi_new() {
	return (irt_work_item*)malloc(1, sizeof(irt_work_item));
}
static inline void _irt_wi_recycle(irt_work_item* wi) {
	free(wi);
}


irt_work_item* irt_wi_create(irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params) {
	irt_work_item* retval = _irt_wi_new();
	retval->id = irt_generate_work_item_id();
	retval->impl_id = impl_id;
	retval->num_groups = 0;
	retval->parameters = params;
	retval->range = range;
}
void irt_wi_destroy(irt_work_item* wi) {
	_irt_wi_recycle(wi);
}

void irt_wi_enqueue(irt_work_item* wi);
void irt_wi_enqueue_other(irt_work_item* wi, irt_worker* worker);

void irt_wi_yield();

void irt_wi_split_uniform(irt_work_item* wi, uint32 elements, irt_work_item** out_wis) {
	// TODO
}
void irt_wi_split_binary(irt_work_item* wi, irt_work_item* out_wis[2]) {
	// TODO
}
void irt_wi_split(irt_work_item* wi, uint32 elements, uint32* offsets, irt_work_item** out_wis) {
	// TODO
}
