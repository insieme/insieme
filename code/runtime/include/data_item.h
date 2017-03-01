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
#ifndef __GUARD_DATA_ITEM_H
#define __GUARD_DATA_ITEM_H

#include "declarations.h"

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(data_item)

typedef enum _irt_data_mode { IRT_DMODE_READ_ONLY, IRT_DMODE_WRITE_ONLY, IRT_DMODE_WRITE_FIRST, IRT_DMODE_READ_WRITE } irt_data_mode;

struct _irt_data_range {
	int64 begin, end, step;
};
// data range marker value representing full range
static const irt_data_range irt_g_data_range_all = {1, 1, 0};

struct _irt_data_block {
	uint32 use_count;
	// irt_hw_id location;
	void* data;
};

struct _irt_data_item {
	irt_data_item_id id;
	// can be null_id if no parent
	irt_data_item_id parent_id;
	irt_type_id type_id;
	uint32 use_count;
	uint32 dimensions;
	// irt_data_mode mode;
	// ranges has as many entries as data_item has dimensions
	irt_data_range* ranges;
	// can be NULL if data item is abstract
	irt_data_block* data_block;
	//	irt_pd_table* performance_data;
	// private implementation detail
	struct _irt_data_item* lookup_table_next;
};


/* ------------------------------ operations ----- */

/** Creates a new data item with the given type and size.
 **/
irt_data_item* irt_di_create(irt_type_id tid, uint32 dimensions, irt_data_range* ranges);

/** Creates a data item representing a sub-range of a parent data item.
 ** Type and dimensions are the same as for the parent.
 **/
irt_data_item* irt_di_create_sub(irt_data_item* parent, irt_data_range* ranges);

/** Destroys an existing data item.
 **/
void irt_di_destroy(irt_data_item* di);


irt_data_block* irt_di_acquire(irt_data_item* di, irt_data_mode mode);
void irt_di_free(irt_data_block* p);


/* ============================== light weight data item ===== */

// size of an actual lw_data_item: sizeof(irt_type_id) + size of the irt_type referenced by type_id
struct _irt_lw_data_item {
	irt_type_id type_id;
	// actual content will be stored here
};


#endif // ifndef __GUARD_DATA_ITEM_H
