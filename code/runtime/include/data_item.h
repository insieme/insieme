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

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(irt_data_item);

enum _irt_data_mode {
	IRT_DMODE_READ_ONLY,
	IRT_DMODE_WRITE_ONLY,
	IRT_DMODE_WRITE_FIRST,
	IRT_DMODE_READ_WRITE
};

struct _irt_data_range {
	int64 begin, end, step;
};

struct _irt_data_block {
	uint32 use_count;
	//irt_hw_id location;
	void* data;
};

struct _irt_data_item {
	irt_data_item_id id;
	uint32 use_count;
	irt_type_id type_id;
	uint32 dimensions;
	irt_data_mode mode;
	// range and sizes have as many entries as data_item has dimensions
	uint64 *sizes;
	irt_data_range* range;
	// can be NULL if data item is abstract
	irt_data_block* data_block;
 };


/* ------------------------------ operations ----- */

irt_data_item* irt_di_create(irt_type_id tid, uint32 dimensions, uint64* sizes);
irt_data_item* irt_di_create_sub(irt_data_item_id parent, irt_data_range range);
void* irt_di_aquire(irt_data_item* di, irt_data_mode mode);
void irt_di_destroy(irt_data_item* di);


/* ============================== light weight data item ===== */

// size of an actual lw_data_item: sizeof(irt_type_id) + size of the irt_type referenced by type_id
struct _irt_lw_data_item {
	irt_type_id type_id;
	// actual content will be stored here
};
