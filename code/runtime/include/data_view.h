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

#include "irt_inttypes.h"

#include "id_generation.h"

/* ------------------------------ data structures ----- */

typedef enum _irt_view_mode {
	READ_ONLY,
	WRITE_ONLY,
	WRITE_FIRST,
	READ_WRITE
} irt_view_mode;

typedef struct _irt_data_view_range {
	int64 begin, end, step;
} irt_data_view_range;

typedef struct _irt_data_block {
	uint32 use_count;
//	irt_hw_id location;
	void* data;
} irt_data_block;

typedef struct _irt_data_view {
	irt_data_item_id data_item;
	uint8 mode;
	// range has as many entries as data_item has dimensions
	irt_data_view_range* range;
	irt_data_block* data_block;
	// cache data pointer?
 } irt_data_view;


/* ------------------------------ operations ----- */

irt_errcode irt_dv_create(irt_data_item_id data, irt_data_view_range* range, irt_view_mode mode, irt_hw_id location, irt_data_view** di_out);
//// parent has to fully contain all elements specified by range
//irt_errcode irt_dv_create_sub(irt_data_view parent, irt_data_view_range range, irt_data_view** di_out);
irt_errcode irt_dv_destroy(irt_data_view* di);
