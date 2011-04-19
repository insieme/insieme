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

#include <stdlib.h>
#include <string.h>

#include "data_item.h" 

#include "utils/lookup_tables.h"

IRT_DEFINE_LOOKUP_TABLE(data_item, lookup_table_next, IRT_ID_HASH, IRT_DATA_ITEM_LT_BUCKETS);

static inline irt_data_item* _irt_di_get(uint16 dimensions) {
	char* retval = (char*)malloc(sizeof(irt_data_item) + sizeof(irt_data_range)*dimensions);
	((irt_data_item*)retval)->ranges = (irt_data_range*)(retval + sizeof(irt_data_item));
	return (irt_data_item*)retval;
}
static inline void _irt_di_recycle(irt_data_item* di) {
	free(di);
}
static inline void _irt_di_dec_use_count(irt_data_item* di) {
	if(--di->use_count == 0) _irt_di_recycle(di); // FIXME atomic operation
}

/** Creates a new data item with the given type and size.
 **/
irt_data_item* irt_di_create(irt_type_id tid, uint32 dimensions, irt_data_range* ranges) {
	irt_data_item* retval = _irt_di_get(dimensions);
	retval->type_id = tid; 
	retval->dimensions = dimensions;
	retval->id = irt_generate_data_item_id();
	memcpy(retval->ranges, ranges, sizeof(irt_data_range)*dimensions);
	retval->use_count = 1;
	retval->parent_id = irt_data_item_null_id();
	retval->lookup_table_next = NULL;
	retval->data_block = NULL;
	irt_data_item_table_insert(retval);
	return retval;
}
/** Creates a data item representing a sub-range of a parent data item. 
 ** Type and dimensions are the same as for the parent.
 **/
irt_data_item* irt_di_create_sub(irt_data_item* parent, irt_data_range* ranges) {
	irt_data_item* retval = _irt_di_get(parent->dimensions);
	memcpy(retval, parent, sizeof(irt_data_item));
	memcpy(retval->ranges, ranges, sizeof(irt_data_range)*parent->dimensions);
	irt_data_item_table_insert(retval);
	return retval;
}
/** Destroys an existing data item.
 **/
void irt_di_destroy(irt_data_item* di) {
	_irt_di_dec_use_count(di);
}

irt_data_block* irt_di_aquire(irt_data_item* di, irt_data_mode mode) {
	if(di->data_block) { 
		return di->data_block;
	}
	else {
		// TODO find or create data block
		return di->data_block;		
	}
}
void irt_di_free(irt_data_block* b) {
	// TODO notify parent
	//_irt_di_dec_use_count(di);
}
