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

#include "irt_atomic.h"
#include "irt_types.h"
#include "utils/lookup_tables.h"

#include "impl/irt_context.impl.h"
#include "impl/instrumentation.impl.h"



IRT_DEFINE_LOOKUP_TABLE(data_item, lookup_table_next, IRT_ID_HASH, IRT_DATA_ITEM_LT_BUCKETS);

static inline irt_data_item* _irt_di_new(uint16 dimensions) {
	char* retval = (char*)malloc(sizeof(irt_data_item) + sizeof(irt_data_range)*dimensions);
	((irt_data_item*)retval)->ranges = (irt_data_range*)(retval + sizeof(irt_data_item));
	return (irt_data_item*)retval;
}
static inline void _irt_di_recycle(irt_data_item* di) {
	irt_di_instrumentation_event(irt_worker_get_current(), DATA_ITEM_RECYCLED, di->id);
	irt_data_item_table_remove(di->id);
	free(di);
}
static inline void _irt_di_dec_use_count(irt_data_item* di) {
	if(irt_atomic_sub_and_fetch((uint32*)&di->use_count, 1) == 0) _irt_di_recycle(di);
}


irt_data_item* irt_di_create(irt_type_id tid, uint32 dimensions, irt_data_range* ranges) {
	irt_data_item* retval = _irt_di_new(dimensions);
	retval->type_id = tid; 
	retval->dimensions = dimensions;
	retval->id = irt_generate_data_item_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	retval->id.cached = retval;
	memcpy(retval->ranges, ranges, sizeof(irt_data_range)*dimensions);
	retval->use_count = 1;
	retval->parent_id = irt_data_item_null_id();
	retval->lookup_table_next = NULL;
	retval->data_block = NULL;
	irt_di_instrumentation_event(irt_worker_get_current(), DATA_ITEM_CREATED, retval->id);
	irt_data_item_table_insert(retval);
	return retval;
}
irt_data_item* irt_di_create_sub(irt_data_item* parent, irt_data_range* ranges) {
	irt_data_item* retval = _irt_di_new(parent->dimensions);
	memcpy(retval, parent, sizeof(irt_data_item));
	retval->ranges = (irt_data_range*)malloc(sizeof(irt_data_range)*parent->dimensions);
	memcpy(retval->ranges, ranges, sizeof(irt_data_range)*parent->dimensions);
	retval->id = irt_generate_data_item_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	retval->id.cached = retval;
	retval->parent_id = parent->id;
	irt_data_item_table_insert(retval);
	return retval;
}
void irt_di_destroy(irt_data_item* di) {
	_irt_di_dec_use_count(di);
}

static inline void* _irt_di_build_data_block(uint32 element_size, uint64* sizes, uint32 dim, uint64 totalSize) {
	IRT_ASSERT(dim != 0, IRT_ERR_IO, "Should not be called for scalars!");

	// handle 0-size dimension
	uint64 cur_size = sizes[0];
	if (cur_size == 0) {
		return NULL;
	}

	// handle terminal case
	if (dim == 1) {
		// allocate big chunk of memory
		void* block = malloc(totalSize * cur_size * element_size);
		IRT_ASSERT(block != NULL, IRT_ERR_IO, "Malloc of data block failed.");
		return block;
	}


	// recursively allocate the data
	void* sub = _irt_di_build_data_block(element_size, sizes+1, dim-1, totalSize*cur_size);

	// allocate index array
	void** index = (void**)malloc(cur_size * sizeof(void*));
	IRT_ASSERT(index != NULL, IRT_ERR_IO, "Malloc of index block failed.");

	// initialize the index array
	index[0] = sub;
	uint64 step_size = ((dim == 2)?element_size:sizeof(void*))*cur_size;
	for (uint64 i = 1; i<cur_size; ++i) {
		// void pointer arithmetic is not defined => use ugly int casts
		index[i] = (void*)((uint64)index[i-1] + step_size);
	}

	// return pointer to index array
	return (void*)index;
}

static inline irt_data_block* _irt_db_new(uint32 element_size, uint64* sizes, uint32 dim) {

	// create resulting data block
	irt_data_block* retval = (irt_data_block*)malloc(sizeof(irt_data_block));
	retval->use_count = 1;

	// handle scalars ..
	if (dim == 0) {
		retval->data = malloc(element_size);
		IRT_ASSERT(retval->data != NULL, IRT_ERR_IO, "Malloc of data block failed.");
		return retval;
	}

	// construct data block recursively
	retval->data = _irt_di_build_data_block(element_size, sizes, dim, 1);
	return retval;
}

static inline void _irt_free_data_block(void* block, uint32 dim) {

	// free sub-blocks if necessary
	if (dim > 1) {
		_irt_free_data_block(((void**)block)[0], dim-1);
	}

	// free this block
	free(block);
}

static inline void _irt_db_delete(irt_data_block* block, uint32 dim) {
	// recursively free blocks
	_irt_free_data_block(block->data, dim);
	free(block);
}

static inline void _irt_db_recycle(irt_data_block* di) {
	// TODO
}

irt_data_block* irt_di_acquire(irt_data_item* di, irt_data_mode mode) {

	irt_data_block* cur_block = di->data_block;

	// see if it is already in the data item
	if(cur_block) {
		return cur_block;
	}

	// look up parents
	while (di->parent_id.value.full != irt_data_item_null_id().value.full) {
		// resolve recursively
		irt_data_block* block = irt_di_acquire(irt_data_item_table_lookup(di->parent_id), mode);

		// no test and set required => race conditions are fixed in the parent
		di->data_block = block;
		return block;
	}

	// create the data blocks
	uint64 type_size = irt_type_get_bytes(irt_context_get_current(), di->type_id);
	uint32 dim = di->dimensions;
	uint64 *sizes = (uint64*)alloca(sizeof(uint64)*dim);
	for (uint32 i=0; i<dim; ++i) {
		sizes[i] = di->ranges[i].end - di->ranges[i].begin;
	}

	// update data block and return value
	irt_data_block* block = _irt_db_new(type_size, sizes, dim);
	if (!irt_atomic_bool_compare_and_swap((uintptr_t*)&(di->data_block), (uintptr_t)cur_block, (uintptr_t)block)) {
		// creation failed => delete created block
		_irt_db_delete(block, dim);
	}

	// return the data block
	return di->data_block;
}
void irt_di_free(irt_data_block* b) {
	// TODO notify parent
	//_irt_di_dec_use_count(di);
}
