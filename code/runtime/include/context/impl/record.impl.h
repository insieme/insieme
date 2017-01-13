/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_CONTEXT_IMPL_RECORD_IMPL_H
#define __GUARD_CONTEXT_IMPL_RECORD_IMPL_H

#include "context/record.h"
#include "abstraction/atomic.h"

#include "context/impl/common.impl.h"
#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"
#include "abstraction/impl/spin_locks.impl.h"

#ifdef __INTEL_COMPILER
#pragma warning push
// 279 - controlling expression is constant
#pragma warning disable 279
#endif

// ------------------------------------------------------------------------------------
//    Block Handling
// ------------------------------------------------------------------------------------

/**
 * Internally, the list of blocks is handled within a linked list. Blocks
 * are added at the front, increasing indices incrementally. Further, more
 * recent region definitions are covering older definitions automatically.
 */

typedef struct _irt_cap_data_block_info_list_node {
	irt_cap_data_block_info info;
	struct _irt_cap_data_block_info_list_node* next;
} irt_cap_data_block_info_list_node;


/**
 * The global list of data blocks maintained by this module.
 */
irt_cap_data_block_info_list_node* volatile irt_g_cap_data_block_list = NULL;

const irt_cap_data_block_info* irt_cap_dbi_register_block(void* base, uint32 size) {
	// create new block
	irt_cap_data_block_info_list_node* node = (irt_cap_data_block_info_list_node*)malloc(sizeof(irt_cap_data_block_info_list_node));
	node->info.base = base;
	node->info.size = size;

	// insert into list of blocks (lock-free)
	do {
		// add next pointer
		node->next = irt_g_cap_data_block_list;

		// update the id for the new block
		node->info.id = (node->next) ? (node->next->info.id + 1) : 1;

		// try adding the element to list
	} while(!irt_atomic_bool_compare_and_swap((intptr_t*)&irt_g_cap_data_block_list, (intptr_t)(node->next), (intptr_t)node, intptr_t));

	// return pointer to stored information
	return &(node->info);
}

const irt_cap_data_block_info* irt_cap_dbi_lookup(void* ptr) {
	// search for the corresponding block
	irt_cap_data_block_info_list_node* cur = irt_g_cap_data_block_list;
	uint64 pos = (uint64)ptr;
	while(cur != NULL) {
		// check whether block is covering the location pointed to
		uint64 base = (uint64)cur->info.base;
		if(base <= pos && pos < base + cur->info.size) { return &(cur->info); }
		cur = cur->next;
	}

	// unknown block!
	return NULL;
}

void irt_cap_dbi_init() {
	irt_g_cap_data_block_list = NULL;
}

void irt_cap_dbi_finalize() {
	// delete data block list
	irt_cap_data_block_info_list_node* cur = irt_g_cap_data_block_list;
	while(cur != NULL) {
		irt_cap_data_block_info_list_node* next = cur->next;
		free(cur);
		cur = next;
	}
	irt_g_cap_data_block_list = NULL;
}


// ------------------------------------------------------------------------------------
//    Region Handling
// ------------------------------------------------------------------------------------


/**
 * Internally, regions are organized within two data structures. A
 * global list of regions is recording all lists encountered during
 * the execution in order of their first encounter while a stack
 * is used to maintain all currently active regions.
 */


/**
 * The global list of regions encountered during the execution.
 */

typedef struct _irt_cap_region_list {
	irt_cap_region region;             // the main data structure representing regions
	struct _irt_cap_region_list* next; // the successor pointer to form the list
} irt_cap_region_list;

irt_cap_region_list* volatile irt_g_cap_region_list = NULL;


/**
 * The global list of data blocks maintained by this module.
 */

typedef struct _irt_cap_region_stack {
	irt_cap_region* region;             // a pointer to corresponding region within the region list
	struct _irt_cap_region_stack* next; // the next pointer to realize the stack
} irt_cap_region_stack;

irt_cap_region_stack* volatile irt_g_cap_region_stack = NULL;


void irt_cap_region_start(uint32 id) {
	// add region to region list
	irt_cap_region_list* elem = (irt_cap_region_list*)malloc(sizeof(irt_cap_region_list));

	// init data
	elem->region.id = id;
	elem->region.active = true;
	irt_spin_init(&elem->region.lock);
	elem->region.usage = NULL;

	// add to list
	elem->next = irt_g_cap_region_list;
	irt_g_cap_region_list = elem;


	// also, push element on stack
	irt_cap_region_stack* stackElem = (irt_cap_region_stack*)malloc(sizeof(irt_cap_region_stack));
	stackElem->region = &(elem->region);

	// push on stack using lock-free mechanism
	do {
		stackElem->next = irt_g_cap_region_stack;
	} while(!irt_atomic_bool_compare_and_swap((intptr_t*)&irt_g_cap_region_stack, (intptr_t)stackElem->next, (intptr_t)stackElem, intptr_t));
}

void irt_cap_region_stop(uint32 id) {
	// check some pre-requirements
	IRT_ASSERT(irt_g_cap_region_stack, IRT_ERR_INTERNAL, "Stack must not be empty!");
	IRT_ASSERT(irt_g_cap_region_stack->region->id == id, IRT_ERR_INTERNAL, "Non-matching start/stop encountered!");

	// pop elment from stack
	irt_cap_region_stack* top = irt_g_cap_region_stack;
	irt_g_cap_region_stack = top->next;
	top->next = NULL;

	// update region state
	top->region->active = false;

	// delete stack element
	free(top);
}

irt_cap_block_usage_info* irt_cap_get_usage_info(irt_cap_region* region, void* ptr) {
	irt_cap_block_usage_info* cur = region->usage;
	uint64 pos = (uint64)ptr;
	while(cur != NULL) {
		// check whether block is covering the location pointed to
		const irt_cap_data_block_info* info = cur->block;
		if((uint64)info->base <= pos && pos < (uint64)info->base + info->size) { return cur; }
		cur = cur->next;
	}
	return NULL;
}

irt_cap_block_usage_info* irt_cat_get_or_create_usage_info(irt_cap_region* region, void* pos) {
	irt_spin_lock(&region->lock);

	irt_cap_block_usage_info* res = irt_cap_get_usage_info(region, pos);
	if(res) {
		irt_spin_unlock(&region->lock);
		return res;
	}

	// not found ...
	// 	=> create new information

	const irt_cap_data_block_info* info = irt_cap_dbi_lookup(pos);
	IRT_ASSERT(info, IRT_ERR_INTERNAL, "Cannot create usage info for unregistered block!");
	irt_cap_block_usage_info* elem = (irt_cap_block_usage_info*)malloc(sizeof(irt_cap_block_usage_info));

	elem->block = info;
	elem->tag = -1;

	// init locks
	elem->locks = (irt_spinlock*)malloc(info->size * sizeof(irt_spinlock));
	for(int i = 0; i < info->size; i++) {
		irt_spin_init(&elem->locks[i]);
	}

	// init data fields
	elem->life_in_values = (char*)malloc(info->size);
	elem->life_out_values = (char*)malloc(info->size);

	// init flag masks
	elem->read = (bool*)calloc(info->size, sizeof(bool));
	elem->is_pointer = (bool*)calloc(info->size, sizeof(bool));
	elem->last_written = (bool*)calloc(info->size, sizeof(bool));
	elem->is_life_out = (bool*)calloc(info->size, sizeof(bool));

	// add to usage
	elem->next = region->usage;
	region->usage = elem;

	irt_spin_unlock(&region->lock);

	return elem;
}

void irt_cap_tag_block(void* pos, uint16 id) {
	irt_cap_region* region = irt_g_cap_region_stack->region;
	IRT_ASSERT(irt_cat_get_or_create_usage_info(region, pos)->tag < 0, IRT_ERR_INTERNAL, "Tag must only be assigned once!");
	irt_cat_get_or_create_usage_info(region, pos)->tag = id;
}

void irt_cap_print_report() {
	irt_cap_region_list* cur = irt_g_cap_region_list;
	while(cur != NULL) {
		int count = 0;
		irt_cap_block_usage_info* it = cur->region.usage;
		while(it != NULL) {
			count++;
			it = it->next;
		}

		printf("Region %u:\n", cur->region.id);
		printf("  Number of used blocks: %d\n", count);

		it = cur->region.usage;
		while(it != NULL) {
			printf("  Block %d from %p size %u - is pointer: %d\n", it->tag, (void*)it->block->base, it->block->size, it->is_pointer[0]);
			it = it->next;
		}

		cur = cur->next;
	}
}


void irt_cap_region_init() {
	// nothing to do
}

void irt_cap_region_finalize() {
	// print some report
	DEBUG(irt_cap_print_report());

	// clear stack
	{
		irt_cap_region_stack* cur = irt_g_cap_region_stack;
		while(cur != NULL) {
			irt_cap_region_stack* next = cur->next;
			free(cur);
			cur = next;
		}
	}

	// clear region usage information
	{
		irt_cap_region_list* cur = irt_g_cap_region_list;
		while(cur != NULL) {
			irt_cap_region_list* next = cur->next;

			// free the usage list ...
			irt_cap_block_usage_info* it = cur->region.usage;
			while(it != NULL) {
				irt_cap_block_usage_info* t = it->next;

				for(int i = 0; i < it->block->size; i++) {
					irt_spin_destroy(&it->locks[i]);
				}

				// free all members
				free((void*)it->locks);
				free(it->life_in_values);
				free(it->life_out_values);
				free(it->read);
				free(it->last_written);
				free(it->is_life_out);
				free(it->is_pointer);

				// free usage info itself
				free(it);

				it = t;
			}

			// free region list entry
			free(cur);

			cur = next;
		}
	}

	irt_g_cap_region_list = NULL;
}

/**
 * Record a write to the given position assuming the given value is read. The
 * separation of the position and the value is used to introduce pointer substitutions.
 */
void irt_cap_read_internal(void* pos, void* value, uint16 size, bool is_ptr) {
	// printf("Reading ..\n");

	if(!irt_cap_dbi_lookup(pos)) {
		return; // not a registered memory location => can be ignored
	}

	// mark value being read within all active regions
	irt_cap_region_stack* cur = irt_g_cap_region_stack;
	while(cur != NULL) {
		irt_cap_block_usage_info* info;
		if(is_ptr) { // external pointer to local may be ignored
			info = irt_cap_get_usage_info(cur->region, pos);
		} else {
			info = irt_cat_get_or_create_usage_info(cur->region, pos);
		}

		if(info) {
			int offset = ((uint64)pos - (uint64)info->block->base) / sizeof(char);
			char* data = (char*)value;

			// process each of the read bytes
			for(int i = 0; i < size / sizeof(char); i++) {
				int absPos = i + offset;

				// request the lock
				irt_spinlock* lock = &info->locks[absPos];
				irt_spin_lock(lock);

				// check whether value has already been written
				if(info->last_written[absPos]) {
					irt_spin_unlock(lock);
					continue; // can be ignored
				}

				// check whether it has been read before
				if(info->read[absPos]) {
					irt_spin_unlock(lock);
					continue; // has been read before
				}

				// mark as read
				info->read[absPos] = true;

				// record value
				info->life_in_values[absPos] = data[i];

				// set pointer flag
				IRT_ASSERT(is_ptr || !info->is_pointer[absPos], IRT_ERR_INTERNAL, "Cannot support mixture of values and pointers!");
				info->is_pointer[absPos] = is_ptr;

				// release the lock
				irt_spin_unlock(lock);

				// DEBUG(printf("New value read - pos %d - size %d\n", absPos, (int)size));
			}
		}

		cur = cur->next;
	}

	// for all retired regions => marke value as being alive
	irt_cap_region_list* it = irt_g_cap_region_list;
	while(it != NULL) {
		if(!it->region.active) { // region has retired

			irt_cap_block_usage_info* info = irt_cap_get_usage_info(&(it->region), pos);
			if(info) {
				int offset = ((uint64)pos - (uint64)info->block->base) / sizeof(char);

				// process each of the read bytes
				for(int i = 0; i < size / sizeof(char); i++) {
					int absPos = i + offset;

					// request the lock
					irt_spin_lock(&info->locks[absPos]);

					// check whether the value has last been writen by this region
					if(!info->last_written[absPos]) {
						irt_spin_unlock(&info->locks[absPos]);
						continue; // can be ignored
					}

					// make sure, position has not been used as a pointer!
					IRT_ASSERT(is_ptr || !info->is_pointer[absPos], IRT_ERR_INTERNAL, "Cannot support mixture of values and pointers!");

					// update ti is_life_out flag
					info->is_life_out[absPos] = true;

					// release the lock
					irt_spin_unlock(&info->locks[absPos]);

					// DEBUG(printf("Life-out discovered - pos %d - size %d\n", absPos, (int)size));
				}
			}
		}

		it = it->next;
	}
}

/**
 * Record a write to the given position assuming the given value is read. The
 * separation of the position and the value is used to introduce pointer substitutions.
 */
void irt_cap_written_internal(void* pos, void* value, uint16 size, bool is_ptr) {
	// printf("Writing ..\n");

	if(!irt_cap_dbi_lookup(pos)) {
		return; // not a registered memory location => can be ignored
	}

	// mark value being read within all active regions
	irt_cap_region_stack* cur = irt_g_cap_region_stack;
	while(cur != NULL) {
		irt_cap_block_usage_info* info;
		if(is_ptr) { // external pointer to local may be ignored
			info = irt_cap_get_usage_info(cur->region, pos);
		} else {
			info = irt_cat_get_or_create_usage_info(cur->region, pos);
		}

		if(info) {
			int offset = ((uint64)pos - (uint64)info->block->base) / sizeof(char);
			char* data = (char*)value;

			// process each of the written bytes
			for(int i = 0; i < size / sizeof(char); i++) {
				int absPos = i + offset;

				// request the lock
				irt_spin_lock(&info->locks[absPos]);

				// mark as being written
				info->last_written[absPos] = true;

				// safe written value (as a life-out value)
				info->life_out_values[absPos] = data[i];

				// mark as pointer, if necessary
				IRT_ASSERT(is_ptr || !info->is_pointer[absPos], IRT_ERR_INTERNAL, "Cannot support mixture of values and pointers!");

				info->is_pointer[absPos] = is_ptr;

				// release the lock
				irt_spin_unlock(&info->locks[absPos]);

				// DEBUG(printf("New value written - pos %d - size %d\n", absPos, (int)size));
			}
		}

		cur = cur->next;
	}

	// mark all retired regions not to be the last writer
	// for all retired regions => marke value as being alive
	irt_cap_region_list* it = irt_g_cap_region_list;
	while(it != NULL) {
		if(!it->region.active) { // region has retired

			irt_cap_block_usage_info* info = irt_cap_get_usage_info(&(it->region), pos);
			if(info) {
				int offset = ((uint64)pos - (uint64)info->block->base) / sizeof(char);

				// process each of the written bytes
				for(int i = 0; i < size / sizeof(char); i++) {
					int absPos = i + offset;

					// request the lock
					irt_spin_lock(&info->locks[absPos]);

					// remove last-written flag
					info->last_written[absPos] = false;

					// make sure, position has not been used as a pointer!
					IRT_ASSERT(is_ptr || !info->is_pointer[absPos], IRT_ERR_INTERNAL, "Cannot support mixture of values and pointers!");

					DEBUG(printf("Life-out terminated - pos %d - size %d\n", absPos, (int)size));

					// release the lock
					irt_spin_unlock(&info->locks[absPos]);
				}
			}
		}

		it = it->next;
	}
}

void irt_cap_read_value(void* pos, uint16 size) {
	irt_cap_read_internal(pos, pos, size, false);
}

void irt_cap_written_value(void* pos, uint16 size) {
	irt_cap_written_internal(pos, pos, size, false);
}

irt_cap_pointer_substitute irt_cap_get_substitute(void* pos) {
	IRT_ASSERT(sizeof(irt_cap_pointer_substitute) == sizeof(void*), IRT_ERR_INTERNAL, "Sizes don't match!");

	irt_cap_pointer_substitute res;

	// handle null
	if(pos == NULL) {
		res.block = 0;
		res.offset = 0;
		return res;
	}

	// resolve all other pointers
	const irt_cap_data_block_info* info = irt_cap_dbi_lookup(pos);

	IRT_ASSERT(info, IRT_ERR_INTERNAL, "Reading unregistered memory location!");
	IRT_ASSERT((uint64)pos - (uint64)info->base <= ((uint32)(0) - 1u), IRT_ERR_INTERNAL, "Width of offset segment not large enought!");

	res.block = info->id;
	res.offset = (uint64)pos - (uint64)info->base;

	DEBUG(printf("Adding substitute to block %d offset %d\n", res.block, res.offset));
	return res;
}


void irt_cap_read_pointer(void** pos) {
	// 1) compute replacement value
	irt_cap_pointer_substitute replacement = irt_cap_get_substitute(*pos);

	// 2) record read of replacement value
	irt_cap_read_internal(pos, &replacement, sizeof(void*), true);
}

void irt_cap_written_pointer(void** pos) {
	// 1) compute replacement value
	irt_cap_pointer_substitute replacement = irt_cap_get_substitute(*pos);

	// 2) record read of replacement value
	irt_cap_written_internal(pos, &replacement, sizeof(void*), true);
}

#undef PTR_CPY


// -- DUMP PROFILE --------------------------------------------


// --- writing captured context to binary file -------------------


#define OUT(X) (tmp = X, fwrite((&tmp), sizeof(tmp), 1, f))

uint32 irt_cap_count_mask_fragments(bool* marks, size_t size) {
	uint32 counter = 0;
	for(uint32 i = 1; i < size - 1; i++) {
		if(marks[i - 1] && !marks[i]) { // increment whenever a fragment is done
			counter++;
		}
	}
	if(marks[size - 1]) { counter++; }
	return counter;
}

uint32 irt_cap_sum_of_block_sizes(irt_cap_block_usage_info* list) {
	if(list == NULL) { return 0; }
	return list->block->size + irt_cap_sum_of_block_sizes(list->next);
}

void irt_cap_output_append_data_fragments(char* data, bool* flags, size_t size, FILE* f) {
	uint32 tmp; // used by write macro

	int start = -1;
	for(uint32 i = 0; i < size; i++) {
		if(flags[i] && start == -1) {
			// start new fragment
			start = i;
		} else if(!flags[i] && start != -1) {
			// write fragment data
			uint32 length = (size_t)(i - start);
			OUT(start);
			OUT(length);

			if(data) { fwrite(&(data[start]), sizeof(char), length, f); }

			DEBUG(printf("Safed Fragment from %d - size %d.\n", start, (int)length));

			start = -1;
		}
	}
	if(flags[size - 1]) {
		// write fragment data
		uint32 length = (size_t)(size - start);
		OUT(start);
		OUT(length);

		if(data) { fwrite(&(data[start]), sizeof(char), length, f); }

		DEBUG(printf("Safed Fragment from %d - size %d.\n", start, (int)length));
	}
}

void irt_cap_output_append_mask_fragments(bool* flags, size_t size, FILE* f) {
	irt_cap_output_append_data_fragments(NULL, flags, size, f);
}

uint32 irt_cap_get_num_regions() {
	uint32 res = 0;
	irt_cap_region_list* cur = irt_g_cap_region_list;
	while(cur != NULL) {
		res++;
		cur = cur->next;
	}
	return res;
}

uint32 irt_cap_count_block_infos(irt_cap_block_usage_info* list) {
	if(list == NULL) { return 0; }
	return irt_cap_count_block_infos(list->next) + 1;
}

void irt_cap_profile_save() {
	uint32 tmp; // used by write macro

	// determine profile name
	const char* file_name = irt_cap_profile_get_filename();

	// safe data to file
	FILE* f = fopen(file_name, "w");

	// start with magic number
	OUT(MAGIC_NUMBER);

	// write number of stored regions
	OUT(irt_cap_get_num_regions());
	for(irt_cap_region_list* it = irt_g_cap_region_list; it != NULL; it = it->next) {
		irt_cap_region* region = &(it->region);

		// write current region ID
		OUT(region->id);

		// write number of blocks
		OUT(irt_cap_count_block_infos(region->usage));

		// write total size of blocks
		OUT(irt_cap_sum_of_block_sizes(region->usage));

		// write life_in blocks
		for(irt_cap_block_usage_info* it2 = region->usage; it2 != NULL; it2 = it2->next) {
			irt_cap_block_usage_info* info = it2;
			uint32 size = info->block->size;

			// add block id
			OUT(info->block->id);

			// add block tag
			OUT(info->tag);

			// save block pointer address (for allignment when being restored)
			OUT((size_t)info->block->base);

			// add block size
			OUT(size);

			// add number of life-in fragments
			OUT(irt_cap_count_mask_fragments(info->read, size));

			// add life-in fragments
			irt_cap_output_append_data_fragments(info->life_in_values, info->read, size, f);
		}

		// write pointer information
		for(irt_cap_block_usage_info* it2 = region->usage; it2 != NULL; it2 = it2->next) {
			irt_cap_block_usage_info* info = it2;
			uint32 size = info->block->size;

			// add number of pointer fragments
			OUT(irt_cap_count_mask_fragments(info->is_pointer, size));

			// add is_pointer fragments
			irt_cap_output_append_mask_fragments(info->is_pointer, size, f);
		}

		//		// write life_out information
		//		for(irt_cap_block_usage_info* it2 = region->usage; it2 !=NULL; it2 = it2->next) {
		//			irt_cap_block_usage_info* info = it2;
		//			uint32 size = info->block->size;
		//
		//			// add block id
		//			OUT(info->block->id);
		//
		//			// add number of life-out fragments
		//			OUT(irt_cap_count_mask_fragments(info->is_life_out, size));
		//
		//			// add life-out fragments
		//			irt_cap_output_append_data_fragments(info->life_out_values, info->is_life_out, size, f);
		//		}
	}

	fclose(f);
}

#undef OUT

#ifdef __INTEL_COMPILER
#pragma warning pop
#endif


#endif // ifndef __GUARD_CONTEXT_IMPL_RECORD_IMPL_H
