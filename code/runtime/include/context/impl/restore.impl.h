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
#ifndef __GUARD_CONTEXT_IMPL_RESTORE_IMPL_H
#define __GUARD_CONTEXT_IMPL_RESTORE_IMPL_H

#include "context/restore.h"

#include "context/impl/common.impl.h"

#ifdef __INTEL_COMPILER
#pragma warning push
// 279 - controlling expression is constant
#pragma warning disable 279
#endif

// -- Storage ------------------------------------

// The storage is the main container used within the
// isolated kernel application to obtain access to data retrieved
// from a file.

typedef struct {
	uint32 id;  // the ID of the represented data block / item
	int32 tag;  // the user defined tag of this data item
	char* data; // does not own the data, just a pointer to the location
} irt_cap_profile_data_block;

typedef struct _irt_cap_profile_life_out_data_fragment {
	uint32 start;
	uint32 size;
	struct _irt_cap_profile_life_out_data_fragment* next;
	char data[];
} irt_cap_profile_life_out_data_fragment;

typedef struct {
	uint32 id;         // the region ID this context was captured for
	uint32 num_blocks; // the number of blocks within the data context

	// life in data structures
	char* data;                         // one data block containing all values
	irt_cap_profile_data_block* blocks; // the list of blocks within the data block above

} irt_cap_profile_region_context;


typedef struct {
	size_t num_contexts;                       // the number of regions stored within this storage
	irt_cap_profile_region_context contexts[]; // the list of contexts (variable size at the end of the struct)
} irt_cap_profile;

#define IN() (fread(&tmp, sizeof(tmp), 1, f) == 1 ? tmp : 0)

irt_cap_profile* irt_cap_load_profile() {
	uint32 tmp; // used by read macro

	// determine profile name
	const char* file_name = irt_cap_profile_get_filename();

	// safe data to file
	FILE* f = fopen(file_name, "r");
	if(!f) {
		printf("Profile file '%s' not found - please specify file using env. variable IRT_CONTEXT_FILE\n", file_name);
		exit(1);
	}

	// start by checking the magic number
	tmp = IN();
	assert(tmp == MAGIC_NUMBER && "File corrupted!");

	// read the number of regions / context contained within the file
	tmp = IN(); // number of regions
	irt_cap_profile* res = (irt_cap_profile*)malloc(sizeof(irt_cap_profile) + sizeof(irt_cap_profile_region_context) * tmp);
	res->num_contexts = tmp;

	DEBUG(printf("Resolving %d context(s) ...\n", (int)res->num_contexts));

	// resolve contexts
	for(size_t i = 0; i < res->num_contexts; i++) {
		// create context instance
		uint32 id = IN();
		uint32 num_blocks = IN();

		irt_cap_profile_region_context* context = &(res->contexts[i]);

		context->id = id;
		context->num_blocks = num_blocks;
		context->blocks = (irt_cap_profile_data_block*)malloc(sizeof(irt_cap_profile_data_block) * num_blocks);

		// create data store
		uint32 totalSize = IN();
		DEBUG(printf("Total Size: %d\n", (int)totalSize));
		DEBUG(printf("Allocating           %d bytes.\n", (int)(totalSize + IRT_CONTEXT_CAPTURE_ALIGNMENT * context->num_blocks)));
		context->data = (char*)malloc(totalSize + IRT_CONTEXT_CAPTURE_ALIGNMENT * context->num_blocks);

		// load individual blocks
		char* cur = context->data;
		for(uint32 j = 0; j < context->num_blocks; j++) {
			irt_cap_profile_data_block* item = &(context->blocks[j]);

			// get item ID
			item->id = IN();

			// get item Tag
			item->tag = IN();

			// get pointer address
			uint32 address = IN();

			// compute next location within data block having the same alignment
			size_t reqAlign = address % IRT_CONTEXT_CAPTURE_ALIGNMENT;
			size_t curAlign = (size_t)cur % IRT_CONTEXT_CAPTURE_ALIGNMENT;
			int diff = reqAlign - curAlign;

			// save start position
			size_t new_pos = (size_t)cur + ((diff < 0) ? diff + IRT_CONTEXT_CAPTURE_ALIGNMENT : diff);
			item->data = (char*)new_pos;

			// check alignment
			assert((char*)new_pos >= cur && "Error in computation - overlapping blocks encountered!");
			assert(new_pos % IRT_CONTEXT_CAPTURE_ALIGNMENT == reqAlign && "Incorrect alignment computation!");

			// move current end
			uint32 size = IN();
			cur = (char*)(new_pos + size); // reads size of block

			// fill in data
			size_t num_fragments = IN();
			for(size_t k = 0; k < num_fragments; k++) {
				char* start = item->data + IN(); // start position
				size_t length = IN();

				// copy data
				length = fread(start, sizeof(char), length, f);
			}

			DEBUG(printf("Restored block %d size %d @ %p\n", item->id, size, (void*)item->data));
		}

		// correct pointers
		for(uint32 j = 0; j < context->num_blocks; j++) {
			// load number of pointer fragments
			uint32 num_fragments = IN();

			// get base index of current block
			char* base = context->blocks[j].data;

			for(uint32 k = 0; k < num_fragments; k++) {
				uint32 start = IN();
				uint32 length = IN();

				assert(length % sizeof(void*) == 0 && "Invalid pointer fragment length!");

				// restore pointers
				for(uint32 l = start; l < start + length; l += sizeof(void*)) {
					// load replacement
					irt_cap_pointer_substitute* replacement = (irt_cap_pointer_substitute*)(&(base[l]));

					// lookup block within context
					irt_cap_profile_data_block* block = NULL;
					for(uint32 m = 0; m < context->num_blocks && !block; m++) {
						if(context->blocks[m].id == replacement->block) { block = &(context->blocks[m]); }
					}

					// compute actual location
					char* ptr = (char*)0x123456; // default value - if not dereferenced, just read!
					if(replacement->block == 0) {
						ptr = NULL;
						DEBUG(printf("Restored NULL pointer\n"));
					} else if(block) {
						ptr = block->data + replacement->offset;
						DEBUG(printf("Restored pointer to block %d offest %d - %p - %p\n", replacement->block, replacement->offset, (void*)block->data,
						             (void*)ptr));
					}

					// restore pointer to block
					*((char**)(&(base[l]))) = ptr;
				}
			}
		}

		// re-allocate array to save some memory
		// DEBUG(printf("Reallocating to %d bytes.\n", cur - context->data));
		// context->data = realloc(context->data, cur - context->data);
	}

	// done
	fclose(f);

	return res;
}


irt_cap_profile* irt_g_cap_profile = NULL;

void irt_cap_profile_get_value(void* target, uint16 region_id, uint16 tag, uint32 size) {
	if(!irt_g_cap_profile) { irt_g_cap_profile = irt_cap_load_profile(); }

	// search for region
	for(int i = 0; i < irt_g_cap_profile->num_contexts; i++) {
		if(irt_g_cap_profile->contexts[i].id == region_id) {
			irt_cap_profile_region_context* context = &(irt_g_cap_profile->contexts[i]);

			for(uint32 m = 0; m < context->num_blocks; m++) {
				if(context->blocks[m].tag == tag) {
					char* base = context->blocks[m].data;

					// copy data
					memcpy(target, base, size);
					return;
				}
			}
		}
	}

	// fail!
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "Requested data block not found!");
}


void irt_cap_profile_finalize() {
	// TODO: cleaning up local stuff

	if(!irt_g_cap_profile) { return; }

	// free the context information
	for(int i = 0; i < irt_g_cap_profile->num_contexts; i++) {
		free(irt_g_cap_profile->contexts[i].data);
		free(irt_g_cap_profile->contexts[i].blocks);
	}
	free(irt_g_cap_profile);
	irt_g_cap_profile = NULL;
}

#ifdef __INTEL_COMPILER
#pragma warning pop
#endif


#endif // ifndef __GUARD_CONTEXT_IMPL_RESTORE_IMPL_H
