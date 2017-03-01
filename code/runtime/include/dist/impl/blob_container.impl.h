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
#ifndef __GUARD_BLOB_CONTAINER_IMPL_H
#define __GUARD_BLOB_CONTAINER_IMPL_H

#include "irt_globals.h"
#include "error_handling.h"
#include "impl/error_handling.impl.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//			Types
////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct _dirt_blob {
	void* payload;
	size_t size;
} dirt_blob;

typedef struct _dirt_container_node dirt_container_node;
struct _dirt_container_node {
	dirt_blob blob;
	dirt_container_node* next;
};

typedef struct _dirt_to_delete dirt_to_delete;
struct _dirt_to_delete {
	unsigned char* buffer;
	dirt_to_delete* next;
};

typedef struct _dirt_blob_container {
	dirt_container_node* root;
	dirt_to_delete* toDelete;
} dirt_blob_container;

typedef struct _dirt_byte_stream {
	unsigned char* payload;
	size_t size;
} dirt_byte_stream;


////////////////////////////////////////////////////////////////////////////////////////////////////////////
//			Globals
////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////
//			Prototypes
////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////
//			Impl
////////////////////////////////////////////////////////////////////////////////////////////////////////////

dirt_blob_container _dirt_init() {
	dirt_blob_container cont;
	cont.root = NULL;
	cont.toDelete = NULL;
	return cont;
}

void _dirt_delete(dirt_blob_container* container) {
	dirt_container_node* curr = container->root;
	while(curr != NULL) {
		_dirt_container_node* tmp = curr;
		curr = curr->next;
		free(tmp);
	}
	dirt_to_delete* toDel = container->toDelete;
	while(toDel != NULL) {
		dirt_to_delete* tmp = toDel;
		toDel = toDel->next;
		free(tmp->buffer);
		free(tmp);
	}
}

void _dirt_write_to_blob_container(dirt_blob_container* container, void* ptr, size_t size) {
	// initialize
	dirt_container_node* new_node = (_dirt_container_node*)malloc(sizeof(_dirt_container_node));
	new_node->next = NULL;
	new_node->blob.payload = ptr;
	new_node->blob.size = size;

	// attach
	_dirt_container_node* current = container->root;
	if(current == NULL) {
		container->root = new_node;
	} else {
		while(current->next != NULL) {
			current = current->next;
		}
		current->next = new_node;
	}
}

dirt_blob _dirt_read_from_blob_container(dirt_blob_container* container) {
	_dirt_container_node* top = container->root;

	// sanity check
	IRT_ASSERT(top != NULL, IRT_ERR_BLOB_CONTAINER, "the blob container is empty");
	container->root = top->next;

	dirt_blob blob = top->blob;
	free(top);
	return blob;
}


dirt_byte_stream _dirt_blob_pack(dirt_blob_container* container) {
	// compute size
	size_t total_size = 0;

	_dirt_container_node* curr = container->root;
	do {
		if(curr != NULL) {
			total_size += curr->blob.size;
			total_size += sizeof(size_t);
			curr = curr->next;
		}
	} while(curr != NULL);

	// allocate buffer
	dirt_byte_stream stream;
	stream.size = total_size;
	stream.payload = (unsigned char*)malloc(total_size);

	// copy
	unsigned char* ptr = stream.payload;
	curr = container->root;
	do {
		if(curr != NULL) {
			memcpy(ptr, (unsigned char*)&curr->blob.size, sizeof(size_t));
			ptr += sizeof(size_t);
			memcpy(ptr, (unsigned char*)curr->blob.payload, curr->blob.size);
			ptr += curr->blob.size;

			curr = curr->next;
		}
	} while(curr != NULL);
	IRT_ASSERT(ptr == (stream.payload + stream.size), IRT_ERR_BLOB_CONTAINER, "copy operation did not made OK");

	return stream;
}

void _dirt_blob_unpack(dirt_blob_container* container, dirt_byte_stream stream) {
	// read size
	unsigned char* ptr = stream.payload;

	_dirt_container_node* new_node = (_dirt_container_node*)malloc(sizeof(_dirt_container_node));
	IRT_ASSERT(container->root == NULL, IRT_ERR_BLOB_CONTAINER, "the blob container must be empty");
	container->root = new_node;
	while(ptr < stream.payload + stream.size) {
		// read a size
		memcpy(&new_node->blob.size, ptr, sizeof(size_t));
		ptr += sizeof(size_t);
		// point to the payload
		new_node->blob.payload = ptr;

		// move ptr
		ptr += new_node->blob.size;
		new_node->next = (_dirt_container_node*)malloc(sizeof(_dirt_container_node));
		new_node = new_node->next;
		new_node->next = NULL;
	}

	// save the stream pointer in the container context for future deletion
	dirt_to_delete* tmp = (dirt_to_delete*)malloc(sizeof(dirt_to_delete));
	tmp->next = container->toDelete;
	tmp->buffer = stream.payload;
	container->toDelete = tmp;
}

#endif // ifndef __GUARD_BLOB_CONTAINER_IMPL_H
