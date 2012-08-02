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

#include <pthread.h>

#include "error_handling.h"

#define IRT_ID_HASH(__id__) ((__id__.value.components.thread<<11) ^ (__id__.value.components.index))
//#define IRT_ID_HASH(__id__) (7*((__id__.value.components.thread<<16) ^ (__id__.value.components.index)) >> 16)

/* Defines a global, thread-safe lookup table and the functions to insert, 
 * retrieve and delete elements from it. The globals must still be created
 * using CREATE_LOOKUP_TABLE
 * __type__ : struct type to create table for (assumed to have an "id" member)
 * __next_name__ : name of the next pointer in the struct
 * __hashing_expression__ : expression that generates a hash value from an id
 * __num_buckets__ : number of slots in the hash map
 * */
#define IRT_DEFINE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__) \
extern irt_##__type__* irt_g_##__type__##_table[__num_buckets__]; \
extern pthread_spinlock_t irt_g_##__type__##_table_locks[__num_buckets__]; \
\
static inline void irt_##__type__##_table_init() { \
	for(int i=0; i<__num_buckets__; ++i) { \
		irt_g_##__type__##_table[i] = NULL; \
		if(pthread_spin_init(&irt_g_##__type__##_table_locks[i], PTHREAD_PROCESS_PRIVATE) != 0) { \
			irt_throw_string_error(IRT_ERR_INIT, "Failed initializing locks for " #__type__ " lookup table."); \
		} \
	} \
} \
static inline void irt_##__type__##_table_clear() { \
	for(int i=0; i<__num_buckets__; ++i) { \
		irt_##__type__ *element = irt_g_##__type__##_table[i], *temp; \
		while(element) { \
			temp = element->__next_name__; \
			element->__next_name__ = NULL; \
			element = temp; \
		} \
		irt_g_##__type__##_table[i] = NULL; \
	} \
} \
static inline void irt_##__type__##_table_cleanup() { \
	irt_##__type__##_table_clear(); \
	for(int i=0; i<__num_buckets__; ++i) { \
		pthread_spin_destroy(&irt_g_##__type__##_table_locks[i]); \
	} \
} \
\
static inline void irt_##__type__##_table_insert(irt_##__type__* element) { \
	uint32 hash_val = __hashing_expression__(element->id) % __num_buckets__; \
	pthread_spin_lock(&irt_g_##__type__##_table_locks[hash_val]); \
	element->__next_name__ = irt_g_##__type__##_table[hash_val]; \
	irt_g_##__type__##_table[hash_val] = element; \
	pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
} \
static inline irt_##__type__* irt_##__type__##_table_lookup(irt_##__type__##_id id) { \
	if(id.cached) { return id.cached; } \
	uint32 hash_val = __hashing_expression__(id) % __num_buckets__; IRT_DEBUG("Looking up %u/%u/%u, hash val %u, in table %p", id.value.components.node, id.value.components.thread, id.value.components.index, hash_val, irt_g_##__type__##_table); \
	irt_##__type__* element; \
	/* No locking required assuming sequential consistency and correctness of requests */ \
	/* The above is a lie. A vile lie. People have lost their heads for less. */ \
	pthread_spin_lock(&irt_g_##__type__##_table_locks[hash_val]); \
	element = irt_g_##__type__##_table[hash_val]; \
	IRT_DEBUG_ONLY(if(element)) { IRT_DEBUG("Starting at elem %p, id %u/%u/%u", element, element->id.value.components.node, element->id.value.components.thread, element->id.value.components.index); } \
	while(element && element->id.value.full != id.value.full) { element = element->__next_name__; IRT_DEBUG("Looking at elem %p, id %u/%u/%u", element, element->id.value.components.node, element->id.value.components.thread, element->id.value.components.index); } \
	pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
	id.cached = element; IRT_DEBUG("Found elem %p\n", element); \
	return element; \
} \
static inline irt_##__type__* irt_##__type__##_table_lookup_or_insert(irt_##__type__* element) { \
	uint32 hash_val = __hashing_expression__(element->id) % __num_buckets__; \
	pthread_spin_lock(&irt_g_##__type__##_table_locks[hash_val]); \
	irt_##__type__* ret_element = irt_g_##__type__##_table[hash_val]; \
	while(ret_element && ret_element->id.value.full != element->id.value.full) { ret_element = ret_element->__next_name__; } \
	if(!ret_element) { \
		element->__next_name__ = irt_g_##__type__##_table[hash_val]; \
		irt_g_##__type__##_table[hash_val] = element; \
		ret_element = element; \
	} \
	pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
	return ret_element; \
} \
static inline void irt_##__type__##_table_remove(irt_##__type__##_id id) { \
	uint32 hash_val = __hashing_expression__(id) % __num_buckets__; \
	irt_##__type__ *element, *previous; \
	pthread_spin_lock(&irt_g_##__type__##_table_locks[hash_val]); \
	element = irt_g_##__type__##_table[hash_val]; \
	if(!element) { \
		pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
		irt_throw_string_error(IRT_ERR_INTERNAL, "Removing nonexistent element from " #__type__ " table."); \
		return; \
	} \
	if(element->id.value.full == id.value.full) { \
		irt_g_##__type__##_table[hash_val] = element->__next_name__; \
		pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
		return; \
	} \
	do { \
		previous = element; \
		element = element->__next_name__; \
	} while(element && element->id.value.full != id.value.full); \
	if(!element) { \
		pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
		irt_throw_string_error(IRT_ERR_INTERNAL, "Removing nonexistent element from " #__type__ " table."); \
		return; \
	} \
	previous->__next_name__ = element->__next_name__;\
	pthread_spin_unlock(&irt_g_##__type__##_table_locks[hash_val]); \
}

#define IRT_CREATE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__) \
irt_##__type__* irt_g_##__type__##_table[__num_buckets__]; \
pthread_spinlock_t irt_g_##__type__##_table_locks[__num_buckets__];
