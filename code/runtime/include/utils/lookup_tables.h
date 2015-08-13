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
#ifndef __GUARD_UTILS_LOOKUP_TABLES_H
#define __GUARD_UTILS_LOOKUP_TABLES_H

#include "abstraction/threads.h"
#include "abstraction/spin_locks.h"
#include "abstraction/impl/spin_locks.impl.h"
#include "abstraction/rdtsc.h"
#include "utils/timing.h"

#include "error_handling.h"

#define IRT_ID_HASH(__id__) ((__id__.thread << 11) ^ (__id__.index))
//#define IRT_ID_HASH(__id__) (7*((__id__.thread<<16) ^ (__id__.index)) >> 16)

// Declares the data structures needed for the lookup table.
// Note that the lookup table lock will also be created for non-locked lookup tables.
#define _IRT_DEFINE_LOOKUP_TABLE_DATA(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__)                                            \
	extern irt_##__type__* irt_g_##__type__##_table[__num_buckets__];                                                                                          \
	extern irt_spinlock irt_g_##__type__##_table_locks[__num_buckets__];

// Defines the functions doing the actual work.
// The passed __locked__ parameter will determine whether table modifications will be protected by locks.
#define _IRT_DEFINE_LOOKUP_TABLE_FUNCTIONS(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__, __post_lookup_action__)               \
                                                                                                                                                               \
	static inline void _irt_##__type__##_table_init_impl(irt_##__type__** table, irt_spinlock* table_locks) {                                                  \
		for(int i = 0; i < __num_buckets__; ++i) {                                                                                                             \
			table[i] = NULL;                                                                                                                                   \
			if(__locked__) {                                                                                                                                   \
				if(irt_spin_init(&table_locks[i]) != 0) { irt_throw_string_error(IRT_ERR_INIT, "Failed initializing locks for " #__type__ " lookup table."); } \
			}                                                                                                                                                  \
		}                                                                                                                                                      \
	}                                                                                                                                                          \
	static inline void _irt_##__type__##_table_clear_impl(irt_##__type__** table, irt_spinlock* table_locks) {                                                 \
		for(int i = 0; i < __num_buckets__; ++i) {                                                                                                             \
			if(__locked__) irt_spin_lock(&table_locks[i]);                                                                                                     \
			irt_##__type__ *element = table[i], *temp;                                                                                                         \
			while(element) {                                                                                                                                   \
				temp = element->__next_name__;                                                                                                                 \
				element->__next_name__ = NULL;                                                                                                                 \
				element = temp;                                                                                                                                \
			}                                                                                                                                                  \
			table[i] = NULL;                                                                                                                                   \
			if(__locked__) irt_spin_unlock(&table_locks[i]);                                                                                                   \
		}                                                                                                                                                      \
	}                                                                                                                                                          \
	static inline void _irt_##__type__##_table_cleanup_impl(irt_##__type__** table, irt_spinlock* table_locks) {                                               \
		_irt_##__type__##_table_clear_impl(table, table_locks);                                                                                                \
		if(__locked__) {                                                                                                                                       \
			for(int i = 0; i < __num_buckets__; ++i) {                                                                                                         \
				irt_spin_destroy(&table_locks[i]);                                                                                                             \
			}                                                                                                                                                  \
		}                                                                                                                                                      \
	}                                                                                                                                                          \
                                                                                                                                                               \
	static inline void _irt_##__type__##_table_insert_impl(irt_##__type__** table, irt_spinlock* table_locks, irt_##__type__* element) {                       \
		uint32 hash_val = __hashing_expression__(element->id) % __num_buckets__;                                                                               \
		if(__locked__) irt_spin_lock(&table_locks[hash_val]);                                                                                                  \
		element->__next_name__ = table[hash_val];                                                                                                              \
		table[hash_val] = element;                                                                                                                             \
		if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                                \
	}                                                                                                                                                          \
	static inline irt_##__type__* _irt_##__type__##_table_lookup_impl(irt_##__type__** table, irt_spinlock* table_locks, irt_##__type__##_id id) {             \
		if(id.cached) { return id.cached; }                                                                                                                    \
		uint32 hash_val = __hashing_expression__(id) % __num_buckets__;                                                                                        \
		IRT_DEBUG("Looking up %u/%u/%u, hash val %u, in table %p", id.node, id.thread, id.index, hash_val, (void*)table);                                      \
		irt_##__type__* element;                                                                                                                               \
		/* No locking required assuming sequential consistency and correctness of requests */                                                                  \
		/* The above is a lie. A vile lie. People have lost their heads for less. */                                                                           \
		if(__locked__) irt_spin_lock(&table_locks[hash_val]);                                                                                                  \
		element = table[hash_val];                                                                                                                             \
		IRT_DEBUG_ONLY(if(element)) {                                                                                                                          \
			IRT_DEBUG("Starting at elem %p, id %u/%u/%u", (void*)element, element->id.node, element->id.thread, element->id.index);                            \
		}                                                                                                                                                      \
		while(element && element->id.full != id.full) {                                                                                                        \
			element = element->__next_name__;                                                                                                                  \
			IRT_DEBUG("Looking at elem %p, id %u/%u/%u", (void*)element, element->id.node, element->id.thread, element->id.index);                             \
		}                                                                                                                                                      \
		__post_lookup_action__; /* allow for some post lookup action like locking */                                                                           \
		if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                                \
		id.cached = element;                                                                                                                                   \
		IRT_DEBUG("Found elem %p\n", (void*)element);                                                                                                          \
		return element;                                                                                                                                        \
	}                                                                                                                                                          \
	static inline irt_##__type__* _irt_##__type__##_table_lookup_or_insert_impl(irt_##__type__** table, irt_spinlock* table_locks,                             \
	                                                                            irt_##__type__* new_element) {                                                 \
		uint32 hash_val = __hashing_expression__(new_element->id) % __num_buckets__;                                                                           \
		if(__locked__) irt_spin_lock(&table_locks[hash_val]);                                                                                                  \
		irt_##__type__* element = table[hash_val];                                                                                                             \
		while(element && element->id.full != new_element->id.full) {                                                                                           \
			element = element->__next_name__;                                                                                                                  \
		}                                                                                                                                                      \
		if(!element) {                                                                                                                                         \
			new_element->__next_name__ = table[hash_val];                                                                                                      \
			table[hash_val] = new_element;                                                                                                                     \
			element = new_element;                                                                                                                             \
		}                                                                                                                                                      \
		__post_lookup_action__; /* allow for some post lookup action like locking */                                                                           \
		if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                                \
		return element;                                                                                                                                        \
	}                                                                                                                                                          \
	static inline irt_##__type__* _irt_##__type__##_table_remove_impl(irt_##__type__** table, irt_spinlock* table_locks, irt_##__type__##_id id) {             \
		uint32 hash_val = __hashing_expression__(id) % __num_buckets__;                                                                                        \
		irt_##__type__ *element, *previous;                                                                                                                    \
		if(__locked__) irt_spin_lock(&table_locks[hash_val]);                                                                                                  \
		element = table[hash_val];                                                                                                                             \
		if(!element) {                                                                                                                                         \
			if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                            \
			irt_throw_string_error(IRT_ERR_INTERNAL, "Removing nonexistent element from " #__type__ " table.");                                                \
			return NULL;                                                                                                                                       \
		}                                                                                                                                                      \
		if(element->id.full == id.full) {                                                                                                                      \
			table[hash_val] = element->__next_name__;                                                                                                          \
			__post_lookup_action__; /* allow for some post lookup action like locking */                                                                       \
			if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                            \
			return element;                                                                                                                                    \
		}                                                                                                                                                      \
		do {                                                                                                                                                   \
			previous = element;                                                                                                                                \
			element = element->__next_name__;                                                                                                                  \
		} while(element && element->id.full != id.full);                                                                                                       \
		if(!element) {                                                                                                                                         \
			if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                            \
			irt_throw_string_error(IRT_ERR_INTERNAL, "Removing nonexistent element from " #__type__ " table.");                                                \
			return NULL;                                                                                                                                       \
		}                                                                                                                                                      \
		previous->__next_name__ = element->__next_name__;                                                                                                      \
		__post_lookup_action__; /* allow for some post lookup action like locking */                                                                           \
		if(__locked__) irt_spin_unlock(&table_locks[hash_val]);                                                                                                \
		return element;                                                                                                                                        \
	}                                                                                                                                                          \
                                                                                                                                                               \
	static inline void _irt_##__type__##_table_print_impl(FILE* log_file, irt_##__type__** table) {                                                            \
		fprintf(log_file, "--------\n");                                                                                                                       \
		fprintf(log_file, "Dumping " #__type__ "_table (at time %" PRIu64 "):\n", irt_time_convert_ticks_to_ns(irt_time_ticks()));                             \
		for(int i = 0; i < __num_buckets__; ++i) {                                                                                                             \
			irt_##__type__* element = table[i];                                                                                                                \
			if(!element) { continue; }                                                                                                                         \
			fprintf(log_file, "Bucket %d: ", i);                                                                                                               \
			while(element) {                                                                                                                                   \
				fprintf(log_file, "[%d %d %d] (%p) -> ", element->id.node, element->id.thread, element->id.index, (void*)element);                             \
				element = element->__next_name__;                                                                                                              \
			}                                                                                                                                                  \
			fprintf(log_file, "(nil)\n");                                                                                                                      \
		}                                                                                                                                                      \
		fflush(log_file);                                                                                                                                      \
	}

// Defines the function wrappers with a simpler interface used externally.
#define _IRT_DEFINE_LOOKUP_TABLE_FUNCTION_WRAPPERS(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__, __post_lookup_action__)       \
	_IRT_DEFINE_LOOKUP_TABLE_FUNCTIONS(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__, __post_lookup_action__)                   \
	static inline void irt_##__type__##_table_init() {                                                                                                         \
		_irt_##__type__##_table_init_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks);                                                           \
	}                                                                                                                                                          \
	static inline void irt_##__type__##_table_clear() {                                                                                                        \
		_irt_##__type__##_table_clear_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks);                                                          \
	}                                                                                                                                                          \
	static inline void irt_##__type__##_table_cleanup() {                                                                                                      \
		_irt_##__type__##_table_cleanup_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks);                                                        \
	}                                                                                                                                                          \
	static inline void irt_##__type__##_table_insert(irt_##__type__* element) {                                                                                \
		_irt_##__type__##_table_insert_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks, element);                                                \
	}                                                                                                                                                          \
	static inline irt_##__type__* irt_##__type__##_table_lookup(irt_##__type__##_id id) {                                                                      \
		return _irt_##__type__##_table_lookup_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks, id);                                              \
	}                                                                                                                                                          \
	static inline irt_##__type__* irt_##__type__##_table_lookup_or_insert(irt_##__type__* element) {                                                           \
		return _irt_##__type__##_table_lookup_or_insert_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks, element);                               \
	}                                                                                                                                                          \
	static inline irt_##__type__* irt_##__type__##_table_remove(irt_##__type__##_id id) {                                                                      \
		return _irt_##__type__##_table_remove_impl(irt_g_##__type__##_table, irt_g_##__type__##_table_locks, id);                                              \
	}                                                                                                                                                          \
	/* Function dumping the full table to the given FILE (e.g. stdout) */                                                                                      \
	__attribute__((used)) void irt_dbg_print_##__type__##_table(FILE* log_file) {                                                                              \
		_irt_##__type__##_table_print_impl(log_file, irt_g_##__type__##_table);                                                                                \
	}

// Defines the lookup table functions and the needed data structures.
#define _IRT_DEFINE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__, __post_lookup_action__)                         \
	_IRT_DEFINE_LOOKUP_TABLE_DATA(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__)                                                \
	_IRT_DEFINE_LOOKUP_TABLE_FUNCTION_WRAPPERS(__type__, __next_name__, __hashing_expression__, __num_buckets__, __locked__, __post_lookup_action__)

/* Defines a global lookup table and the functions to insert, retrieve and
 * delete elements from it. Note that there are two variants of
 * lookup tables - locked and non-locked ones. Only the locked variant can be
 * modified safely by multiple concurrent threads. The locked lookup table also
 * comes in a variant which supports passing a post_lookup_action code block.
 *
 * Arguments:
 * __type__                 struct type to create table for (assumed to have an "id" member)
 * __next_name__            name of the next pointer in the struct
 * __hashing_expression__   expression that generates a hash value from an id
 * __num_buckets__          number of slots in the hash map
 * __post_lookup_action__   an optional code block argument for locked lookup tables which
 *                          is executed after a successful lookup, lookup_or_insert or remove
 *
 * Note: The globals must still be created using the matching CREATE_LOOKUP_TABLE macro
 */
#define IRT_DEFINE_LOCKED_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__)                                                       \
	_IRT_DEFINE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__, 1, {})

#define IRT_DEFINE_LOCKED_LOOKUP_TABLE_WITH_POST_LOOKUP_ACTION(__type__, __next_name__, __hashing_expression__, __num_buckets__, __post_lookup_action__)       \
	_IRT_DEFINE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__, 1, __post_lookup_action__)

#define IRT_DEFINE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__)                                                              \
	_IRT_DEFINE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__, 0, {})

/* Creates the data structures necessary for the lookup tables to store their
 * data.
 */
#define IRT_CREATE_LOCKED_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__)                                                       \
	irt_##__type__* irt_g_##__type__##_table[__num_buckets__];                                                                                                 \
	irt_spinlock irt_g_##__type__##_table_locks[__num_buckets__];

#define IRT_CREATE_LOOKUP_TABLE(__type__, __next_name__, __hashing_expression__, __num_buckets__) irt_##__type__* irt_g_##__type__##_table[__num_buckets__];

#endif // ifndef __GUARD_UTILS_LOOKUP_TABLES_H
