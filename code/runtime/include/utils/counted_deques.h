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

#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"

#include "error_handling.h"

/* Declares a thread-safe counted deque and the functions to insert, 
 * retrieve and delete elements from it.
 * __type__ : struct type to create deque for
 * */
#define IRT_DECLARE_COUNTED_DEQUE(__type__) \
struct _irt_##__type__##_cdeque { \
	irt_##__type__ *start, *end; \
	irt_spinlock lock; \
	uint32 size; \
}; \
typedef struct _irt_##__type__##_cdeque irt_##__type__##_cdeque; \
\
static inline void irt_##__type__##_cdeque_init(irt_##__type__##_cdeque* q); \
static inline void irt_##__type__##_cdeque_clear(irt_##__type__##_cdeque* q); \
static inline void irt_##__type__##_cdeque_cleanup(irt_##__type__##_cdeque* q); \
static inline void irt_##__type__##_cdeque_insert_front(irt_##__type__##_cdeque* q, irt_##__type__* element); \
static inline void irt_##__type__##_cdeque_insert_back(irt_##__type__##_cdeque* q, irt_##__type__* element); \
static inline irt_##__type__* irt_##__type__##_cdeque_pop_front(irt_##__type__##_cdeque* q); \
static inline irt_##__type__* irt_##__type__##_cdeque_pop_back(irt_##__type__##_cdeque* q); \
static inline irt_##__type__* irt_##__type__##_cdeque_take_elem(irt_##__type__##_cdeque* q, irt_##__type__* elem); \
static inline uint32 irt_##__type__##_cdeque_get_size(irt_##__type__##_cdeque* q);


/* Defines a thread-safe counted deque and the functions to insert, 
 * retrieve and delete elements from it.
 * __type__ : struct type to create deque for
 * __next_name__ : name of the next pointer in the struct
 * __prev_name__ : name of the prev pointer in the struct
 * */
#define IRT_DEFINE_COUNTED_DEQUE(__type__, __next_name__, __prev_name__) \
\
static inline void irt_##__type__##_cdeque_init(irt_##__type__##_cdeque* q) { \
	IRT_ASSERT(irt_spin_init(&(q->lock)) == 0, \
				IRT_ERR_INIT, "Failed initializing locks for " #__type__ " deque."); \
	q->start = NULL; \
	q->end = NULL; \
	q->size = 0; \
} \
static inline void irt_##__type__##_cdeque_clear(irt_##__type__##_cdeque* q) { \
	irt_##__type__ *cur = q->start, *temp; \
	while(cur) { \
		temp = cur->__next_name__; \
		cur->__next_name__ = NULL; \
		cur->__prev_name__ = NULL; \
		cur = temp; \
	} \
	q->start = NULL; \
	q->end = NULL; \
	q->size = 0; \
} \
static inline void irt_##__type__##_cdeque_cleanup(irt_##__type__##_cdeque* q) { \
	irt_##__type__##_cdeque_clear(q); \
	irt_spin_destroy(&(q->lock)); \
} \
\
static inline void irt_##__type__##_cdeque_insert_front(irt_##__type__##_cdeque* q, irt_##__type__* element) { \
	element->__prev_name__ = NULL; \
	irt_spin_lock(&(q->lock)); \
	element->__next_name__ = q->start; \
	if(q->start) q->start->__prev_name__ = element; \
	else q->end = element; \
	q->start = element; \
	q->size++; \
	irt_spin_unlock(&(q->lock)); \
} \
static inline void irt_##__type__##_cdeque_insert_back(irt_##__type__##_cdeque* q, irt_##__type__* element) { \
	element->__next_name__ = NULL; \
	irt_spin_lock(&(q->lock)); \
	element->__prev_name__ = q->end; \
	if(q->end) q->end->__next_name__ = element; \
	else q->start = element; \
	q->end = element; \
	q->size++; \
	irt_spin_unlock(&(q->lock)); \
} \
\
static inline irt_##__type__* irt_##__type__##_cdeque_pop_front(irt_##__type__##_cdeque* q) { \
	irt_spin_lock(&(q->lock)); \
	irt_##__type__ *retval = q->start; \
	if(retval) { \
		q->start = retval->__next_name__; \
		if(q->start) q->start->__prev_name__ = NULL; \
		else q->end = NULL; \
		retval->__next_name__ = NULL; \
		q->size--; \
	} \
	irt_spin_unlock(&(q->lock)); \
	return retval; \
} \
static inline irt_##__type__* irt_##__type__##_cdeque_pop_back(irt_##__type__##_cdeque* q) { \
	irt_spin_lock(&(q->lock)); \
	irt_##__type__ *retval = q->end; \
	if(retval) { \
		q->end = retval->__prev_name__; \
		if(q->end) q->end->__next_name__ = NULL; \
		else q->start = NULL; \
		retval->__prev_name__ = NULL; \
		q->size--; \
	} \
	irt_spin_unlock(&(q->lock)); \
	return retval; \
} \
static inline void irt_##__type__##_cdeque_move_front_front(irt_##__type__##_cdeque* source, irt_##__type__##_cdeque* dest, int n) { \
	irt_spin_lock(&(source->lock)); \
	if(n >= source->size) { \
		irt_spin_unlock(&(source->lock)); \
		return; \
	} \
	irt_spin_lock(&(dest->lock)); \
	for(int i=0; i<n; ++i) { \
		irt_##__type__ *val = source->start; \
		source->start = val->__next_name__; \
		source->start->__prev_name__ = NULL; \
		if(dest->start) dest->start->__prev_name__ = val; \
		else dest->end = val; \
		val->__next_name__ = dest->start; \
		dest->start = val; \
	} \
	source->size -= n; \
	dest->size += n; \
	irt_spin_unlock(&(source->lock)); \
	irt_spin_unlock(&(dest->lock)); \
} \
static inline irt_##__type__* irt_##__type__##_cdeque_take_elem(irt_##__type__##_cdeque* q, irt_##__type__* elem) { \
	irt_spin_lock(&(q->lock)); \
	if(q->start == NULL) { /* list is empty */ \
		irt_spin_unlock(&(q->lock)); \
		return NULL; \
	} \
	if(q->start == elem) { /* first elem is target */ \
		irt_##__type__ *retval = q->start; \
		q->start = retval->__next_name__; \
		if(q->start) q->start->__prev_name__ = NULL; \
		else q->end = NULL; \
		q->size--; \
		irt_spin_unlock(&(q->lock)); \
		retval->__next_name__ = NULL; \
		return retval; \
	} \
	if(q->end == elem) { /* last elem is target */ \
		irt_##__type__ *retval = q->end; \
		q->end = retval->__prev_name__; \
		if(q->end) q->end->__next_name__ = NULL; \
		else q->start = NULL; \
		q->size--; \
		irt_spin_unlock(&(q->lock)); \
		retval->__prev_name__ = NULL; \
		return retval; \
	} \
	irt_##__type__ *retval = q->start->__next_name__; \
	while(retval && retval != elem) retval = retval->__next_name__; \
	if(retval != NULL) { \
		retval->__prev_name__->__next_name__ = retval->__next_name__; \
		if(retval->__next_name__) retval->__next_name__->__prev_name__ = retval->__prev_name__; \
		else q->end = retval->__prev_name__; \
		retval->__next_name__ = NULL; \
		retval->__prev_name__ = NULL; \
		q->size--; \
	} \
	irt_spin_unlock(&(q->lock)); \
	return retval; \
} \
static inline uint32 irt_##__type__##_cdeque_get_size(irt_##__type__##_cdeque* q) { \
	return q->size; \
}
