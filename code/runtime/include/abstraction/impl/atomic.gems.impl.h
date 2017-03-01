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
#ifndef __GUARD_ABSTRACTION_IMPL_ATOMIC_GEMS_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_ATOMIC_GEMS_IMPL_H

#include "abstraction/atomic.h"
#include "abstraction/spin_locks.h"

asm int atomic_rmw_int_asm(int* ptr, int value) {
	nop;
	nop;
	nop;
	rmw;
	nop;
	nop;
};

int atomic_rmw_int(int* ptr, int value) {
	return atomic_rmw_int_asm(ptr, value);
};

// TODO [_GEMS]: work-around implementation with global lock

/* 0 is unlocked */
irt_spinlock global_lock = 0;

#define IRT_DEFINE_SYNC_OP_AND_FETCH(__type__, __op_string__, __op__)                                                                                          \
	__type__ __sync_##__op_string__##_and_fetch_##__type__(__type__* ptr, __type__ value) {                                                                    \
		__type__ tmp;                                                                                                                                          \
		irt_spin_lock(&global_lock);                                                                                                                           \
		*ptr __op__## = value;                                                                                                                                 \
		tmp = *ptr;                                                                                                                                            \
		irt_spin_unlock(&global_lock);                                                                                                                         \
		return tmp;                                                                                                                                            \
	};

#define IRT_DEFINE_SYNC_FETCH_AND_OP(__type__, __op_string__, __op__)                                                                                          \
	__type__ __sync_fetch_and_##__op_string__##_##__type__(__type__* ptr, __type__ value) {                                                                    \
		__type__ tmp;                                                                                                                                          \
		irt_spin_lock(&global_lock);                                                                                                                           \
		tmp = *ptr;                                                                                                                                            \
		*ptr __op__## = value;                                                                                                                                 \
		irt_spin_unlock(&global_lock);                                                                                                                         \
		return tmp;                                                                                                                                            \
	};

#define IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(__type__)                                                                                                        \
	bool __sync_bool_compare_and_swap_##__type__(__type__* ptr, __type__ oldval, __type__ newval) {                                                            \
		bool res;                                                                                                                                              \
		irt_spin_lock(&global_lock);                                                                                                                           \
		if(res = (*ptr == oldval)) { *ptr = newval; }                                                                                                          \
		irt_spin_unlock(&global_lock);                                                                                                                         \
		return res;                                                                                                                                            \
	};

#define IRT_DEFINE_SYNC_VAL_COMPARE_AND_SWAP(__type__)                                                                                                         \
	__type__ __sync_val_compare_and_swap_##__type__(__type__* ptr, __type__ oldval, __type__ newval) {                                                         \
		__type__ res;                                                                                                                                          \
		irt_spin_lock(&global_lock);                                                                                                                           \
		if((res = *ptr) == oldval) { *ptr = newval; }                                                                                                          \
		irt_spin_unlock(&global_lock);                                                                                                                         \
		return res;                                                                                                                                            \
	};

/* 32-bit signed int versions */

IRT_DEFINE_SYNC_FETCH_AND_OP(int32, add, +)
IRT_DEFINE_SYNC_FETCH_AND_OP(int32, sub, -)

IRT_DEFINE_SYNC_OP_AND_FETCH(int32, add, +)
IRT_DEFINE_SYNC_OP_AND_FETCH(int32, sub, -)
IRT_DEFINE_SYNC_OP_AND_FETCH(int32, or, | )
IRT_DEFINE_SYNC_OP_AND_FETCH(int32, and, &)
IRT_DEFINE_SYNC_OP_AND_FETCH(int32, xor, ^)

IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(int32)
IRT_DEFINE_SYNC_VAL_COMPARE_AND_SWAP(int32)

/* 32-bit unsigned int versions */

IRT_DEFINE_SYNC_FETCH_AND_OP(uint32, add, +)
IRT_DEFINE_SYNC_FETCH_AND_OP(uint32, sub, -)

IRT_DEFINE_SYNC_OP_AND_FETCH(uint32, add, +)
IRT_DEFINE_SYNC_OP_AND_FETCH(uint32, sub, -)
IRT_DEFINE_SYNC_OP_AND_FETCH(uint32, or, | )
IRT_DEFINE_SYNC_OP_AND_FETCH(uint32, and, &)
IRT_DEFINE_SYNC_OP_AND_FETCH(uint32, xor, ^)

IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(uint32)
IRT_DEFINE_SYNC_VAL_COMPARE_AND_SWAP(uint32)

/* 64-bit signed int versions */

IRT_DEFINE_SYNC_FETCH_AND_OP(int64, add, +)
IRT_DEFINE_SYNC_FETCH_AND_OP(int64, sub, -)

IRT_DEFINE_SYNC_OP_AND_FETCH(int64, add, +)
IRT_DEFINE_SYNC_OP_AND_FETCH(int64, sub, -)
IRT_DEFINE_SYNC_OP_AND_FETCH(int64, or, | )
IRT_DEFINE_SYNC_OP_AND_FETCH(int64, and, &)
IRT_DEFINE_SYNC_OP_AND_FETCH(int64, xor, ^)

IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(int64)
IRT_DEFINE_SYNC_VAL_COMPARE_AND_SWAP(int64)

/* 64-bit unsigned int versions */

IRT_DEFINE_SYNC_FETCH_AND_OP(uint64, add, +)
IRT_DEFINE_SYNC_FETCH_AND_OP(uint64, sub, -)

IRT_DEFINE_SYNC_OP_AND_FETCH(uint64, add, +)
IRT_DEFINE_SYNC_OP_AND_FETCH(uint64, sub, -)
IRT_DEFINE_SYNC_OP_AND_FETCH(uint64, or, | )
IRT_DEFINE_SYNC_OP_AND_FETCH(uint64, and, &)
IRT_DEFINE_SYNC_OP_AND_FETCH(uint64, xor, ^)

IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(uint64)
IRT_DEFINE_SYNC_VAL_COMPARE_AND_SWAP(uint64)

/* unitptr_t versions */

IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(uintptr_t)

/* bool versions */

IRT_DEFINE_SYNC_BOOL_COMPARE_AND_SWAP(bool)

#endif // ifndef __GUARD_ABSTRACTION_IMPL_ATOMIC_GEMS_IMPL_H
