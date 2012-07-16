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

// direct mapping to GCC primitives
// TODO windows implementation, should map to InterlockedCompareExchange64 etc. 

#define irt_atomic_fetch_and_add(__location, __value, ...)  __sync_fetch_and_add(__location, __value, ##__VA_ARGS__)
#define irt_atomic_fetch_and_sub(__location, __value, ...)  __sync_fetch_and_sub(__location, __value, ##__VA_ARGS__)
#define irt_atomic_fetch_and_or(__location, __value, ...)   __sync_fetch_and_or(__location, __value, ##__VA_ARGS__)
#define irt_atomic_fetch_and_and(__location, __value, ...)  __sync_fetch_and_and(__location, __value, ##__VA_ARGS__)
#define irt_atomic_fetch_and_xor(__location, __value, ...)  __sync_fetch_and_xor(__location, __value, ##__VA_ARGS__)
#define irt_atomic_fetch_and_nand(__location, __value, ...) __sync_fetch_and_nand(__location, __value, ##__VA_ARGS__)

#define irt_atomic_add_and_fetch(__location, __value, ...)  __sync_add_and_fetch(__location, __value, ##__VA_ARGS__)
#define irt_atomic_sub_and_fetch(__location, __value, ...)  __sync_sub_and_fetch(__location, __value, ##__VA_ARGS__)
#define irt_atomic_or_and_fetch(__location, __value, ...)   __sync_or_and_fetch(__location, __value, ##__VA_ARGS__)
#define irt_atomic_and_and_fetch(__location, __value, ...)  __sync_and_and_fetch(__location, __value, ##__VA_ARGS__)
#define irt_atomic_xor_and_fetch(__location, __value, ...)  __sync_xor_and_fetch(__location, __value, ##__VA_ARGS__)
#define irt_atomic_nand_and_fetch(__location, __value, ...) __sync_nand_and_fetch(__location, __value, ##__VA_ARGS__)

/**
 * These builtins perform an atomic compare and swap. That is, if the current value of *__location is oldval, then write newval into *__location.
 *
 * irt_atomic_bool_compare_and_swap returns true if successful, false otherwise
 * irt_atomic_val_compare_and_swap returns the value of *__location before the operation
 */
#define irt_atomic_bool_compare_and_swap(__location, __oldval, __newval, ...) __sync_bool_compare_and_swap(__location, __oldval, __newval, ##__VA_ARGS__)
#define irt_atomic_val_compare_and_swap(__location, __oldval, __newval, ...)  __sync_val_compare_and_swap(__location, __oldval, __newval, ##__VA_ARGS__)

#define irt_atomic_lock_test_and_set(__location,  __value, ...) __sync_lock_test_and_set(__location, __value, ##__VA_ARGS__)
#define irt_atomic_lock_release(__location, ...)                __sync_lock_release(__location, ##__VA_ARGS__)

// convenience
// explicitly cast return value to void to supress warnings
#define irt_atomic_inc(__location) (void)irt_atomic_fetch_and_add(__location, 1)
#define irt_atomic_dec(__location) (void)irt_atomic_fetch_and_sub(__location, 1)
