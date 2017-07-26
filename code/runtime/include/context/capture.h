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
 */

#pragma once
#ifndef __GUARD_CONTEXT_CAPTURE_H
#define __GUARD_CONTEXT_CAPTURE_H

/**
 * This header file is defining a list of macros to be inserted into instrumented or isolated
 * code. Both sets are enabled / disabled using macros.
 *
 * To enable the recording / instrumentation macros, the macro name RECORD has to be defined before
 * including this file. To enable the macros used within the isolated codes, the macro RESTORE has
 * to be defined. Both macros ma be defined at the same time.
 */


#ifdef RECORD

#include "record.h"

// two macros initializing and finalizing the infrastructure
#define INIT()                                                                                                                                                 \
	{                                                                                                                                                          \
		irt_cap_dbi_init();                                                                                                                                    \
		irt_cap_region_init();                                                                                                                                 \
	}
#define FINISH()                                                                                                                                               \
	{                                                                                                                                                          \
		irt_cap_profile_save();                                                                                                                                \
		irt_cap_region_finalize();                                                                                                                             \
		irt_cap_dbi_finalize();                                                                                                                                \
	}

// reading and writing of values
#define READ(A) (irt_cap_read_value(&A, sizeof(A)), A)
// #define WRITE(A,V) 		(A = (V), irt_cap_written_value(&A, sizeof(A)), A)
#define WRITE(A, V) (A = (V), irt_cap_written_value(&A, sizeof(A)))

// reading and writing of pointers
#define READ_PTR(A) (irt_cap_read_pointer((void**)&A), A)
// #define WRITE_PTR(A, L) (A = (V), irt_cap_written_pointer(&A), A)
#define WRITE_PTR(A, V) (A = (V), irt_cap_written_pointer((void**)&A))

// region marking
#define START(ID) irt_cap_region_start(ID)
#define STOP(ID) irt_cap_region_stop(ID)


// block handling
#define REG_BLOCK(Ptr, Size) irt_cap_dbi_register_block(Ptr, Size)->base
#define REG_LOCAL(P) irt_cap_dbi_register_block(&P, sizeof(P))

// allows to tag a block with a ID - taged blocks can be reloaded
#define TAG_BLOCK(P, ID) (REG_LOCAL(P), irt_cap_tag_block(&P, ID))

#else

// in case recording is disabled

// two macros initializing and finalizing the infrastructure
#define INIT()
#define FINISH()

// reading and writing of values
#define READ(A) (A)
#define WRITE(A, V) (A = (V))

// reading and writing of pointers
#define READ_PTR(A) (A)
#define WRITE_PTR(A, L) (A = (L))

// region marking
#define START(ID)
#define STOP(ID)


// block handling
#define REG_BLOCK(Ptr, Size) Ptr
#define REG_LOCAL(P)

// allows to tag a block with a ID - taged blocks can be reloaded
#define TAG_BLOCK(P, ID)

#endif

#ifdef RESTORE
// -- for isolated execution --

#include "restore.h"

// declares and restores a value of the given type from a recorded profile
#define LOAD(TYPE, NAME, RID, TAG)                                                                                                                             \
	TYPE NAME;                                                                                                                                                 \
	irt_cap_profile_get_value(&NAME, RID, TAG, sizeof(TYPE))

// loads a captured value into the given variable
#define LOAD_VALUE(VAR, RID, TAG) irt_cap_profile_get_value(&VAR, RID, TAG, sizeof(VAR))

// verifies whether the life-out values are accurate
#define CHECK_LIFE_OUT true /* not implemented */

// to be called after executing the isolated code region
#define FINALIZE()                                                                                                                                             \
	{ irt_cap_profile_finalize(); }

#endif


#endif // ifndef __GUARD_CONTEXT_CAPTURE_H
