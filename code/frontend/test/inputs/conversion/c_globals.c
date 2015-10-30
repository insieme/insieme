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

#pragma test expect_ir("lit(\"x\": ref<int<4>,f,f>)")
int x;

#pragma test expect_ir("lit(\"y\": ref<real<4>,t,f>)")
const float y;

typedef enum { Bla, Alb } enum_t;
#pragma test expect_ir("lit(\"globalEnum\": ref<struct enum { enum_type : enum_def<IMP_enum_t,enum_entry<Bla,0>,enum_entry<Alb,1>>; value : uint<4>; },f,f>)")
enum_t globalEnum;

typedef struct { int x; } IAmTheTagType;
#pragma test expect_ir("lit(\"tt\": ref<struct IMP_IAmTheTagType { x: int<4>; },f,f>)")
IAmTheTagType tt;

typedef struct _omp_lock_t { int x; } omp_lock_t;
void omp_set_lock(omp_lock_t* lock);
#pragma test expect_ir("lit(\"lck\": ref<struct IMP__omp_lock_t { int<4> x; },f,f>)")
omp_lock_t lck;

int main() {
	globalEnum;
	y;
	tt;
	#pragma test expect_ir("STRING","ptr_from_ref(lck)")
	&lck;
	#pragma test expect_ir("STRING","omp_set_lock(ptr_from_ref(lck))")
	omp_set_lock(&lck);
	return x;
}
