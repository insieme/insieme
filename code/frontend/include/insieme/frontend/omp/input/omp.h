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
#ifndef _OMP_H
#define _OMP_H

#ifdef __cplusplus
extern "C" {
#endif

typedef enum omp_sched_t { omp_sched_static = 1, omp_sched_dynamic = 2, omp_sched_guided = 3, omp_sched_auto = 4 } omp_sched_t;

int omp_get_thread_num();
int omp_get_num_threads();

void omp_set_num_threads(int num_threads);
int omp_get_max_threads();

void omp_set_dynamic(int dynamic_threads);
int omp_get_dynamic();

void omp_set_nested(int nested);
int omp_get_nested();

void omp_set_schedule(omp_sched_t kind, int modifier);
void omp_get_schedule(omp_sched_t* kind, int* modifier);

void omp_set_max_active_levels(int max_levels);
int omp_get_max_active_levels();

int omp_get_thread_limit();

int omp_in_parallel();

int omp_get_level();
int omp_get_active_level();

int omp_get_team_size(int level);

int omp_get_ancestor_thread_num(int level);

int omp_get_num_procs();

double omp_get_wtime();

// locks

// struct _irt_lock { int d; };
//
// typedef struct _irt_lock irt_lock;
//
//#define omp_lock_t struct _irt_lock
//
// void irt_lock_init(irt_lock* l) {};
// void irt_lock_acquire(irt_lock* l) {};
// void irt_lock_release(irt_lock* l) {};
//
//#define omp_init_lock(_param) irt_lock_init(_param)
//#define omp_set_lock(_param) irt_lock_acquire(_param)
//#define omp_unset_lock(_param) irt_lock_release(_param)

typedef struct _omp_lock_t { int insieme_omp_lock_struct_marker; } omp_lock_t;

void omp_init_lock(omp_lock_t* lock);
void omp_set_lock(omp_lock_t* lock);
int omp_test_lock(omp_lock_t* lock);
void omp_unset_lock(omp_lock_t* lock);


#ifdef __cplusplus
}
#endif

#endif /* _OMP_H */
