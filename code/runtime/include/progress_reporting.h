/**
 * Copyright (c) 2002-2018 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_PERFORMANCE_ESTIMATION_H
#define __GUARD_PERFORMANCE_ESTIMATION_H

#ifdef IRT_ENABLE_PROGRESS_REPORTING

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "irt_globals.h"
#include "irt_maintenance.h"
#include "utils/timing.h"
#include "worker.h"

#ifndef IRT_PROGRESS_REPORTING_INTERVAL
#define IRT_PROGRESS_REPORTING_INTERVAL 128
#endif


typedef struct irt_progress_reporting_data {
	uint64 start_time;
} irt_progress_reporting_data;

// report some global progress
static inline uint64 irt_report_progress(uint64 progress) {
	static uint64 global_progress = 0;
	return global_progress += progress;
}

// report some progress within a worker
static inline void irt_report_progress_thread(uint64 progress) {
	irt_worker_get_current()->reported_progress += progress;
}

uint64 _irt_progress_reporting_get_worker_progress(irt_worker* worker) {
	return worker->reported_progress;
}

uint64 _irt_progress_reporting_print_progress_callback(void* data) {
	irt_progress_reporting_data* reporting_data = (irt_progress_reporting_data*) data;

	uint64 current_time = irt_time_ms();
	fprintf(stderr, "%" PRIu64 " ", current_time - reporting_data->start_time);

//	// print progress individually for each worker
//	uint64 global_progress = irt_report_progress(0);
//	for(int i = 0; i < irt_g_worker_count; ++i) {
//		fprintf(stderr, "%" PRIu64 " ", _irt_progress_reporting_get_worker_progress(irt_g_workers[i]) + global_progress);
//	}

	// print sum of worker progress
	uint64 global_progress = irt_report_progress(0) * irt_g_worker_count;
	for(int i = 0; i < irt_g_worker_count; ++i) {
		global_progress += _irt_progress_reporting_get_worker_progress(irt_g_workers[i]);
	}
	fprintf(stderr, "%" PRIu64, global_progress);

//	// print maximum progress increment for each worker
//	uint64 max_diff = 0;
//	for(int i = 0; i < irt_g_worker_count; ++i) {
//		uint64 new_value = _irt_progress_reporting_get_worker_progress(irt_g_workers[i]);
//		uint64 current_diff = new_value - reporting_data->last_progress[i];
//		reporting_data->last_progress[i] = new_value;
//		max_diff = MAX(max_diff, current_diff);
//	}
//	fprintf(stderr, "%" PRIu64, max_diff);

	fprintf(stderr, "\n");
	return IRT_PROGRESS_REPORTING_INTERVAL;
}

irt_progress_reporting_data* _irt_progress_reporting_get_data() {
	static irt_progress_reporting_data reporting_data;
	return &reporting_data;
}

void irt_progress_reporting_init() {
	irt_progress_reporting_data* reporting_data = _irt_progress_reporting_get_data();
	static irt_maintenance_lambda ml = { _irt_progress_reporting_print_progress_callback, reporting_data, IRT_PROGRESS_REPORTING_INTERVAL, NULL };
	reporting_data->start_time = irt_time_ms();
	irt_maintenance_register(&ml);
}

void irt_progress_reporting_end() {
	_irt_progress_reporting_print_progress_callback(_irt_progress_reporting_get_data());
}


#endif // IRT_ENABLE_PROGRESS_REPORTING
#endif // ifndef __GUARD_PERFORMANCE_ESTIMATION_H
