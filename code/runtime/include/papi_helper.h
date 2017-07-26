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
#ifndef __GUARD_PAPI_HELPER_H
#define __GUARD_PAPI_HELPER_H

/*
 *
 * General instructions on how to use PAPI with the runtime instrumentation system:
 *
 * This file provides helper functions for the PAPI interface. The PAPI events
 * to be instrumented can be supplied via an environment variable named
 * IRT_INST_PAPI_EVENTS, separated via colons, e.g.
 * IRT_INST_PAPI_EVENTS="PAPI_TOT_CYC:PAPI_L2_TCM:PAPI_BR_MSP".
 *
 * To find out what events are present on a specific machine and what their names
 * are, navigate to the PAPI installation directory and execute "./bin/papi_avail -a".
 * This will give you hardware information including the maximum number of counters
 * that can be measured in parallel, as well as all preset events that can be counted.
 * Use the preset names like PAPI_TOT_CYC for the environment variable.
 *
 * Since not all events can be measured in arbitrary combinations, one should check
 * first whether a chosen combination is possible. For this, execute
 * "./bin/papi_event_chooser PRESET <papi_event> ...", e.g.
 * "./bin/papi_event_chooser PRESET PAPI_TOT_CYC PAPI_L2_TCM". The program will tell
 * you either what preset events can still be added or which event of your list cannot
 * be counted with another already contained in your list.
 *
 */

#ifdef IRT_USE_PAPI
#include "papi.h"
#endif

#define IRT_INST_PAPI_MAX_COUNTERS 16
//#define IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS 512

void irt_papi_init();

void irt_papi_shutdown();

void irt_papi_finalize_context(irt_context* context);

void irt_papi_finalize_worker(irt_worker* worker);

void irt_papi_start();

void irt_papi_stop(long long int* papi_values);

int64 irt_papi_get_value_by_name(irt_context* context, long long int* papi_values, const char* event_name);

void irt_papi_select_events(irt_worker* worker, irt_context* context, const char* papi_events_string);

void irt_papi_select_events_from_env(irt_worker* worker, irt_context* context);

void irt_papi_setup_context(irt_context* context);

void irt_papi_setup_worker(irt_worker* worker);

#endif // ifndef __GUARD_PAPI_HELPER_H
