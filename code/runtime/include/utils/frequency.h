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
#ifndef __GUARD_UTILS_FREQUENCY_H
#define __GUARD_UTILS_FREQUENCY_H

#include "config.h"

/*
 * These functions provide an interface to get and set CPU frequency settings.
 *
 * HyperThreading support: Values are only read for the first HT unit, but written for both of them for safety reasons.
 */

// cached information about the current cpu min/max frequencies, eliminates superfluous writes
static uint32 irt_g_cpu_freq_cur_state[IRT_MAX_CORES][2];

/*
 * reads all available frequencies for all available cores as a list into the provided pointer
 */
int32 irt_cpu_freq_get_available_frequencies(uint32* frequencies, uint32* length);

/*
 * reads all available frequencies for a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_core(const uint32 coreid, uint32* frequencies, uint32* length);

/*
 * reads all available frequencies for a worker running on a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_worker(const irt_worker* worker, uint32* frequencies, uint32* length);

/*
 * gets the current frequency a core of a worker is running at
 */

int32 irt_cpu_freq_get_cur_frequency_worker(const irt_worker* worker);

/*
 * sets the maximum frequency a core of a worker is allowed to run at
 */

bool irt_cpu_freq_set_max_frequency_worker(const irt_worker* worker, const uint32 frequency);

/*
 * gets the maximum frequency a core of a worker is allowed to run at
 */

int32 irt_cpu_freq_get_max_frequency_worker(const irt_worker* worker);

/*
 * gets the maximum frequency of a core is allowed to run at
 */

int32 irt_cpu_freq_get_max_frequency_core(const uint32 core);

/*
 * sets the minimum frequency a core of a worker is allowed to run at
 */

bool irt_cpu_freq_set_min_frequency_worker(const irt_worker* worker, const uint32 frequency);

/*
 * gets the minimum frequency a core of a worker is allowed to run at
 */

int32 irt_cpu_freq_get_min_frequency_worker(const irt_worker* worker);

/*
 * gets the minimum frequency of a core is allowed to run at
 */

int32 irt_cpu_freq_get_min_frequency_core(const uint32 core);

/*
 * resets all the min and max frequencies of all cores of all workers to the available min and max reported by the hardware
 */

bool irt_cpu_freq_reset_frequencies();

/*
 * resets the min and max frequencies of a core of a worker to the available min and max
 */

int32 irt_cpu_freq_reset_frequency_worker(const irt_worker* worker);

/*
 * sets the frequency of a core of a worker to a specific value by setting both the min and max to this value
 */

int32 irt_cpu_freq_set_frequency_worker(const irt_worker* worker, const uint32 frequency);

int32 irt_cpu_freq_set_frequency_core(const uint32 coreid, const uint32 frequency);

/*
 * sets the frequency of a core of a worker to the value specified by the environment variable
 *
 * MAX = maximum value
 * MIN = minimum value
 * OS = leave it to the OS to do DVFS
 * numerical value = clock frequency in MHz
 *
 * only frequencies offered by the operating system / hardware (c.f. scaling_available_frequencies) are actually written
 */

int32 irt_cpu_freq_set_frequency_worker_env(const irt_worker* worker);

#ifndef _GEMS

/*
 * this function blindly sets the frequency of cores belonging to a socket, whether the runtime actually has workers running on them or not
 */

bool irt_cpu_freq_set_frequency_socket(const uint32 socket, const uint32 frequency);

/*
 * This function sets the frequencies of all cores of sockets from an environmental variable. Only actual values are supported as of now, no placeholders like
 * MIN/MAX/OS.
 */

int32 irt_cpu_freq_set_frequency_socket_env();

/*
 * prints the current frequency of all cores of all workers to stdout
 */

int32 irt_cpu_freq_print_cur_frequency();

/*
 * sets the frequency of all cores of all workers to a given value
 */

int32 irt_cpu_freq_set_frequency(const uint32 frequency);

/*
 * checks whether cores have individual clock frequency domains
 */

bool irt_cpu_freq_cores_have_individual_domains();

#endif

#endif // ifndef __GUARD_UTILS_FREQUENCY_H
