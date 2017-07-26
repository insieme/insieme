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
#ifndef __GUARD_UTILS_IMPL_FREQUENCY_GEMS_H
#define __GUARD_UTILS_IMPL_FREQUENCY_GEMS_H

#ifdef _GEMS_ODROID
#include "platforms/odroid/rapmi.h"
#endif

/*
 * These functions provide an interface to get and set CPU frequency settings.
 */

#define IRT_GEM_MIN_FREQ (1 * 1000)   // 1 MHz
#define IRT_GEM_MAX_FREQ (101 * 1000) // 101 MHz
#define IRT_GEM_STEP_FREQ (5 * 1000)  // 5 MHz

#ifdef _GEMS_ODROID
int irt_cpu_freq_a07_max_freq;
unsigned char irt_cpu_freq_cluster;
#endif

int32 irt_cpu_freq_get_available_frequencies(uint32* frequencies, uint32* length) {
#ifdef _GEMS_SIM
	uint32 freq = IRT_GEM_MIN_FREQ;
	uint32 j = 0;

	while(freq <= IRT_GEM_MAX_FREQ) {
		frequencies[j++] = freq;
		freq += IRT_GEM_STEP_FREQ;
	}

	*length = j;
	#elif defined(_GEMS_ODROID)
	*length = 0;
	for(int j = 8; j >= 0; j--) {
		frequencies[(*length)++] = freq_table_a15[j];
	}
	for(int j = 7; j >= 0; j--) {
		frequencies[(*length)++] = freq_table_a07[j] / 2;
	}

	irt_cpu_freq_a07_max_freq = freq_table_a07[7] / 2;

	irt_cpu_freq_cluster = rapmi_get_cluster();
	#endif

	return 0;
}

/*
 * reads all available frequencies for a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_core(const uint32 coreid, uint32* frequencies, uint32* length) {
	IRT_ASSERT(irt_affinity_mask_get_first_cpu(irt_worker_get_current()->affinity) == coreid, IRT_ERR_INVALIDARGUMENT,
	           "DVFS of non-current core is unsupported");
	return irt_cpu_freq_get_available_frequencies(frequencies, length);
}

/*
 * reads all available frequencies for a worker running on a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_worker(const irt_worker* worker, uint32* frequencies, uint32* length) {
	IRT_ASSERT(irt_worker_get_current() == worker, IRT_ERR_INVALIDARGUMENT, "DVFS of non-current worker is unsupported");
	return irt_cpu_freq_get_available_frequencies(frequencies, length);
}

/*
 * gets the current frequency a core of a worker is running at
 */

int32 irt_cpu_freq_get_cur_frequency_worker(const irt_worker* worker) {
	IRT_ASSERT(irt_worker_get_current() == worker, IRT_ERR_INVALIDARGUMENT, "DVFS of non-current worker is unsupported");

	#ifdef _GEMS_SIM
	return rapmi_get_freq();
	#elif defined(_GEMS_ODROID)
	return (irt_cpu_freq_cluster == A15_CLUSTER) ? rapmi_get_freq() : rapmi_get_freq() / 2;
	#endif
}

/*
 * sets the frequency of a core of a worker to a specific value by setting both the min and max to this value
 */

int32 irt_cpu_freq_set_frequency_worker(const irt_worker* worker, const uint32 frequency) {
#ifdef _GEMS_SIM
#elif defined(_GEMS_ODROID)
	// Cluster switching involved
	if(irt_cpu_freq_cluster == A15_CLUSTER && frequency <= irt_cpu_freq_a07_max_freq) {
		irt_cpu_freq_cluster = A07_CLUSTER;
		return rapmi_set_cluster(A07_CLUSTER, frequency * 2);
	} else if(irt_cpu_freq_cluster == A07_CLUSTER && frequency > irt_cpu_freq_a07_max_freq) {
		irt_cpu_freq_cluster = A15_CLUSTER;
		return rapmi_set_cluster(A15_CLUSTER, frequency);
	}

	// No cluster switching involved
	return rapmi_set_freq((irt_cpu_freq_cluster == A07_CLUSTER) ? frequency * 2 : frequency);
#endif
}

/*
 * sets the maximum frequency a core of a worker is allowed to run at
 */

bool irt_cpu_freq_set_max_frequency_worker(const irt_worker* worker, const uint32 frequency) {
	/* No limits on the gemsclaim simulator */
	return (IRT_GEM_MAX_FREQ >= frequency);
}

/*
 * gets the maximum frequency a core of a worker is allowed to run at
 */

int32 irt_cpu_freq_get_max_frequency_worker(const irt_worker* worker) {
	return IRT_GEM_MAX_FREQ;
}

/*
 * gets the maximum frequency a core is allowed to run at
 */

int32 irt_cpu_freq_get_max_frequency_core(const uint32 coreid) {
	return IRT_GEM_MAX_FREQ;
}

/*
 * sets the minimum frequency a core of a worker is allowed to run at
 */

bool irt_cpu_freq_set_min_frequency_worker(const irt_worker* worker, const uint32 frequency) {
	/* No limits on the gemsclaim simulator */
	return (IRT_GEM_MIN_FREQ <= frequency);
}

/*
 * gets the minimum frequency a core of a worker is allowed to run at
 */

int32 irt_cpu_freq_get_min_frequency_worker(const irt_worker* worker) {
	return IRT_GEM_MIN_FREQ;
}

/*
 * gets the minimum frequency a core is allowed to run at
 */

int32 irt_cpu_freq_get_min_frequency_core(const uint32 coreid) {
	return IRT_GEM_MIN_FREQ;
}

/*
 * resets all the min and max frequencies of all cores of all workers to the available min and max reported by the hardware
 */

bool irt_cpu_freq_reset_frequencies() {
	/* No limits on the gemsclaim simulator */
	return true;
}

/*
 * resets the min and max frequencies of a core of a worker to the available min and max
 */

int32 irt_cpu_freq_reset_frequency_worker(const irt_worker* worker) {
	/* No limits on the gemsclaim simulator */

	return (IRT_GEM_MAX_FREQ - IRT_GEM_MIN_FREQ) / IRT_GEM_STEP_FREQ + 1;
}

/*
 * checks whether cores have individual clock frequency domains
 */

bool irt_cpu_freq_cores_have_individual_domains() {
	// TODO: check whether this applies to gemsclaim
	return false;
}

#endif // ifndef __GUARD_UTILS_IMPL_FREQUENCY_GEMS_H
