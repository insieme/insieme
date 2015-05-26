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

#pragma once
#ifndef __GUARD_UTILS_IMPL_FREQUENCY_STD_H
#define __GUARD_UTILS_IMPL_FREQUENCY_STD_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "utils/affinity.h"
#include "utils/impl/affinity.impl.h"
#include "hwinfo.h"

#define FREQ_PATH_STRING "/sys/devices/system/cpu/cpu%u/cpufreq/%s"
#define FREQ_PATH_MAX_LENGTH 128
#define FREQ_MIN_STRING "scaling_min_freq"
#define FREQ_MAX_STRING "scaling_max_freq"
#define FREQ_CUR_STRING "scaling_cur_freq"

typedef enum {
	SCALING_MAX_FREQ = 0,
	SCALING_MIN_FREQ,
} irt_cpu_frequency_type;

/*
 * These functions provide an interface to get and set CPU frequency settings.
 *
 * HyperThreading support: Values are only read for the first HT unit, but written for both of them for safety reasons.
 */

/*
 * reads all available frequencies for all available cores as a list into the provided pointer
 */
int32 irt_cpu_freq_get_available_frequencies(uint32* frequencies, uint32* length) {
    return irt_cpu_freq_get_available_frequencies_core(0, frequencies, length);
}

/*
 * reads all available frequencies for a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_core(const uint32 coreid, uint32* frequencies, uint32* length) {
	char path_to_cpufreq[FREQ_PATH_MAX_LENGTH];
	uint32 counter = 1;
	uint32 frequencies_temp[IRT_INST_MAX_CPU_FREQUENCIES] = { 0 };

	sprintf(path_to_cpufreq, FREQ_PATH_STRING, coreid, "scaling_available_frequencies");
	FILE* file = fopen(path_to_cpufreq, "r");

	if(file == NULL) {
		IRT_DEBUG("Instrumentation: Unable to open frequency file for reading for core %u, file %s, reason: %s\n", coreid, path_to_cpufreq, strerror(errno));
		return -2;
	}

	if(fscanf(file, "%u", &frequencies_temp[0]) != 1) {
		IRT_DEBUG("Instrumentation: Unable to read available frequencies for core %u, file %s, reason: %s\n", coreid, path_to_cpufreq, strerror(errno));
		fclose(file);
		return -1;
	}

	while(fscanf(file, " %u", &frequencies_temp[counter]) == 1) {
		IRT_ASSERT(counter < IRT_INST_MAX_CPU_FREQUENCIES, IRT_ERR_INSTRUMENTATION, "Instrumentation: number of available frequencies %u for core %u exceeds IRT_INST_MAX_CPU_FREQUENCIES", counter, coreid);
		++counter;
	}

	for(uint32 j = 0; j < counter; ++j)
		frequencies[j] = frequencies_temp[j]/1000;
	*length = counter;

	fclose(file);

	return 0;
}

/*
 * reads all available frequencies for a worker running on a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_worker(const irt_worker* worker, uint32* frequencies, uint32* length) {
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	return irt_cpu_freq_get_available_frequencies_core(coreid, frequencies, length);
}

/*
 * low-level function that writes the frequency for a given /sys file path
 */

bool _irt_cpu_freq_write(const char* path_to_cpufreq, const uint32 frequency) {
	FILE* file = fopen(path_to_cpufreq, "w");

	if(file == NULL) {
		IRT_DEBUG("Instrumentation: Unable to open frequency file %s for writing, reason: %s\n", path_to_cpufreq, strerror(errno));
		return false;
	}

	// frequency is given in MHz, not KHz...
	if(fprintf(file, "%u\n", frequency*1000) < 1) {
		IRT_DEBUG("Instrumentation: Unable to write frequency to file %s, reason: %s\n", path_to_cpufreq, strerror(errno));
		fclose(file);
		return false;
	}

	fclose(file);

	return true;
}

/*
 * low-level function that reads the frequency for a given /sys file path
 */

int32 _irt_cpu_freq_read(const char* path_to_cpufreq) {
	uint32 temp = 0;
	int32 retval = 0;
	FILE* file = fopen(path_to_cpufreq, "r");

	if(file == NULL) {
		IRT_DEBUG("Instrumentation: Unable to open frequency file %s for reading, reason: %s\n", path_to_cpufreq, strerror(errno));
		return -2;
	}

	if((retval = fscanf(file, "%u", &temp)) < 1) {
		IRT_DEBUG("Instrumentation: Unable to read frequency from file %s, reason: %s\n", path_to_cpufreq, strerror(errno));
		fclose(file);
		return -1;
	}

	fclose(file);

	// returning MHz here, not KHz......
	return temp/1000;
}

bool __irt_cpu_freq_set(const uint32 coreid, const uint32 frequency, irt_cpu_frequency_type freq_type) {

	if(irt_g_frequency_cur_state[coreid][freq_type] == frequency)
		return true;

	char path_to_cpufreq[FREQ_PATH_MAX_LENGTH];

	if(freq_type == SCALING_MIN_FREQ)
		sprintf(path_to_cpufreq, FREQ_PATH_STRING, coreid, FREQ_MIN_STRING);
	else
		sprintf(path_to_cpufreq, FREQ_PATH_STRING, coreid, FREQ_MAX_STRING);

	if(_irt_cpu_freq_write(path_to_cpufreq, frequency)) {
		irt_affinity_mask_set(&irt_g_frequency_setting_modified_mask, coreid, true);
		irt_g_frequency_cur_state[coreid][freq_type] = frequency;
		return true;
	}
	return false;
}

/*
 * sets the respective frequency setting (max/min/current/...) a core should run at
 */

bool _irt_cpu_freq_set(const uint32 coreid, const uint32 frequency, irt_cpu_frequency_type freq_type) {

	bool ret_a = true, ret_b = true;

	ret_a = __irt_cpu_freq_set(coreid, frequency, freq_type);

	if(irt_hw_get_hyperthreading_enabled()) {
		ret_b = __irt_cpu_freq_set(irt_hw_get_sibling_hyperthread(coreid), frequency, freq_type);
	}

	// will show failure if hyperthreading is active but setting the freq for either logical CPU in the OS failed
	return ret_a && ret_b;
}

int32 _irt_cpu_freq_get_uncached(const uint32 coreid, const char* freq_type) {
	char path_to_cpufreq[FREQ_PATH_MAX_LENGTH];
	sprintf(path_to_cpufreq, FREQ_PATH_STRING, coreid, freq_type);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * gets the respective frequency setting (max/min/current/...) a core is running at
 */

int32 _irt_cpu_freq_get_cached(const uint32 coreid, irt_cpu_frequency_type freq_type) {

	if(freq_type == SCALING_MIN_FREQ || freq_type == SCALING_MAX_FREQ) {
		if(irt_g_frequency_cur_state[coreid][freq_type] != 0)
			return irt_g_frequency_cur_state[coreid][freq_type];
	}

	if(freq_type == SCALING_MIN_FREQ)
		return _irt_cpu_freq_get_uncached(coreid, FREQ_MIN_STRING);
	else if(freq_type == SCALING_MAX_FREQ)
		return _irt_cpu_freq_get_uncached(coreid, FREQ_MAX_STRING);
	else {
		IRT_ASSERT(false, IRT_ERR_INVALIDARGUMENT, "CPU frequency cache can only be used for MIN and MAX values!");
		return -1;
	}
}

/*
 * gets the current frequency a core of a worker is running at
 */

int32 irt_cpu_freq_get_cur_frequency_worker(const irt_worker* worker) {
	return _irt_cpu_freq_get_uncached(irt_affinity_mask_get_first_cpu(worker->affinity), FREQ_CUR_STRING);
}

/*
 * sets the maximum frequency a core of a worker is allowed to run at
 */

bool irt_cpu_freq_set_max_frequency_worker(const irt_worker* worker, const uint32 frequency) {
	return _irt_cpu_freq_set(irt_affinity_mask_get_first_cpu(worker->affinity), frequency, SCALING_MAX_FREQ);
}

/*
 * gets the maximum frequency a core of a worker is allowed to run at
 */

int32 irt_cpu_freq_get_max_frequency_worker(const irt_worker* worker) {
	return irt_cpu_freq_get_max_frequency_core(irt_affinity_mask_get_first_cpu(worker->affinity));
}

/*
 * gets the maximum frequency a core is allowed to run at
 */

int32 irt_cpu_freq_get_max_frequency_core(const uint32 coreid) {
	return _irt_cpu_freq_get_cached(coreid, SCALING_MAX_FREQ);
}

/*
 * sets the minimum frequency a core of a worker is allowed to run at
 */

bool irt_cpu_freq_set_min_frequency_worker(const irt_worker* worker, const uint32 frequency) {
	return _irt_cpu_freq_set(irt_affinity_mask_get_first_cpu(worker->affinity), frequency, SCALING_MIN_FREQ);
}

/*
 * gets the minimum frequency a core of a worker is allowed to run at
 */

int32 irt_cpu_freq_get_min_frequency_worker(const irt_worker* worker) {
	return irt_cpu_freq_get_min_frequency_core(irt_affinity_mask_get_first_cpu(worker->affinity));
}

/*
 * gets the minimum frequency a core is allowed to run at
 */

int32 irt_cpu_freq_get_min_frequency_core(const uint32 coreid) {
	return _irt_cpu_freq_get_cached(coreid, SCALING_MIN_FREQ);
}

/*
 * resets all the min and max frequencies of all cores of all workers to the available min and max reported by the hardware
 */

bool irt_cpu_freq_reset_frequencies() {
	bool retval = true;
	uint32 frequency = 0;
	uint32 total_cores = irt_hw_get_num_cpus();

	for(uint32 coreid = 0; coreid < total_cores; ++coreid) {
		if(irt_affinity_mask_is_set(irt_g_frequency_setting_modified_mask, coreid)) {
			frequency = _irt_cpu_freq_get_uncached(coreid, "cpuinfo_max_freq");
			retval &= _irt_cpu_freq_set(coreid, frequency, SCALING_MAX_FREQ);
			frequency = _irt_cpu_freq_get_uncached(coreid, "cpuinfo_min_freq");
			retval &= _irt_cpu_freq_set(coreid, frequency, SCALING_MIN_FREQ);
		}
	}
	if(retval)
		irt_affinity_mask_clear(&irt_g_frequency_setting_modified_mask);
	return retval;
}

/*
 * resets the min and max frequencies of a core of a worker to the available min and max
 */

int32 irt_cpu_freq_reset_frequency_worker(const irt_worker* worker) {
	int32 retval = 0;
	uint32 freqs[IRT_INST_MAX_CPU_FREQUENCIES];
	uint32 length;
	if((retval = irt_cpu_freq_get_available_frequencies_worker(worker, freqs, &length)) == 0) {
		irt_cpu_freq_set_max_frequency_worker(worker, freqs[0]);
		irt_cpu_freq_set_min_frequency_worker(worker, freqs[length-1]);
	} else
		return retval;
	return 0;
}

/*
 * sets the frequency of a core of a worker to a specific value by setting both the min and max to this value
 */

int32 irt_cpu_freq_set_frequency_worker(const irt_worker* worker, const uint32 frequency) {
	int32 old_min_freq = irt_cpu_freq_get_min_frequency_worker(worker);

	// min always has to be >= max, so if old_min is larger than max, first set new min
	if((uint32)old_min_freq > frequency) {
		irt_cpu_freq_set_min_frequency_worker(worker, frequency);
		irt_cpu_freq_set_max_frequency_worker(worker, frequency);
	} else {
		irt_cpu_freq_set_max_frequency_worker(worker, frequency);
		irt_cpu_freq_set_min_frequency_worker(worker, frequency);
	}

	return 0;
}

int32 irt_cpu_freq_set_frequency_core(const uint32 coreid, const uint32 frequency) {
	int32 old_min_freq = irt_cpu_freq_get_min_frequency_core(coreid);

	if((uint32)old_min_freq > frequency) {
		_irt_cpu_freq_set(coreid, frequency, SCALING_MIN_FREQ);
		_irt_cpu_freq_set(coreid, frequency, SCALING_MAX_FREQ);
	} else {
		_irt_cpu_freq_set(coreid, frequency, SCALING_MAX_FREQ);
		_irt_cpu_freq_set(coreid, frequency, SCALING_MIN_FREQ);
	}

return 0;
}

/*
 * this function blindly sets the frequency of cores belonging to a socket, whether the runtime actually has workers running on them or not
 */

bool irt_cpu_freq_set_frequency_socket(const uint32 socket, const uint32 frequency) {
	bool retval = 0;
	uint32 core_start = socket * irt_hw_get_num_cores_per_socket();
	uint32 core_end = (socket + 1) * irt_hw_get_num_cores_per_socket();

	// first HT unit
	for(uint32 coreid = core_start; coreid < core_end; ++coreid) {
		// write max, min, max because we don't know the old frequency and setting max below min is rejected by the OS
		// reading the old frequency before to determine that has approximately the same overhead
		retval |= _irt_cpu_freq_set(coreid, frequency, SCALING_MAX_FREQ);
		retval |= _irt_cpu_freq_set(coreid, frequency, SCALING_MIN_FREQ);
		retval |= _irt_cpu_freq_set(coreid, frequency, SCALING_MAX_FREQ);
	}

	return retval;
}

/*
 * This function sets the frequencies of all cores of sockets from an environmental variable. Only actual values are supported as of now, no placeholders like MIN/MAX/OS.
 */

int32 irt_cpu_freq_set_frequency_socket_env() {
	char* freq_str_orig = getenv(IRT_CPU_FREQUENCIES);
	int32 retval = 0;

	if(freq_str_orig) {
		uint32 socketid = 0;

		// get information from first core of the system, assume homogeneity
		char *tok = strtok(freq_str_orig, ",");

		while(tok) {
			uint32 freq = atoi(tok);
			irt_cpu_freq_set_frequency_socket(socketid, freq);
			tok = strtok(NULL, ",");
			++socketid;
		}
	} else {
		retval = 1;
	}

	// the rest of this function is only runtime log file output
	// TODO: we only output the first core of each socket for brevity, but this should be changed

	uint32 cpu_freq_list[IRT_HW_MAX_NUM_SOCKETS];

	for(uint32 socketid = 0; socketid < irt_hw_get_num_sockets(); ++socketid)
		cpu_freq_list[socketid] = _irt_cpu_freq_get_uncached(socketid*irt_hw_get_num_cores_per_socket(), "scaling_cur_freq");

	char cpu_freq_output[1024] = { 0 };
	char* cur = cpu_freq_output;
	if(!irt_affinity_mask_is_empty(irt_g_frequency_setting_modified_mask))
		cur += sprintf(cur, "set, ");
	else
		cur += sprintf(cur, "not set, ");

	for(uint32 i = 0; i < irt_hw_get_num_sockets(); ++i)
		cur += sprintf(cur, "%u, ", cpu_freq_list[i]);
	// cutting off the last comma and whitespace
	*(cur-2) = '\0';

	irt_log_setting_s("IRT_CPU_FREQUENCIES", cpu_freq_output);
	return retval;
}

/*
 * prints the current frequency of all cores of all workers to stdout
 */

int32 irt_cpu_freq_print_cur_frequency() {
	int32 retval = 0;

	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		if((retval = irt_cpu_freq_get_cur_frequency_worker(irt_g_workers[i])) > 0) {
			printf("%u:%u ", i, retval);
		} else
			return retval;
	}
	printf("\n");
	return 0;
}

/*
 * sets the frequency of all cores of all workers to a given value
 */

int32 irt_cpu_freq_set_frequency(const uint32 frequency) {
	int32 retval = 0;
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		if((retval = irt_cpu_freq_set_frequency_worker(irt_g_workers[i], frequency)) != 0) {
			return retval;
		}
	}
	return 0;
}


#endif // ifndef __GUARD_UTILS_IMPL_FREQUENCY_STD_H
