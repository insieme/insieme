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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "affinity.h"
#include "utils/impl/affinity.impl.h"
#include "hwinfo.h"

#define FREQ_PATH_PREFIX "/sys/devices/system/cpu/cpu%u/cpufreq/"

/*
 * These functions provide an interface to read and set cpu frequency settings.
 *
 * Hyperthreading support: Values are only read for the first HT unit, but written for both of them for safety reasons.
 */

bool irt_g_frequency_setting_specified;

int32 irt_cpu_freq_get_available_frequencies_core(uint32 coreid, uint32** frequencies, uint32* length) {

	char path_to_cpufreq[1024] = { 0 };
	uint32 counter = 1;
//	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);

	uint32 frequencies_temp[IRT_INST_MAX_CPU_FREQUENCIES] = { 0 };

	sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_available_frequencies", coreid);

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

	while(fscanf(file, " %u", &frequencies_temp[counter]) == 1) ++counter;

	*length = counter;
	(*frequencies) = (uint32*)malloc(counter*sizeof(uint32));

	for(uint32 j = 0; j < counter; ++j)
		(*frequencies)[j] = frequencies_temp[j]/1000;

	fclose(file);

	return 0;
}

/*
 * reads all available frequencies for a worker running on a specific core as a list into the provided pointer
 */

int32 irt_cpu_freq_get_available_frequencies_worker(irt_worker* worker, uint32** frequencies, uint32* length) {
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

/*
 * gets the current frequency a core of a worker is running on
 */

int32 irt_cpu_freq_get_cur_frequency_worker(irt_worker* worker) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_cur_freq", coreid);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * sets the maximum frequency a core of a worker is allowed to run on
 */

bool irt_cpu_freq_set_max_frequency_worker(irt_worker* worker, const uint32 frequency) {
	char path_to_cpufreq_a[1024] = { 0 };
	char path_to_cpufreq_b[1024] = { 0 };
	uint32 coreid_a = irt_affinity_mask_get_first_cpu(worker->affinity);
	uint32 coreid_b = irt_get_sibling_hyperthread(coreid_a);
	sprintf(path_to_cpufreq_a, FREQ_PATH_PREFIX "scaling_max_freq", coreid_a);
	sprintf(path_to_cpufreq_b, FREQ_PATH_PREFIX "scaling_max_freq", coreid_b);
	bool ret_a = _irt_cpu_freq_write(path_to_cpufreq_a, frequency);
	bool ret_b = _irt_cpu_freq_write(path_to_cpufreq_b, frequency);
	return ret_a || ret_b;
}

/*
 * gets the maximum frequency a core of a worker is allowed to run on
 */

int32 irt_cpu_freq_get_max_frequency_worker(irt_worker* worker) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_max_freq", coreid);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * sets the minimum frequency a core of a worker is allowed to run on
 */

bool irt_cpu_freq_set_min_frequency_worker(irt_worker* worker, const uint32 frequency) {
	char path_to_cpufreq_a[1024] = { 0 };
	char path_to_cpufreq_b[1024] = { 0 };
	uint32 coreid_a = irt_affinity_mask_get_first_cpu(worker->affinity);
	uint32 coreid_b = irt_get_sibling_hyperthread(coreid_a);
	sprintf(path_to_cpufreq_a, FREQ_PATH_PREFIX "scaling_min_freq", coreid_a);
	sprintf(path_to_cpufreq_b, FREQ_PATH_PREFIX "scaling_min_freq", coreid_b);
	bool ret_a = _irt_cpu_freq_write(path_to_cpufreq_a, frequency);
	bool ret_b = _irt_cpu_freq_write(path_to_cpufreq_b, frequency);
	return ret_a || ret_b;
}

/*
 * gets the minimum frequency a core of a worker is allowed to run on
 */

int32 irt_cpu_freq_get_min_frequency_worker(irt_worker* worker) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_min_freq", coreid);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * resets all the min and max frequencies of all cores of all workers to the available min and max reported by the hardware
 */

bool irt_cpu_freq_reset_frequency() {
	bool retval = 0;
	uint32 frequency = 0;
	char path_to_cpufreq[1024] = { 0 };
	uint32 total_cores = irt_get_num_threads_per_core() * irt_get_num_cores_per_socket() * irt_get_num_sockets();

	for(uint32 coreid = 0; coreid < total_cores; ++coreid) {

		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "cpuinfo_max_freq", coreid);
		frequency = _irt_cpu_freq_read(path_to_cpufreq);
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_max_freq", coreid);
		retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "cpuinfo_min_freq", coreid);
		frequency = _irt_cpu_freq_read(path_to_cpufreq);
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_min_freq", coreid);
		retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
	}
	return retval;
}

/*
 * resets the min and max frequencies of a core of a worker to the available min and max
 */

int32 irt_cpu_freq_reset_frequency_worker(irt_worker* worker) {
	int32 retval = 0;
	uint32* freqs;
	uint32 length;
	if((retval = irt_cpu_freq_get_available_frequencies_worker(worker, &freqs, &length)) == 0) {
		irt_cpu_freq_set_max_frequency_worker(worker, freqs[0]);
		irt_cpu_freq_set_min_frequency_worker(worker, freqs[length-1]);
	} else
		return retval;
	return 0;
}

/*
 * sets the frequency of a core of a worker to a specific value by setting both the min and max to this value
 */

int32 irt_cpu_freq_set_frequency_worker(irt_worker* worker, uint32 frequency) {
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

/*
 * this function blindly sets the frequency of cores belonging to a socket, whether the runtime actually has workers running on them or not
 */

bool irt_cpu_freq_set_frequency_socket(uint32 socket, uint32 frequency) {
	char path_to_cpufreq[1024] = { 0 };

	bool retval = 0;
	uint32 core_start = socket * irt_get_num_cores_per_socket();
	uint32 core_end = (socket + 1) * irt_get_num_cores_per_socket();

	// first HT unit
	for(uint32 coreid = core_start; coreid < core_end; ++coreid) {
		// write max, min, max because we don't know the old frequency and setting max below min is rejected by the OS
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_max_freq", coreid);
		retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_min_freq", coreid);
		retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_max_freq", coreid);
		retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
	}

	// second HT unit
	if(irt_get_num_threads_per_core() > 1) {
		core_start = irt_get_sibling_hyperthread(core_start);
		core_end = irt_get_sibling_hyperthread(core_end);
		for(uint32 coreid = core_start; coreid < core_end; ++coreid) {
			// write max, min, max because we don't know the old frequency and setting max below min is rejected by the OS
			sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_max_freq", coreid);
			retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
			sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_min_freq", coreid);
			retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
			sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_max_freq", coreid);
			retval |= _irt_cpu_freq_write(path_to_cpufreq, frequency);
		}
	}

	return retval;
}

/*
 * This function sets the frequencies of all cores of sockets from an environmental variable. Only actual values are supported as of now.
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
			if(irt_cpu_freq_set_frequency_socket(socketid, freq))
				irt_g_frequency_setting_specified = true;
			else
				irt_g_frequency_setting_specified = false;
			tok = strtok(NULL, ",");
			++socketid;
		}
	} else {
		irt_g_frequency_setting_specified = false;
		retval = 1;
	}

	// the rest of this function is only runtime log file output
	// TODO: we only output the first core of each socket for brevity, but this should be changed

	char path_to_cpufreq[1024] = { 0 };
	uint32 cpu_freq_list[IRT_HW_MAX_NUM_SOCKETS];

	for(uint32 i = 0; i < irt_get_num_sockets(); ++i) {
		sprintf(path_to_cpufreq, FREQ_PATH_PREFIX "scaling_cur_freq", i * irt_get_num_cores_per_socket());
		cpu_freq_list[i] = _irt_cpu_freq_read(path_to_cpufreq);
	}

	char cpu_freq_output[1024] = { 0 };
	char* cur = cpu_freq_output;
	if(irt_g_frequency_setting_specified)
		cur += sprintf(cur, "set, ");
	else
		cur += sprintf(cur, "not set, ");

	for(uint32 i = 0; i < irt_get_num_sockets(); ++i) {
		cur += sprintf(cur, "%u, ", cpu_freq_list[i]);
	}
	*(cur-2) = '\0';

	irt_log_setting_s("IRT_CPU_FREQUENCIES", cpu_freq_output);
	return retval;
}

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

int32 irt_cpu_freq_set_frequency_worker_env(irt_worker* worker) {
	char* freq_str_orig = getenv(IRT_CPU_FREQUENCIES);
	if(freq_str_orig) {
		char freq_str[strlen(freq_str_orig)]; // needed since strtok modifies the string it's working on
		strcpy(freq_str, freq_str_orig);

		int32 retval;
		uint32* freqs;
		uint32 length;

		if((retval = irt_cpu_freq_get_available_frequencies_worker(worker, &freqs, &length)) == 0) {
			char *tok = strtok(freq_str, ",");
			// copies the first entry, to be used by all workers if it was the only one supplied
			char first_copy[strlen(tok)];
			strcpy(first_copy, tok);

			// used to check if only one setting was supplied
			int i;
			// skip nonrelevant entries - the n-th tok is the setting for worker with id n
			for(i = 1; i <= worker->id.thread; ++i) {
				tok = strtok(NULL, ",");
				if(!tok)
					break;
			}

			// if i != 1 we did at least one iteration => numbers of workers and frequencies must match
			if(!tok) {
				if(i != 1) {
					IRT_ASSERT(tok != NULL, IRT_ERR_INSTRUMENTATION, "Number of workers is higher than number of cpu frequencies provided!")
				} else {
					tok = first_copy;
				}
			}

			if(strcmp(tok, "OS") == 0) {
				int32 retval = irt_cpu_freq_reset_frequency_worker(worker);
				irt_g_frequency_setting_specified = true;
				return retval;
			} else if(strcmp(tok, "MAX") == 0) {
				int32 retval = irt_cpu_freq_set_frequency_worker(worker, freqs[0]);
				irt_g_frequency_setting_specified = true;
				return retval;
			} else if(strcmp(tok, "MIN") == 0) {
				int32 retval = irt_cpu_freq_set_frequency_worker(worker, freqs[length-1]);
				irt_g_frequency_setting_specified = true;
				return retval;
			} else {

				uint32 freq = atoi(tok);
				bool available;
				for(uint32 i = 0; i < length; ++i) {
					if(freq == freqs[i]) {
						available = true;
						break;
					}
				}

				if(available) {
					int32 retval = irt_cpu_freq_set_frequency_worker(worker, freq);
					irt_g_frequency_setting_specified = true;
					return retval;
				} else {
					IRT_DEBUG("Instrumentation: Requested frequency setting %s unknown", getenv(IRT_CPU_FREQUENCIES));
					irt_g_frequency_setting_specified = false;
					return -1;
				}
			}
		} else {
			irt_g_frequency_setting_specified = false;
			return retval;
		}
	} else {
		irt_g_frequency_setting_specified = false;
		return 0;
	}
	return 0;
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

int32 irt_cpu_freq_set_frequency(uint32 frequency) {
	int32 retval = 0;
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		if((retval = irt_cpu_freq_set_frequency_worker(irt_g_workers[i], frequency)) != 0) {
			return retval;
		}
	}
	return 0;
}
