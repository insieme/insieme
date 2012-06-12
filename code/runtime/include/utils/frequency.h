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
#include <papi.h>
#include "affinity.h"

/*
 * These functions provide an interface to read and set cpu frequency settings
 */


/*
 * reads all available frequencies for a worker running on a specific core as a list into the provided pointer
 */

int irt_cpu_freq_get_available_frequencies_core(irt_worker* worker, unsigned int** frequencies, unsigned int* length) {

	char path_to_cpufreq[1024] = { 0 };
	unsigned int counter = 1;
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);

	unsigned int frequencies_temp[IRT_INST_MAX_CPU_FREQUENCIES] = { 0 };

	sprintf(path_to_cpufreq, "/sys/devices/system/cpu/cpu%u/cpufreq/scaling_available_frequencies", coreid);

	FILE* file = fopen(path_to_cpufreq, "r");

	if(file == NULL) {
		IRT_DEBUG("Instrumentation: Unable to open frequency file for reading for worker %lu, file %s, reason: %s\n", worker->id.value.full, path_to_cpufreq, strerror(errno));
		return -2;
	}

	if(fscanf(file, "%u", &frequencies_temp[0]) != 1) {
		IRT_DEBUG("Instrumentation: Unable to read available frequencies for worker %lu, file %s, reason: %s\n", worker->id.value.full, path_to_cpufreq, strerror(errno));
		fclose(file);
		return -1;
	}

	while(fscanf(file, " %u", &frequencies_temp[counter]) == 1) ++counter;

	*length = counter;
	(*frequencies) = (unsigned int*)malloc(counter*sizeof(unsigned int));

	for(unsigned int j = 0; j < counter; ++j)
		(*frequencies)[j] = frequencies_temp[j];

	fclose(file);

	return 0;
}

/*
 * low-level function that writes the frequency for a given /sys file path
 */

int _irt_cpu_freq_write(const char* path_to_cpufreq, const unsigned int frequency) {
	int retval = 0;
	FILE* file = fopen(path_to_cpufreq, "w");

	if(file == NULL) {
		IRT_DEBUG("Instrumentation: Unable to open frequency file for worker %lu, file %s, reason: %s\n", worker->id.value.full, path_to_cpufreq, strerror(errno));
		return -2;
	}

	if((retval = fprintf(file, "%u\n", frequency) < 1)) {
		IRT_DEBUG("Instrumentation: Unable to write frequency for worker %lu, file %s, reason: %s\n", worker->id.value.full, path_to_cpufreq, strerror(errno));
		fclose(file);
		return -1;
	}

	fclose(file);

	return 0;
}

/*
 * low-level function that reads the frequency for a given /sys file path
 */

int _irt_cpu_freq_read(const char* path_to_cpufreq) {

	unsigned int temp = 0;
	int retval = 0;

	FILE* file = fopen(path_to_cpufreq, "r");

	if(file == NULL) {
		IRT_DEBUG("Instrumentation: Unable to open frequency file for writing for worker %lu, file %s, reason: %s\n", worker->id.value.full, path_to_cpufreq, strerror(errno));
		return -2;
	}

	if((retval = fscanf(file, "%u", &temp)) < 1) {
		IRT_DEBUG("Instrumentation: Unable to read frequency for worker %lu, file %s, reason: %s\n", worker->id.value.full, path_to_cpufreq, strerror(errno));
		fclose(file);
		return -1;
	}

	fclose(file);

	return temp;
}

/*
 * gets the current frequency a core of a worker is running on
 */

int irt_cpu_freq_get_cur_frequency_core(irt_worker* worker) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, "/sys/devices/system/cpu/cpu%u/cpufreq/scaling_cur_freq", coreid);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * sets the maximum frequency a core of a worker is allowed to run on
 */

int irt_cpu_freq_set_max_frequency_core(irt_worker* worker, unsigned int frequency) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, "/sys/devices/system/cpu/cpu%u/cpufreq/scaling_max_freq", coreid);
	return _irt_cpu_freq_write(path_to_cpufreq, frequency);
}

/*
 * gets the maximum frequency a core of a worker is allowed to run on
 */

int irt_cpu_freq_get_max_frequency_core(irt_worker* worker) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, "/sys/devices/system/cpu/cpu%u/cpufreq/scaling_max_freq", coreid);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * sets the minimum frequency a core of a worker is allowed to run on
 */

int irt_cpu_freq_set_min_frequency_core(irt_worker* worker, unsigned int frequency) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, "/sys/devices/system/cpu/cpu%u/cpufreq/scaling_min_freq", coreid);
	return _irt_cpu_freq_write(path_to_cpufreq, frequency);
}

/*
 * gets the minimum frequency a core of a worker is allowed to run on
 */

int irt_cpu_freq_get_min_frequency_core(irt_worker* worker) {
	char path_to_cpufreq[1024] = { 0 };
	uint32 coreid = irt_affinity_mask_get_first_cpu(worker->affinity);
	sprintf(path_to_cpufreq, "/sys/devices/system/cpu/cpu%u/cpufreq/scaling_min_freq", coreid);
	return _irt_cpu_freq_read(path_to_cpufreq);
}

/*
 * sets the frequency of a core of a worker to a specific value by setting both the min and max to this value
 */

int irt_cpu_freq_set_frequency_core(irt_worker* worker, unsigned int frequency) {
	int old_min_freq = 0;
	char path_to_cpufreq[1024] = { 0 };

	old_min_freq = irt_cpu_freq_get_min_frequency_core(worker);

	// min always has to be >= max, so if old_min is larger than max, first set new min
	if(old_min_freq > frequency) {
		irt_cpu_freq_set_min_frequency_core(worker, frequency);
		irt_cpu_freq_set_max_frequency_core(worker, frequency);
	} else {
		irt_cpu_freq_set_max_frequency_core(worker, frequency);
		irt_cpu_freq_set_min_frequency_core(worker, frequency);
	}

	return 0;
}

/*
 * prints the current frequency of all cores of all workers to stdout
 */

int irt_cpu_freq_print_cur_frequency() {
	int retval = 0;

	for(unsigned int i = 0; i < irt_g_worker_count; ++i) {
		if((retval = irt_cpu_freq_get_cur_frequency_core(irt_g_workers[i])) > 0) {
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

int irt_cpu_freq_set_frequency(unsigned int frequency) {
	int retval = 0;
	for(unsigned int i = 0; i < irt_g_worker_count; ++i) {
		if((retval = irt_cpu_freq_set_frequency_core(irt_g_workers[i], frequency)) != 0) {
			return retval;
		}
	}
	return 0;
}

/*
 * resets all the min and max frequencies of all cores of all workers to the available min and max
 */

int irt_cpu_freq_reset_frequency() {
	int retval = 0;
	unsigned int* freqs;
	unsigned int length;
	for(unsigned int i = 0; i < irt_g_worker_count; ++i) {
		if((retval = irt_cpu_freq_get_available_frequencies_core(irt_g_workers[i], &freqs, &length)) == 0) {
			irt_cpu_freq_set_max_frequency_core(irt_g_workers[i], freqs[0]);
			irt_cpu_freq_set_min_frequency_core(irt_g_workers[i], freqs[length-1]);
		} else
			return retval;
	}
	return 0;
}
