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
#ifndef __GUARD_HWINFO_H
#define __GUARD_HWINFO_H

#ifndef _GEMS_SIM
#include <unistd.h>
#include <string.h>
#endif

#ifdef IRT_USE_PAPI
#include "papi_helper.h"
#endif
#include "error_handling.h"
#include "utils/frequency.h"

typedef struct _irt_hw_cpuid_info {
	uint32 family;
	uint32 model;
	uint32 stepping;
} irt_hw_cpuid_info;

typedef struct _irt_hw_info {
	irt_hw_cpuid_info cpuid;
	uint32 cpus;
	uint32 threads_per_core;
	uint32 cores_per_socket;
	uint32 sockets;
	uint32 numa_nodes;
	uint32 cpu_max_mhz;
	uint32 cpu_min_mhz;
	char vendor_string[IRT_HW_MAX_STRING_LENGTH];
	char model_string[IRT_HW_MAX_STRING_LENGTH];
	uint32 memory_levels;
} irt_hw_info;

static irt_hw_info __irt_g_cached_hw_info;

// static uint32 __irt_g_cached_cpu_count = 0;
// static uint32 __irt_g_cached_threads_per_core_count = 0;
// static uint32 __irt_g_cached_cores_per_socket_count = 0;
// static uint32 __irt_g_cached_sockets_count = 0;
// static uint32 __irt_g_cached_numa_nodes_count = 0;

uint32 irt_hw_get_num_cpus() {
	if(__irt_g_cached_hw_info.cpus != 0) { return __irt_g_cached_hw_info.cpus; }
	uint32 ret = 1;
	#ifdef _SC_NPROCESSORS_ONLN
	// Linux
	ret = sysconf(_SC_NPROCESSORS_ONLN);
	#elif defined(_WIN32)
	// Windows
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	ret = sysinfo.dwNumberOfProcessors;
	#elif defined(_SC_NPROC_ONLN)
	// Irix
	ret = sysconf(_SC_NPROC_ONLN);
	#elif defined(MPC_GETNUMSPUS)
	// HPUX
	ret = mpctl(MPC_GETNUMSPUS, NULL, NULL);
	#endif
	if(ret < 1) { ret = 1; }
	__irt_g_cached_hw_info.cpus = ret;
	return ret;
}

// to be used only for testing
void _irt_hw_set_num_cpus(uint32 num) {
	__irt_g_cached_hw_info.cpus = num;
}

void _irt_hw_info_shutdown() {
#ifdef IRT_USE_PAPI
	irt_papi_shutdown();
	#endif
}

int32 _irt_hw_info_init() {
#ifdef IRT_USE_PAPI
	irt_papi_init();

	const PAPI_hw_info_t* hwinfo = PAPI_get_hardware_info();

	if(hwinfo == NULL) {
		IRT_WARN("hwinfo: Error trying to get hardware information from PAPI!\n");
		return -1;
	}

	if(hwinfo->threads > 0) { __irt_g_cached_hw_info.threads_per_core = hwinfo->threads; }
	if(hwinfo->cores > 0) { __irt_g_cached_hw_info.cores_per_socket = hwinfo->cores; }
	if(hwinfo->sockets > 0) { __irt_g_cached_hw_info.sockets = hwinfo->sockets; }
	if(hwinfo->nnodes > 0) { __irt_g_cached_hw_info.numa_nodes = hwinfo->nnodes; }
	if(hwinfo->cpu_max_mhz > 0) { __irt_g_cached_hw_info.cpu_max_mhz = hwinfo->cpu_max_mhz; }
	if(hwinfo->cpu_min_mhz > 0) { __irt_g_cached_hw_info.cpu_min_mhz = hwinfo->cpu_min_mhz; }

	__irt_g_cached_hw_info.cpuid.family = hwinfo->cpuid_family;
	__irt_g_cached_hw_info.cpuid.model = hwinfo->cpuid_model;
	__irt_g_cached_hw_info.cpuid.stepping = hwinfo->cpuid_stepping;

	strncpy(__irt_g_cached_hw_info.model_string, hwinfo->model_string, IRT_HW_MAX_STRING_LENGTH);
	strncpy(__irt_g_cached_hw_info.vendor_string, hwinfo->vendor_string, IRT_HW_MAX_STRING_LENGTH);

	#else
	IRT_DEBUG("hwinfo: papi not available, reporting dummy values")
	#endif

	return 0;
}

uint32 irt_hw_get_num_threads_per_core() {
	if(__irt_g_cached_hw_info.threads_per_core == 0) { _irt_hw_info_init(); }

	IRT_ASSERT(__irt_g_cached_hw_info.threads_per_core != 0, IRT_ERR_HW_INFO, "Hardware information only available when runtime compiled with PAPI!")

	return __irt_g_cached_hw_info.threads_per_core;
}

uint32 irt_hw_get_num_cores_per_socket() {
	if(__irt_g_cached_hw_info.cores_per_socket == 0) { _irt_hw_info_init(); }

	IRT_ASSERT(__irt_g_cached_hw_info.cores_per_socket != 0, IRT_ERR_HW_INFO, "Hardware information only available when runtime compiled with PAPI!")

	return __irt_g_cached_hw_info.cores_per_socket;
}

uint32 irt_hw_get_num_sockets() {
	if(__irt_g_cached_hw_info.sockets == 0) { _irt_hw_info_init(); }

	IRT_ASSERT(__irt_g_cached_hw_info.sockets != 0, IRT_ERR_HW_INFO, "Hardware information only available when runtime compiled with PAPI!")

	return __irt_g_cached_hw_info.sockets;
}

uint32 irt_hw_get_num_numa_nodes() {
	if(__irt_g_cached_hw_info.numa_nodes == 0) { _irt_hw_info_init(); }

	IRT_ASSERT(__irt_g_cached_hw_info.numa_nodes != 0, IRT_ERR_HW_INFO, "Hardware information only available when runtime compiled with PAPI!")

	return __irt_g_cached_hw_info.numa_nodes;
}

uint32 irt_hw_get_sibling_hyperthread(uint32 coreid) {
	// should work for Intel HyperThreading and sanely set up Linux systems
	uint32 num_threads_per_core = irt_hw_get_num_threads_per_core();
	if(num_threads_per_core > 1) {
		uint32 cores_total = irt_hw_get_num_sockets() * irt_hw_get_num_cores_per_socket();
		return (coreid + cores_total) % (cores_total * num_threads_per_core);
	} else {
		return coreid;
	}
}

uint32 irt_hw_get_cpu_max_mhz() {
	if(__irt_g_cached_hw_info.cpu_max_mhz == 0) { _irt_hw_info_init(); }

	IRT_ASSERT(__irt_g_cached_hw_info.cpu_max_mhz != 0, IRT_ERR_HW_INFO, "Hardware information only available when runtime compiled with PAPI!")

	return __irt_g_cached_hw_info.cpu_max_mhz;
}

uint32 irt_hw_get_cpu_min_mhz() {
	if(__irt_g_cached_hw_info.cpu_min_mhz == 0) { _irt_hw_info_init(); }

	IRT_ASSERT(__irt_g_cached_hw_info.cpu_min_mhz != 0, IRT_ERR_HW_INFO, "Hardware information only available when runtime compiled with PAPI!")

	return __irt_g_cached_hw_info.cpu_min_mhz;
}

bool irt_hw_get_hyperthreading_enabled() {
	return (irt_hw_get_num_threads_per_core() > 1);
}

irt_hw_cpuid_info irt_hw_get_cpuid_info() {
	if(__irt_g_cached_hw_info.cpuid.family == 0) { _irt_hw_info_init(); }
	return __irt_g_cached_hw_info.cpuid;
}

char* irt_hw_get_vendor_string() {
	if(strlen(__irt_g_cached_hw_info.vendor_string) == 0) { _irt_hw_info_init(); }
	return __irt_g_cached_hw_info.vendor_string;
}

char* irt_hw_get_model_string() {
	if(strlen(__irt_g_cached_hw_info.model_string) == 0) { _irt_hw_info_init(); }
	return __irt_g_cached_hw_info.model_string;
}

void irt_hw_dump_info(FILE* fd) {
	_irt_hw_info_init();
	fprintf(fd, "--------\nIRT hardware dump:\n");
	fprintf(fd, "  Number of CPUs: %u\n", irt_hw_get_num_cpus());
	#ifdef IRT_USE_PAPI
	fprintf(fd, "  System type:\n");
	fprintf(fd, "    CPU vendor: %s\n", __irt_g_cached_hw_info.vendor_string);
	fprintf(fd, "    CPU model: %s\n", __irt_g_cached_hw_info.model_string);
	fprintf(fd, "    CPU cpuid: family: %u, model: %u, stepping: %u\n", irt_hw_get_cpuid_info().family, irt_hw_get_cpuid_info().model,
	        irt_hw_get_cpuid_info().stepping);
	fprintf(fd, "  CPU hierarchy:\n");
	fprintf(fd, "    Number of numa nodes:\t\t%4u\n", irt_hw_get_num_numa_nodes());
	fprintf(fd, "    Number of sockets: \t\t\t%4u\n", irt_hw_get_num_sockets());
	fprintf(fd, "    Number of cores per socket: \t%4u\n", irt_hw_get_num_cores_per_socket());
	fprintf(fd, "    Number of HW threads per core: \t%4u\n", irt_hw_get_num_threads_per_core());

	const PAPI_hw_info_t* hwinfo = PAPI_get_hardware_info();

	fprintf(fd, "  Cache hierarchy: %u levels\n", hwinfo->mem_hierarchy.levels);
	for(int32 i = 0; i < hwinfo->mem_hierarchy.levels; ++i) {
		uint32 number_of_memories = 0;
		for(uint32 j = 0; j < PAPI_MH_MAX_LEVELS; ++j)
			if(hwinfo->mem_hierarchy.level[i].cache[j].type != PAPI_MH_TYPE_EMPTY) { number_of_memories++; }
		fprintf(fd, "    Level %u: number of entities: %u \n", i, number_of_memories);

		for(uint32 j = 0; j < PAPI_MH_MAX_LEVELS; ++j) {
			if(hwinfo->mem_hierarchy.level[i].cache[j].type == PAPI_MH_TYPE_EMPTY) { continue; }
			fprintf(fd, "      Type:");
			if(hwinfo->mem_hierarchy.level[i].cache[j].type == PAPI_MH_TYPE_INST) {
				fprintf(fd, "%16s", "instruction");
			} else if(hwinfo->mem_hierarchy.level[i].cache[j].type == PAPI_MH_TYPE_DATA) {
				fprintf(fd, "%16s", "data");
			} else if(hwinfo->mem_hierarchy.level[i].cache[j].type == PAPI_MH_TYPE_UNIFIED) {
				fprintf(fd, "%16s", "unified");
			} else {
				fprintf(fd, "%16s", "unknown");
			}
			fprintf(fd, ", size: %10u KB, ", hwinfo->mem_hierarchy.level[i].cache[j].size);
			fprintf(fd, "(line size: %4u B, ", hwinfo->mem_hierarchy.level[i].cache[j].line_size);
			fprintf(fd, "lines: %8u, ", hwinfo->mem_hierarchy.level[i].cache[j].num_lines);
			fprintf(fd, "associativity: %4u)\n", hwinfo->mem_hierarchy.level[i].cache[j].associativity);
		}
	}
	#else
	#endif
	fprintf(fd, "  Miscellaneous:\n");
	fprintf(fd, "    CPU DVFS domain:\t\t\t%s\n", irt_cpu_freq_cores_have_individual_domains() ? "cores" : "sockets");
	fprintf(fd, "    CPU DVFS range: min: %u MHz, max: %u MHz\n", irt_hw_get_cpu_min_mhz(), irt_hw_get_cpu_max_mhz());
	#ifndef IRT_USE_PAPI
	fprintf(fd, "(Note: Compile with -DIRT_USE_PAPI to get more detailed hardware information)\n");
	#endif
	fprintf(fd, "--------\n");
}

void irt_hw_print_info() {
	irt_hw_dump_info(stdout);
}


#endif // ifndef __GUARD_HWINFO_H
