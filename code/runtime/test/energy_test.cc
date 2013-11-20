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

#define IRT_ENABLE_INDIVIDUAL_REGION_INSTRUMENTATION
#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STATIC
#define IRT_RUNTIME_TUNING

#include <gtest/gtest.h>
#include "standalone.h"

// type table

irt_type g_insieme_type_table[] = {
	{ IRT_T_INT64, 8, 0, 0 },
};

// work item table

void insieme_wi_startup_implementation_dvfs(irt_work_item* wi);
void insieme_wi_startup_implementation_rapl(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants_dvfs[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation_dvfs, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_startup_variants_rapl[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation_rapl, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants_dvfs },
	{ 1, g_insieme_wi_startup_variants_rapl }
};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 1;
	context->impl_table_size = 2;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
}

// work item function definitions

void insieme_wi_startup_implementation_dvfs(irt_work_item* wi) {
	uint32 sockets = irt_get_num_sockets();
	uint32 length;
	uint32 frequencies[IRT_INST_MAX_CPU_FREQUENCIES];

	// assumes that all cores support the same frequencies (which they do, in a sane universe)
	irt_cpu_freq_get_available_frequencies_core(0, frequencies, &length);

	// try each frequency once
	for(uint32 freq_id = 0; freq_id < length; ++freq_id) {
		uint32 all_core_frequencies_min[irt_get_num_sockets() * irt_get_num_cores_per_socket()];
		uint32 all_core_frequencies_max[irt_get_num_sockets() * irt_get_num_cores_per_socket()];
		uint32 frequency = frequencies[freq_id];
		// try each socket once
		for(uint32 socket_id = 0; socket_id < sockets; ++socket_id) {
			// save the current frequency settings
			for(uint32 core_id = 0; core_id < irt_get_num_sockets() * irt_get_num_cores_per_socket(); ++core_id) {
				all_core_frequencies_min[core_id] = _irt_cpu_freq_get(core_id, "scaling_min_freq");
				all_core_frequencies_max[core_id] = _irt_cpu_freq_get(core_id, "scaling_max_freq");
			}
			// set the frequency of the current socket
			irt_cpu_freq_set_frequency_socket(socket_id, frequency);
			// check all cores, the ones on this socket must be set correctly, the rest must remain unchanged
			for(uint32 core_id = 0; core_id < irt_get_num_sockets() * irt_get_num_cores_per_socket(); ++core_id) {
				if(core_id >= socket_id * irt_get_num_cores_per_socket() && core_id < (socket_id+1)*irt_get_num_cores_per_socket()) {
					EXPECT_EQ(_irt_cpu_freq_get(core_id, "scaling_cur_freq"), frequency);
					if(irt_get_hyperthreading_enabled())
						EXPECT_EQ(_irt_cpu_freq_get(irt_get_sibling_hyperthread(core_id), "scaling_cur_freq"), frequency);
				} else {
					EXPECT_EQ(_irt_cpu_freq_get(core_id, "scaling_cur_freq"), all_core_frequencies_min[core_id]);
					EXPECT_EQ(_irt_cpu_freq_get(core_id, "scaling_cur_freq"), all_core_frequencies_max[core_id]);
					if(irt_get_hyperthreading_enabled()) {
						uint32 sibling = irt_get_sibling_hyperthread(core_id);
						EXPECT_EQ(_irt_cpu_freq_get(sibling, "scaling_cur_freq"), all_core_frequencies_min[sibling]);
						EXPECT_EQ(_irt_cpu_freq_get(sibling, "scaling_cur_freq"), all_core_frequencies_max[sibling]);
					}
				}
			}
			// reset all frequencies to system default again
			irt_cpu_freq_reset_frequencies();
			// check that all frequencies have been reset
			for(uint32 core_id = 0; core_id < irt_get_num_sockets() * irt_get_num_cores_per_socket(); ++core_id) {
				EXPECT_EQ(_irt_cpu_freq_get(core_id, "scaling_min_freq"), _irt_cpu_freq_get(core_id, "cpuinfo_min_freq"));
				EXPECT_EQ(_irt_cpu_freq_get(core_id, "scaling_max_freq"), _irt_cpu_freq_get(core_id, "cpuinfo_max_freq"));
			}
		}
	}
}

void insieme_wi_startup_implementation_rapl(irt_work_item* wi) {
	rapl_energy_data data;
	data.number_of_cpus = irt_get_num_sockets();
	double package[data.number_of_cpus];
	double mc[data.number_of_cpus];
	double cores[data.number_of_cpus];
	data.package = package;
	data.mc = mc;
	data.cores = cores;

	rapl_energy_data data2;
	data2.number_of_cpus = irt_get_num_sockets();
	double package2[data2.number_of_cpus];
	double mc2[data2.number_of_cpus];
	double cores2[data2.number_of_cpus];
	data2.package = package2;
	data2.mc = mc2;
	data2.cores = cores2;
	
	// take a reading, sleep for 100 ms and take another reading
	irt_get_energy_consumption(&data);
	irt_nanosleep(1e8);
	irt_get_energy_consumption(&data2);

	for(uint32 i = 0; i < data.number_of_cpus; ++i) {
		EXPECT_LT(0.1, data2.package[i] - data.package[i]);
		EXPECT_LT(data2.package[i] - data.package[i], 100);
		EXPECT_LT(0.1, data2.cores[i] - data.cores[i]);
		EXPECT_LT(data2.cores[i] - data.cores[i], 100);
		EXPECT_LT(data2.mc[i] - data.mc[i], 100); // mc readings are not present on all CPUs, therefore they can be 0
		//printf("socket %u package: %f J, cores: %f J, memory controller: %f J\n", i, data2.package[i] - data.package[i], data2.cores[i] - data.cores[i], data2.mc[i] - data.mc[i]);
	}
}

TEST(energy, dvfs) {
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, 0, NULL);
}

TEST(energy, rapl) {
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, 1, NULL);
}

