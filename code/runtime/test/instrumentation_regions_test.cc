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

#include <gtest/gtest.h>
#include <string>
#include <vector>
#include <sstream>
#include <iostream>
using std::string;
using std::ostream;
using std::vector;

#define MAX_PARA 4
#define MAX_REGIONS 8

#define IRT_ENABLE_REGION_INSTRUMENTATION
//#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STATIC

#define IRT_LIBRARY_MAIN
#define IRT_LIBRARY_NO_MAIN_FUN
#include "irt_library.hxx"

void insieme_init_context_common(irt_context* context) {
	context->impl_table_size = 0;
	context->info_table_size = 0;
	context->type_table_size = 0;
}

void insieme_init_context_simple(irt_context* context) {
	insieme_init_context_common(context);
	context->num_regions = 1;
}

void insieme_init_context_nested(irt_context* context) {
	insieme_init_context_common(context);
	context->num_regions = 2;
}

void insieme_init_context_nested_multiple(irt_context* context) {
	insieme_init_context_common(context);
	context->num_regions = MAX_REGIONS;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
}

TEST(region_instrumentation, simple) {
	irt::init_in_context(MAX_PARA, insieme_init_context_simple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time");
		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);

		EXPECT_EQ(reg0->num_executions, 0);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->aggregated_cpu_time, 0);
		EXPECT_EQ(reg0->aggregated_wall_time, 0);

		uint64 start = irt_time_ticks();

		ir_inst_region_start(0);
		irt_nanosleep(1e8);
		ir_inst_region_end(0);
		uint64 end = irt_time_ticks();

		double elapsed = end - start;

		// expect to be within 3% of the expected result
		EXPECT_GT(reg0->aggregated_cpu_time / elapsed, 0.90);
		EXPECT_LT(reg0->aggregated_cpu_time / elapsed, 1.10);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->num_executions, 1);
	});
	irt::shutdown();
}

TEST(region_instrumentation, multiple_metrics) {
	irt::init_in_context(MAX_PARA, insieme_init_context_simple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,wall_time");
		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);

		EXPECT_EQ(reg0->num_executions, 0);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->aggregated_cpu_time, 0);
		EXPECT_EQ(reg0->aggregated_wall_time, 0);

		uint64 start = irt_time_ticks();
		ir_inst_region_start(0);
		irt_nanosleep(1e8);
		ir_inst_region_end(0);
		uint64 end = irt_time_ticks();

		double elapsed = end - start;


		EXPECT_GT(reg0->aggregated_cpu_time / elapsed, 0.90);
		EXPECT_LT(reg0->aggregated_cpu_time / elapsed, 1.10);
		EXPECT_GT(reg0->aggregated_wall_time / elapsed, 0.90);
		EXPECT_LT(reg0->aggregated_wall_time / elapsed, 1.10);

		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->num_executions, 1);
	});
	irt::shutdown();
}

TEST(region_instrumentation, nested) {
	irt::init_in_context(MAX_PARA, insieme_init_context_nested_multiple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,wall_time");

		uint64 start = irt_time_ticks();
		ir_inst_region_start(0);
		irt_nanosleep(1e7);
		ir_inst_region_start(1);
		irt_nanosleep(1e7);
		ir_inst_region_start(2);
		irt_nanosleep(1e7);
		ir_inst_region_end(2);
		ir_inst_region_start(3);
		ir_inst_region_start(4);
		irt_nanosleep(1e7);
		ir_inst_region_end(4);
		irt_nanosleep(1e7);
		ir_inst_region_end(3);
		ir_inst_region_end(1);
		ir_inst_region_end(0);
		uint64 end = irt_time_ticks();

		double elapsed = end - start;

		irt_inst_region_context_data* region[5];

		for(int i = 0; i < 5; ++i) {
			region[i] = &(irt_context_get_current()->inst_region_data[i]);

			EXPECT_GT(region[i]->aggregated_cpu_time, 8e6) << " for region id " << i;
			EXPECT_LT(region[i]->aggregated_cpu_time / elapsed, 1.10) << " for region id " << i;
			EXPECT_GT(region[i]->aggregated_wall_time, 8e6) << " for region id " << i;
			EXPECT_LT(region[i]->aggregated_wall_time / elapsed, 1.10) << " for region id " << i;
			EXPECT_EQ(region[i]->last_cpu_time, 0) << " for region id " << i;
			EXPECT_EQ(region[i]->last_wall_time, 0) << " for region id " << i;
			EXPECT_GT(region[i]->aggregated_cpu_time / (double)region[i]->aggregated_wall_time, 0.97) << " for region id " << i;
			EXPECT_LT(region[i]->aggregated_cpu_time / (double)region[i]->aggregated_wall_time, 1.03) << " for region id " << i;
		}

		EXPECT_GT(region[0]->aggregated_cpu_time, region[1]->aggregated_cpu_time);
		EXPECT_GT(region[1]->aggregated_cpu_time, region[2]->aggregated_cpu_time);
		EXPECT_GT(region[1]->aggregated_cpu_time, region[3]->aggregated_cpu_time);
		EXPECT_GT(region[1]->aggregated_cpu_time, region[4]->aggregated_cpu_time);
		EXPECT_GT(region[3]->aggregated_cpu_time, region[4]->aggregated_cpu_time);
	});
	irt::shutdown();
}

TEST(region_instrumentation, repeated_execution) {
	irt::init_in_context(MAX_PARA, insieme_init_context_simple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,wall_time");

		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);

		EXPECT_EQ(reg0->num_executions, 0);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->aggregated_cpu_time, 0);
		EXPECT_EQ(reg0->aggregated_wall_time, 0);

		ir_inst_region_start(0);
		ir_inst_region_end(0);

		EXPECT_EQ(reg0->num_executions, 1);
		EXPECT_GT(reg0->aggregated_cpu_time, 0);
		EXPECT_LT(reg0->aggregated_cpu_time, 1e5);
		EXPECT_GT(reg0->aggregated_wall_time, 0);
		EXPECT_LT(reg0->aggregated_wall_time, 1e5);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);

		ir_inst_region_start(0);
		ir_inst_region_end(0);

		EXPECT_EQ(reg0->num_executions, 2);
		EXPECT_GT(reg0->aggregated_cpu_time, 0);
		EXPECT_LT(reg0->aggregated_cpu_time, 1e6);
		EXPECT_GT(reg0->aggregated_wall_time, 0);
		EXPECT_LT(reg0->aggregated_wall_time, 1e6);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);

		for(uint32 i = 0; i < 1e6; ++i) {
			ir_inst_region_start(0);
			ir_inst_region_end(0);
		}

		EXPECT_GT(reg0->aggregated_cpu_time, 0);
		EXPECT_LT(reg0->aggregated_cpu_time, 1e11);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->num_executions, 1e6 + 2);
	});
	irt::shutdown();
}

TEST(region_instrumentation, parallel) {
	irt::init_in_context(MAX_PARA, insieme_init_context_nested_multiple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,wall_time");

		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);

		EXPECT_EQ(reg0->num_executions, 0);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->aggregated_cpu_time, 0);
		EXPECT_EQ(reg0->aggregated_wall_time, 0);

		auto workload = [&]() {
			ir_inst_region_start(0);
			irt_nanosleep(1e7);
			ir_inst_region_end(0);
			ir_inst_region_start(1);
			irt_nanosleep(1e7);
			ir_inst_region_end(1);
			ir_inst_region_start(2);
			irt_nanosleep(1e8);
			ir_inst_region_end(2);
		};

		irt::merge(irt::parallel(workload));

		irt_inst_region_context_data* region[3];

		for(int i = 0; i < 3; ++i) {
			region[i] = &(irt_context_get_current()->inst_region_data[i]);

			EXPECT_GT(region[i]->aggregated_cpu_time, 0) << " for region id " << i;
			EXPECT_LT(region[i]->aggregated_cpu_time, 1e11) << " for region id " << i;
			EXPECT_GT(region[i]->aggregated_wall_time, 0) << " for region id " << i;
			EXPECT_LT(region[i]->aggregated_wall_time, 1e11) << " for region id " << i;
			EXPECT_EQ(region[i]->last_cpu_time, 0) << " for region id " << i;
			EXPECT_EQ(region[i]->last_wall_time, 0) << " for region id " << i;
		}

	});
	irt::shutdown();
}

TEST(region_instrumentation, rapl) {
#ifdef DISABLE_ENERGY
	printf("Warning: Compiled with -DDISABLE_ENERGY, not testing RAPL\n");
	return;
#endif

#ifndef IRT_USE_PAPI
	printf("Warning: Compiled without PAPI, not testing RAPL\n");
	return;
#endif
	if(!irt_rapl_is_supported()) {
		printf("Warning: CPU model does not support RAPL, not testing it\n");
		return;
	}

	// check if capabilities are required (kernel versions 3.7 and olders that have the CAP_SYS_RAWIO security fix)

	struct stat info = {0};
	int32 retval = stat("/dev/cpu/0/msr", &info);
	ASSERT_EQ(retval, 0);                 // check whether file is present (and we have execute permissions on the parent directory)
	ASSERT_NE(info.st_mode & S_IROTH, 0); // check whether file has o+r permission (assuming that it is owned by root:root)

	int32 file = _irt_open_msr(0);

	// if this fails although the file was present and permissions were set correctly, we are working on a system
	// where the CAP_SYS_RAWIO capability is necessary (i.e. Linux kernels 3.7 and newer)
	ASSERT_NE(file, -1); // check if file could be opened
	_irt_close_msr(file);

	irt::init_in_context(MAX_PARA, insieme_init_context_simple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,cpu_energy,cores_energy,memory_controller_energy");

		irt_affinity_policy policy = {IRT_AFFINITY_FIXED, 0};
		uint32 workerid = 0;

		// init affinity map to 0
		for(uint32 coreid = 0; coreid < IRT_MAX_WORKERS; ++coreid) {
			policy.fixed_map[coreid] = 0;
		}

		// set affinity map to use all cores of the current socket
		for(uint32 coreid = 0; coreid < irt_hw_get_num_sockets() * irt_hw_get_num_cores_per_socket(); ++coreid) {
			policy.fixed_map[workerid++] = coreid;
			// printf("%d\n", policy.fixed_map[coreid]);
		}

		// set afinity
		irt_set_global_affinity_policy(policy);


		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);

		ir_inst_region_start(0);
		irt_nanosleep(1e8);
		ir_inst_region_end(0);

		EXPECT_GT(reg0->aggregated_cpu_energy, 0);
		EXPECT_LT(reg0->aggregated_cpu_energy, 100);
		// mc and core readings are not present on all CPUs, therefore they can be 0
		EXPECT_LT(reg0->aggregated_cores_energy, 100);
		EXPECT_LT(reg0->aggregated_memory_controller_energy, 100);
		EXPECT_EQ(reg0->last_cpu_energy, 0);
		EXPECT_EQ(reg0->last_cores_energy, 0);
		EXPECT_EQ(reg0->last_memory_controller_energy, 0);
	});
	irt::shutdown();
}

TEST(region_instrumentation, pfor) {
	irt::init_in_context(MAX_PARA, insieme_init_context_nested, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,wall_time");

		auto workload = [&]() {
			int32 sum = 0;
			ir_inst_region_start(1);
			irt::pfor_impl(0, 10000, 1, [&](uint64 i) {
				for(int j = 0; j < 10000; j++) {
					// we don't really care about the correctness of the computation, but hey, why not...
					irt_atomic_add_and_fetch(&sum, 1, int32);
				}
			});
			ir_inst_region_end(1);
		};

		uint64 start = irt_time_ticks();
		ir_inst_region_start(0);
		irt::merge(irt::parallel(workload));
		ir_inst_region_end(0);
		uint64 end = irt_time_ticks();

		double elapsed = end - start;

		for(int i = 0; i < 2; ++i) {
			irt_inst_region_context_data* region = &(irt_context_get_current()->inst_region_data[i]);

			EXPECT_GT(region->aggregated_cpu_time, 0) << " for region id " << i;
			EXPECT_LT(region->aggregated_cpu_time / (elapsed * MAX_PARA), 1.10) << " for region id " << i;
			EXPECT_GT(region->aggregated_wall_time / elapsed, 0.90) << " for region id " << i;
			EXPECT_LT(region->aggregated_wall_time / elapsed, 1.10) << " for region id " << i;
			EXPECT_EQ(region->last_cpu_time, 0) << " for region id " << i;
			EXPECT_EQ(region->last_wall_time, 0) << " for region id " << i;
			EXPECT_EQ(region->num_executions, 1) << " for region id " << i;
		}
	});
	irt::shutdown();
}

TEST(region_instrumentation, papi) {
#ifndef IRT_USE_PAPI
	printf("Warning: Compiled without PAPI, not testing RAPL\n");
	return;
#else
	irt::init_in_context(MAX_PARA, insieme_init_context_simple, insieme_cleanup_context);
	irt::run([]() {
		const char* env_string = "wall_time,PAPI_TOT_INS";
		irt_inst_region_select_metrics(env_string);
		// explicit call to papi necessary because we do not have an env var
		// set as it would be the case for normal apps running in the runtime
		irt_papi_select_events(irt_worker_get_current(), irt_context_get_current(), env_string);

		double a = 2.0;
		double b = 1.0;
		ir_inst_region_start(0);
		while(a < 1e5) {
			a = a + b;
		}
		ir_inst_region_end(0);

		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);

		EXPECT_GT(reg0->aggregated_PAPI_TOT_INS, 1e4);
		// set a high upper limit for PAPI_TOT_INS to accomodate debug builds with large overheads
		EXPECT_LT(reg0->aggregated_PAPI_TOT_INS, 1e7);
		EXPECT_EQ(reg0->last_PAPI_TOT_INS, 0);
		EXPECT_GT(reg0->aggregated_wall_time, 0);
		EXPECT_LT(reg0->aggregated_wall_time, 1e9);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->num_executions, 1);

		printf("res: %f, total instruction count: %" PRIu64 "\n", a, reg0->aggregated_PAPI_TOT_INS);
	});
	irt::shutdown();
#endif
}

TEST(region_instrumentation, all_metrics) {
	irt::init_in_context(MAX_PARA, insieme_init_context_simple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("");
		//	irt_papi_select_events(irt_context_get_current(), NULL);

		ir_inst_region_start(0);
		irt_nanosleep(1e8);
		ir_inst_region_end(0);

		irt_inst_region_context_data* reg0 = &(irt_context_get_current()->inst_region_data[0]);
		EXPECT_GT(reg0->aggregated_cpu_time, 8e7);
		EXPECT_LT(reg0->aggregated_cpu_time, 1e9);
		EXPECT_GT(reg0->aggregated_wall_time, 8e7);
		EXPECT_LT(reg0->aggregated_wall_time, 1e9);
		EXPECT_EQ(reg0->last_cpu_time, 0);
		EXPECT_EQ(reg0->last_wall_time, 0);
		EXPECT_EQ(reg0->num_executions, 1);
	});
	irt::shutdown();
}

TEST(region_instrumentation, pfor_nested) {
	irt::init_in_context(MAX_PARA, insieme_init_context_nested_multiple, insieme_cleanup_context);
	irt::run([]() {
		irt_inst_region_select_metrics("cpu_time,wall_time");

		auto workload = [&]() {
			for(int k = 0; k < 5000; ++k) {
				uint64 sum = 0;
				uint64 iterations = 100;

				ir_inst_region_start(1);
				ir_inst_region_start(2);
				irt::pfor_impl(0, 1000, 1, [&](uint64 i) {
					for(uint j = 0; j < iterations; j++) {
						sum++;
					}
				});
				ir_inst_region_end(2);
				ir_inst_region_end(1);
				irt::pfor_impl(0, 1000, 1, [&](uint64 i) {
					for(uint j = 0; j < iterations; j++) {
						sum++;
					}
				});
				ir_inst_region_start(3);
				irt::pfor_impl(0, 1000, 1, [&](uint64 i) {
					for(uint j = 0; j < iterations; j++) {
						sum++;
					}
				});
				ir_inst_region_end(3);
				ir_inst_region_start(4);
				ir_inst_region_end(4);
				ir_inst_region_start(5);
				ir_inst_region_end(5);
				ir_inst_region_start(6);
				irt::pfor_impl(0, 1000, 1, [&](uint64 i) {
					for(uint j = 0; j < iterations; j++) {
						sum++;
					}
				});
				ir_inst_region_end(6);
				ir_inst_region_start(7);
				ir_inst_region_end(7);
			}
		};

		uint64 start = irt_time_ticks();
		ir_inst_region_start(0);
		irt::merge(irt::parallel(workload));
		ir_inst_region_end(0);
		uint64 end = irt_time_ticks();

		double elapsed = end - start;

		irt_inst_region_context_data* region[MAX_REGIONS];

		for(int i = 0; i < MAX_REGIONS; ++i) {
			region[i] = &(irt_context_get_current()->inst_region_data[i]);

			EXPECT_GT(region[i]->aggregated_cpu_time, 0) << " for region id " << i;
			EXPECT_LT(region[i]->aggregated_cpu_time / (elapsed * MAX_PARA), 1.10) << " for region id " << i;
			EXPECT_GT(region[i]->aggregated_wall_time, 0) << " for region id " << i;
			//			EXPECT_LT(region[i]->aggregated_wall_time/(elapsed*IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE), 1.10) << " for region id " << i;
			EXPECT_EQ(region[i]->last_cpu_time, 0) << " for region id " << i;
			EXPECT_EQ(region[i]->last_wall_time, 0) << " for region id " << i;
		}

		EXPECT_GT(region[0]->aggregated_cpu_time, region[1]->aggregated_cpu_time);
		EXPECT_GT(region[1]->aggregated_cpu_time, region[2]->aggregated_cpu_time);
		EXPECT_GT(region[3]->aggregated_cpu_time, region[4]->aggregated_cpu_time);
		EXPECT_GT(region[3]->aggregated_cpu_time, region[5]->aggregated_cpu_time);
		EXPECT_GT(region[6]->aggregated_cpu_time, region[4]->aggregated_cpu_time);
		EXPECT_GT(region[6]->aggregated_cpu_time, region[5]->aggregated_cpu_time);

	});
	irt::shutdown();
}
