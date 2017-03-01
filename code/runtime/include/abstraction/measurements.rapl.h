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
#ifndef __GUARD_ABSTRACTION_MEAUSEREMENTS_RAPL_H
#define __GUARD_ABSTRACTION_MEAUSEREMENTS_RAPL_H

/*
 *
 * General instructions on how to do energy measurements with the runtime
 * instrumentation system:
 *
 * Currently the RAPL readings are only supported by Intel SandyBridge and
 * SandyBridge EP Processors. Any attempts to read these model specific registers
 * (MSRs) on any other architecture will fail. It is yet unclear how those readings
 * are obtained, most likely Intel uses a formula based on the cores' units'
 * utilization. This still needs to be investigated.
 *
 * In its current state, this interface reads the processor package energy consumption.
 * This includes everything regarding cores, possibly present GPU units and memory
 * controllers. It does *not* measure anything else like the energy consumption of the
 * memory itself. The value returned is the energy consumption in joules.
 *
 * To be able to measure one needs to make sure that:
 * * the msr module is loaded (i.e. "sudo modprobe msr" in ubuntu)
 * * the user has write and read permissions to /dev/cpu/[0-9]/msr
 *
 *
 * TODO:
 *
 * * Actually, no utility should depend on irt_* functionality accoring to Peter T.
 *
 * * verify equal results for all cores (since we measure the entire package)
 * * unify all functions and document them
 * * add dram and other parts? (gpu? probably not)
 * * ...
 *
 */

#include "irt_inttypes.h"

#define MSR_RAPL_POWER_UNIT 0x606

/*
 * Platform specific RAPL Domains.
 * Note that PP1 RAPL Domain is supported on 062A only
 * And DRAM RAPL Domain is supported on 062D only
 */
/* Package RAPL Domain */
//#define MSR_PKG_RAPL_POWER_LIMIT	0x610
#define MSR_PKG_ENERGY_STATUS 0x611
//#define MSR_PKG_PERF_STATUS		0x613
//#define MSR_PKG_POWER_INFO		0x614
//
///* PP0 RAPL Domain */
//#define MSR_PP0_POWER_LIMIT		0x638
#define MSR_PP0_ENERGY_STATUS 0x639
//#define MSR_PP0_POLICY			0x63A
//#define MSR_PP0_PERF_STATUS		0x63B
//
///* PP1 RAPL Domain, may reflect to uncore devices */
//#define MSR_PP1_POWER_LIMIT		0x640
//#define MSR_PP1_ENERGY_STATUS		0x641
//#define MSR_PP1_POLICY			0x642
//
///* DRAM RAPL Domain */
//#define MSR_DRAM_POWER_LIMIT		0x618
#define MSR_DRAM_ENERGY_STATUS 0x619
//#define MSR_DRAM_PERF_STATUS		0x61B
//#define MSR_DRAM_POWER_INFO		0x61C
//
///* RAPL UNIT BITMASK */
//#define POWER_UNIT_OFFSET	0
//#define POWER_UNIT_MASK		0x0F
//
//#define ENERGY_UNIT_OFFSET	0x08
//#define ENERGY_UNIT_MASK	0x1F00
//
//#define TIME_UNIT_OFFSET	0x10
//#define TIME_UNIT_MASK		0xF000

#define IRT_RAPL_REGISTER_READ_INTERVAL 32786

/*
 * struct to hold RAPL data for all sockets/CPUs
 */

typedef struct _rapl_energy_data {
	double package;
	double mc;
	double cores;
} rapl_energy_data;

/*
 * Reads the package energy consumption in joules into the provided pointer. Calling
 * this function too often (e.g. in less than 100ms intervals) might result in wrong
 * values.
 */

void _irt_get_rapl_energy_consumption(rapl_energy_data* data);

/*
 * checks if RAPL is supported - currently, only Sandy Bridge (EX) and Ivy Bridge processors are known
 * to support it, so we simply check for the processor type
 */

bool irt_rapl_is_supported();

/*
 * checks if RAPL is in use
 */

bool irt_rapl_is_used();

/*
 * initialize RAPL support (allocate spinlock, register maintenance thread)
 */

void irt_rapl_init();

/*
 * cleanup function, destroys spinlock
 */

void irt_rapl_finalize();


// pointer to the function that is used to obtain energy readings
void (*irt_get_energy_consumption)(rapl_energy_data* data);

/*
 * a dummy method if no energy instrumentation is available
 */

void irt_get_energy_consumption_dummy(rapl_energy_data* data);

/*
 * selects the method used to obtain energy readings
 */

void irt_energy_select_instrumentation_method();

#endif // ifndef __GUARD_ABSTRACTION_MEAUSEREMENTS_RAPL_H
