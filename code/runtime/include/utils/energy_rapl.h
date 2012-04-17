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
 * * verify equal results for all cores (since we measure the entire package)
 * * determine which core's MSR should be read (doesn't matter?)
 * * determine if write permissions to msr files are necessary
 * * unify all functions and document them
 * * add dram and other parts? (gpu? probably not)
 * * ...
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <inttypes.h>
#include <unistd.h>
#include <math.h>

#define MSR_RAPL_POWER_UNIT		0x606

/*
 * Platform specific RAPL Domains.
 * Note that PP1 RAPL Domain is supported on 062A only
 * And DRAM RAPL Domain is supported on 062D only
 */
/* Package RAPL Domain */
//#define MSR_PKG_RAPL_POWER_LIMIT	0x610
#define MSR_PKG_ENERGY_STATUS		0x611
//#define MSR_PKG_PERF_STATUS		0x613
//#define MSR_PKG_POWER_INFO		0x614
//
///* PP0 RAPL Domain */
//#define MSR_PP0_POWER_LIMIT		0x638
//#define MSR_PP0_ENERGY_STATUS		0x639
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
//#define MSR_DRAM_ENERGY_STATUS		0x619
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

/*
 * opens a file descriptor for an MSR of a given core
 */

int32 _irt_open_msr(uint32 core) {
	char path_to_msr[512];
	int32 file;

	sprintf(path_to_msr, "/dev/cpu/%u/msr", core);

	if ((file = open(path_to_msr, O_RDONLY)) < 0) {
		IRT_DEBUG("Instrumentation: Unable to open MSR file for reading, file %s, reason: %s\n", path_to_msr, strerror(errno));
		return -1;
	}

	return file;
}

/*
 * reads given subject information from an MSR
 */

int64 _irt_read_msr(int32 file, int32 subject) {
	int64 data;

	if (pread(file, &data, sizeof data, subject) != sizeof data) {
		IRT_DEBUG("Instrumentation: Unable to read MSR file %s, reason: %s\n", path_to_msr, strerror(errno));
		return -1.0;
	}

	return data;
}

/*
 * closes an MSR file descriptor
 */

uint64 _irt_close_msr(int32 file) {
	close(file);
}

/*
 * Reads the package energy consumption in joules into the provided pointer. Calling
 * this function too often (e.g. in less than 100ms intervals) might result in wrong
 * values.
 */

void irt_get_energy_consumption(double *package_energy) {
	int32 file = 0;
	int32 core = 0;
	int64 result = 0;
	double power_units = -1.0, energy_units = -1.0, time_units = -1.0;
	double package = -1.0, dram = -1.0;

	if((file = _irt_open_msr(core)) < 1) {
		*package_energy = -1.0;
		return;
	}

	if((result = _irt_read_msr(file, MSR_RAPL_POWER_UNIT)) < 0) {
		*package_energy = -1.0;
		return;
	}

	energy_units = pow(0.5, (double)((result>>8) & 0x1F));

	result = _irt_read_msr(file, MSR_PKG_ENERGY_STATUS);
	package = (double) result * energy_units;

	*package_energy = package;

	_irt_close_msr(file);
}
