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
#ifndef __GUARD_ABSTRACTION_IMPL_RAPL_STD_H
#define __GUARD_ABSTRACTION_IMPL_RAPL_STD_H

#include <math.h>
#include "irt_inttypes.h"
#include "abstraction/msr.h"
#include "hwinfo.h"
#include "utils/affinity.h"
#include "worker.h"
#include "abstraction/rapl.h"
#include "abstraction/impl/msr.impl.h"

#ifdef _MSC_VER
	#include <io.h>
#else
	#include <unistd.h>
#endif

void _irt_get_rapl_energy_consumption(rapl_energy_data* data) {
	int32 file = 0;
	uint64 result = 0;
	double energy_units = -1.0;

	data->package = 0.0;
	data->cores = 0.0;
	data->mc = 0.0;

	// mark sockets that should be measured (i.e. that have cores which have workers running on them)
	uint32 num_sockets = irt_get_num_sockets();
	bool socket_mask[num_sockets];

	for(uint32 i = 0; i < num_sockets; ++i)
		socket_mask[i] = false;

	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		uint32 coreid = irt_affinity_mask_get_first_cpu(irt_g_workers[i]->affinity);
		if(coreid != -1)
			socket_mask[coreid / irt_get_num_cores_per_socket()] = true;
	}

	// do the actual measurement, sum over all sockets
	for(uint32 socket_id = 0; socket_id < num_sockets; ++socket_id) {
		if(socket_mask[socket_id]) {
			if((file = _irt_open_msr(socket_id * irt_get_num_cores_per_socket())) > 0) {
				if((result = _irt_read_msr(file, MSR_RAPL_POWER_UNIT)) >= 0) {
					energy_units = pow(0.5, (double) ((result >> 8) & 0x1F));
					if ((result = _irt_read_msr(file, MSR_PKG_ENERGY_STATUS) & 0xFFFFFFFF) >= 0)
						data->package += (double) (result & 0xFFFFFFFF) * energy_units;
					if ((result = _irt_read_msr(file, MSR_DRAM_ENERGY_STATUS) & 0xFFFFFFFF) >= 0)
						data->mc += (double) (result & 0xFFFFFFFF) * energy_units;
					if ((result = _irt_read_msr(file, MSR_PP0_ENERGY_STATUS) & 0xFFFFFFFF) >= 0)
						data->cores += (double) (result & 0xFFFFFFFF) * energy_units;
				}
				_irt_close_msr(file);
			}
		}
	}
}

bool irt_rapl_is_supported() {
	volatile unsigned a, b, c, d;

	const unsigned vendor_string_ebx = 0x756E6547; // Genu
	const unsigned vendor_string_ecx = 0x6C65746E; // ineI
	const unsigned vendor_string_edx = 0x49656E69; // ntel

	__asm__ __volatile__("cpuid" : "=b" (b), "=c" (c), "=d" (d) : "a" (0x0));

	// if not an intel cpu
	if(b != vendor_string_ebx || c != vendor_string_ecx || d != vendor_string_edx)
		return false;

	__asm__ __volatile__("cpuid" : "=a" (a) : "a" (0x00000001) : "ebx", "ecx", "edx");

	const unsigned model_number = (a>>4)&0xF; // bits 4-7
	const unsigned family_code = (a>>8)&0xF; // bits 8-11
	const unsigned extended_model = (a>>16)&0xF; // bits 16-19

	if(family_code == 0x6) {
		if(model_number == 0xA && extended_model == 0x2) // SandyBridge 32nm
			return true;
		if(model_number == 0xE && extended_model == 0x2) // SandyBridge EN 32nm
			return true;
		if(model_number == 0xE && extended_model == 0x3) // IvyBridge EN 22nm
			return true;
		if(model_number == 0xD && extended_model == 0x2) // dx1 = E5-2660
			return true;
		if(model_number == 0xA && extended_model == 0x3) // IvyBridge 22nm
			return true;
	}

	return false;
}


#endif // ifndef __GUARD_ABSTRACTION_IMPL_RAPL_STD_H
