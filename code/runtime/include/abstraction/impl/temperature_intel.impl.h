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

#include "abstraction/temperature_intel.h"
#include "abstraction/impl/msr.impl.h"

uint64 irt_get_temperature_intel(irt_worker* worker) {
	int32 file;
	uint64 result;

	uint64 core_temperature = 0;

	if((file = _irt_open_msr(irt_affinity_mask_get_first_cpu(worker->affinity))) >= 0) {
		if((result = _irt_read_msr(file, IA32_THERM_STATUS)&0xFFFFFFFF) >= 0) {
			// check if temperature reading is valid at all
			if(result >> 31) {
				core_temperature = TCC_ACTIVATION_TEMPERATURE - ((result >> 16)&0x3F);
				//printf("resolution: %u\n", (unsigned)(result >> 27)&0xF);
				//printf("readout absolute: %u\n", 100-(unsigned)(result >> 16)&0x3F);
				//printf("readout diff: %u\n", (unsigned)(result >> 16)&0x3F);
			}
		}
		_irt_close_msr(file);
	}
	return core_temperature;
}

bool irt_temperature_intel_is_supported() {
	volatile unsigned a, b, c, d;

	const unsigned vendor_string_ebx = 0x756E6547; // Genu
	const unsigned vendor_string_ecx = 0x6C65746E; // ineI
	const unsigned vendor_string_edx = 0x49656E69; // ntel

	__asm__ __volatile__("cpuid" : "=b" (b), "=c" (c), "=d" (d) : "a" (0x0));

	// if not an intel cpu
	if(b != vendor_string_ebx || c != vendor_string_ecx || d != vendor_string_edx)
		return false;

	__asm__ __volatile__("cpuid" : "=a" (a) : "a" (0x00000006) : "ebx", "ecx", "edx");

	// CPUID.06H.EAX[0]: digital sensors present or not
	return (a&0x1);
}
