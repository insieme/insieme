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
#ifndef __GUARD_ABSTRACTION_IMPL_TEMPERATURE_INTEL_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_TEMPERATURE_INTEL_IMPL_H

#include "abstraction/temperature_intel.h"
#include "abstraction/impl/msr.impl.h"

uint64 irt_get_temperature_intel_core(const irt_worker* worker) {
	int32 file;
	uint64 result;

	uint64 core_temperature = 0;

	if((file = _irt_open_msr(irt_affinity_mask_get_first_cpu(worker->affinity))) >= 0) {
		if((result = _irt_read_msr(file, IA32_THERM_STATUS) & 0xFFFFFFFF) >= 0) {
			// check if temperature reading is valid at all
			if(result >> 31) {
				core_temperature = TCC_ACTIVATION_TEMPERATURE - ((result >> 16) & 0x3F);
				// printf("resolution: %u\n", (unsigned)(result >> 27)&0xF);
				// printf("readout absolute: %u\n", 100-(unsigned)(result >> 16)&0x3F);
				// printf("readout diff: %u\n", (unsigned)(result >> 16)&0x3F);
			}
		}
		_irt_close_msr(file);
	}
	return core_temperature;
}

uint64 irt_get_temperature_intel_package(const irt_worker* worker) {
	int32 file;
	uint64 result;

	uint64 package_temperature = 0;

	if((file = _irt_open_msr(irt_affinity_mask_get_first_cpu(worker->affinity))) >= 0) {
		if((result = _irt_read_msr(file, IA32_PACKAGE_THERM_STATUS) & 0xFFFFFFFF) >= 0) {
			// check if temperature reading is valid at all
			if(result >> 31) {
				package_temperature = TCC_ACTIVATION_TEMPERATURE - ((result >> 16) & 0x3F);
				// printf("resolution: %u\n", (unsigned)(result >> 27)&0xF);
				// printf("readout absolute: %u\n", 100-(unsigned)(result >> 16)&0x3F);
				// printf("readout diff: %u\n", (unsigned)(result >> 16)&0x3F);
			}
		}
		_irt_close_msr(file);
	}
	return package_temperature;
}

bool irt_temperature_intel_core_is_supported() {
	volatile unsigned a, b, c, d;

	const unsigned vendor_string_ebx = 0x756E6547; // Genu
	const unsigned vendor_string_ecx = 0x6C65746E; // ineI
	const unsigned vendor_string_edx = 0x49656E69; // ntel

	__asm__ __volatile__("cpuid" : "=b"(b), "=c"(c), "=d"(d) : "a"(0x0));

	// if not an intel cpu
	if(b != vendor_string_ebx || c != vendor_string_ecx || d != vendor_string_edx) { return false; }

	__asm__ __volatile__("cpuid" : "=a"(a) : "a"(0x00000006) : "ebx", "ecx", "edx");

	// CPUID.06H.EAX[0]: digital sensors present or not
	return (a & 0x1);
}

bool irt_temperature_intel_package_is_supported() {
	volatile unsigned a, b, c, d;

	const unsigned vendor_string_ebx = 0x756E6547; // Genu
	const unsigned vendor_string_ecx = 0x6C65746E; // ineI
	const unsigned vendor_string_edx = 0x49656E69; // ntel

	__asm__ __volatile__("cpuid" : "=b"(b), "=c"(c), "=d"(d) : "a"(0x0));

	// if not an intel cpu
	if(b != vendor_string_ebx || c != vendor_string_ecx || d != vendor_string_edx) { return false; }

	__asm__ __volatile__("cpuid" : "=a"(a) : "a"(0x00000006) : "ebx", "ecx", "edx");

	// CPUID.06H.EAX[6]: digital sensors present or not
	return ((a >> 6) & 0x1);
}


#endif // ifndef __GUARD_ABSTRACTION_IMPL_TEMPERATURE_INTEL_IMPL_H
