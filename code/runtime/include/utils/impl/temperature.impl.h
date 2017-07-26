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

#pragma once
#ifndef __GUARD_UTILS_IMPL_TEMPERATURE_IMPL_H
#define __GUARD_UTILS_IMPL_TEMPERATURE_IMPL_H

#include "utils/temperature.h"

#ifdef _WIN32
#warning "Temperature measurements in Windows are not supported!"
#else
#include "utils/temperature.h"
#include "abstraction/impl/temperature_intel.impl.h"
#endif

uint64 irt_get_temperature_dummy(const irt_worker* worker) {
	return 0;
}

void irt_temperature_select_instrumentation_method() {
	if(irt_temperature_intel_core_is_supported()) {
		irt_get_temperature_core = &irt_get_temperature_intel_core;
		irt_log_setting_s("irt core temperature measurement method", "intel");
	} else {
		irt_get_temperature_core = &irt_get_temperature_dummy;
		irt_log_setting_s("irt core temperature measurement method", "none");
	}

	if(irt_temperature_intel_package_is_supported()) {
		irt_get_temperature_package = &irt_get_temperature_intel_package;
		irt_log_setting_s("irt pkg temperature measurement method", "intel");
	} else {
		irt_get_temperature_package = &irt_get_temperature_dummy;
		irt_log_setting_s("irt pkg temperature measurement method", "none");
	}
}


#endif // ifndef __GUARD_UTILS_IMPL_TEMPERATURE_IMPL_H
