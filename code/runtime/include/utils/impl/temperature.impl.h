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
	}
	else {
		irt_get_temperature_core = &irt_get_temperature_dummy;
		irt_log_setting_s("irt core temperature measurement method", "none");
	}
	
	if(irt_temperature_intel_package_is_supported()) {
		irt_get_temperature_package = &irt_get_temperature_intel_package;
		irt_log_setting_s("irt pkg temperature measurement method", "intel");
	}
	else {
		irt_get_temperature_package = &irt_get_temperature_dummy;
		irt_log_setting_s("irt pkg temperature measurement method", "none");
	}
}


#endif // ifndef __GUARD_UTILS_IMPL_TEMPERATURE_IMPL_H
