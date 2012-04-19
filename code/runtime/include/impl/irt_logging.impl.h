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

#include "irt_logging.h"
#include "utils/affinity.h"
#include <stdio.h>
#include <stdarg.h>

FILE* irt_g_log_file;

#if(IRT_LOGGING)

const char* _irt_time_string() {
	static char buffer[32];
	time_t rawtime;
	struct tm *timeinfo;

	time(&rawtime);
	timeinfo = localtime(&rawtime);
	strftime(buffer, 32, "%Y-%m-%d %H-%M-%S", timeinfo);

	return buffer;
}

void irt_log_init() {
	irt_g_log_file = fopen("insieme_runtime.log", "w+");
	irt_log("# Runtime logging started on %s\n", _irt_time_string());
	
	irt_log_comment("Compile-time settings:");
#ifdef IRT_RUNTIME_TUNING
	irt_log_setting_s("IRT_RUNTIME_TUNING", "enabled");
#else
	irt_log_setting_s("IRT_RUNTIME_TUNING", "disabled");
#endif
#ifdef IRT_RUNTIME_TUNING_EXTENDED
	irt_log_setting_s("IRT_RUNTIME_TUNING_EXTENDED", "enabled");
#else
	irt_log_setting_s("IRT_RUNTIME_TUNING_EXTENDED", "disabled");
#endif
#ifdef USE_OPENCL
	irt_log_setting_s("USE_OPENCL", "enabled");
#else
	irt_log_setting_s("USE_OPENCL", "disabled");
#endif
#ifdef IRT_ENABLE_INSTRUMENTATION
	irt_log_setting_s("IRT_ENABLE_INSTRUMENTATION", "enabled");
#else
	irt_log_setting_s("IRT_ENABLE_INSTRUMENTATION", "disabled");
#endif
#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_log_setting_s("IRT_ENABLE_REGION_INSTRUMENTATION", "enabled");
#else
	irt_log_setting_s("IRT_ENABLE_REGION_INSTRUMENTATION", "disabled");
#endif
#ifdef IRT_ENABLE_ENERGY_INSTRUMENTATION
	irt_log_setting_s("IRT_ENABLE_ENERGY_INSTRUMENTATION", "enabled");
#else
	irt_log_setting_s("IRT_ENABLE_ENERGY_INSTRUMENTATION", "disabled");
#endif

	irt_log_comment("Environment:");
	irt_log_setting_u("cores_available", irt_affinity_cores_available());
	irt_log_setting_u(IRT_DEFAULT_VARIANT_ENV, getenv(IRT_DEFAULT_VARIANT_ENV) ? atoi(getenv(IRT_DEFAULT_VARIANT_ENV)) : 0);
}

void irt_log_comment(const char* comment) {
	irt_log("# %s\n", comment);
}

void irt_log_setting_s(const char* name, const char* value) {
	irt_log("%40s: %40s\n", name, value);
}
void irt_log_setting_u(const char* name, uint64 value) {
	irt_log("%40s: %40lu\n", name, value);
}

void irt_log(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(irt_g_log_file, format, args);
    va_end(args);
}

void irt_log_cleanup() {
	irt_log("# Runtime logging completed on %s\n", _irt_time_string());
	fclose(irt_g_log_file);
}


#else // (IRT_LOGGING)
void irt_log_init() {}
void irt_log_comment(const char* comment) {}
void irt_log_setting_s(const char* name, const char* value) {}
void irt_log_setting_u(const char* name, uint64 value) {}
void irt_log(const char* format, ...) {}
void irt_log_cleanup() {}
#endif // (IRT_LOGGING)