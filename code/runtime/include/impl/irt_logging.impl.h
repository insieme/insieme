/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_IMPL_IRT_LOGGING_IMPL_H
#define __GUARD_IMPL_IRT_LOGGING_IMPL_H

#include "irt_logging.h"
#include "irt_version.h"
#include "compilerinfo.h"
#include "utils/affinity.h"
#include "utils/impl/affinity.impl.h"
#include <stdio.h>
#include <stdarg.h>

FILE* irt_g_log_file;

#if(IRT_LOGGING)

const char* _irt_time_string() {
	static char buffer[32];
	time_t rawtime;
	struct tm* timeinfo;

	time(&rawtime);
	timeinfo = localtime(&rawtime);
	strftime(buffer, 32, "%Y-%m-%d %H-%M-%S", timeinfo);

	return buffer;
}

void irt_log_init() {
	char* output_path = getenv("IRT_INST_OUTPUT_PATH");
	char fn[] = "insieme_runtime.log";
	char buffer[1024] = "";
	if(output_path != NULL) { sprintf(buffer, "%s/", output_path); }
	sprintf(buffer + strlen(buffer), "%s", fn);
#ifdef IRT_USE_MPI
	sprintf(buffer + strlen(buffer), ".%s", getenv("OMPI_COMM_WORLD_RANK"));
#endif

	irt_g_log_file = fopen(buffer, "w+");

	IRT_ASSERT(irt_g_log_file != NULL, IRT_ERR_IO, "Unable to create %s", buffer);

	irt_log("# Runtime logging started on %s\n", _irt_time_string());

	irt_log_compiler_info();

	irt_log_comment("Compile-time settings:");
	irt_log_setting_s("IRT_CODE_VERSION", IRT_CODE_VERSION);
	#if IRT_SCHED_POLICY == IRT_SCHED_POLICY_STATIC
	irt_log_setting_s("IRT_SCHED_POLICY", "IRT_SCHED_POLICY_STATIC");
	#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_LAZY_BINARY_SPLIT
	irt_log_setting_s("IRT_SCHED_POLICY", "IRT_SCHED_POLICY_LAZY_BINARY_SPLIT");
	#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING
	irt_log_setting_s("IRT_SCHED_POLICY", "IRT_SCHED_POLICY_STEALING");
	#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING_CIRCULAR
	irt_log_setting_s("IRT_SCHED_POLICY", "IRT_SCHED_POLICY_STEALING_CIRCULAR");
	#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_UBER
	irt_log_setting_s("IRT_SCHED_POLICY", "IRT_SCHED_POLICY_UBER");
	#else
	irt_log_setting_s("IRT_SCHED_POLICY", "IRT_SCHED_POLICY_UNKNOWN");
	#endif
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
	#ifdef IRT_ENABLE_OPENCL
	irt_log_setting_s("IRT_ENABLE_OPENCL", "enabled");
	#else
	irt_log_setting_s("IRT_ENABLE_OPENCL", "disabled");
	#endif
	#ifdef IRT_USE_PAPI
	irt_log_setting_s("IRT_USE_PAPI", "enabled");
	#else
	irt_log_setting_s("IRT_USE_PAPI", "disabled");
	#endif
	#ifdef IRT_USE_HWLOC
	irt_log_setting_s("IRT_USE_HWLOC", "enabled");
	#else
	irt_log_setting_s("IRT_USE_HWLOC", "disabled");
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
	#ifdef IRT_ASTEROIDEA_STACKS
	irt_log_setting_s("IRT_ASTEROIDEA_STACKS", "enabled");
	#else
	irt_log_setting_s("IRT_ASTEROIDEA_STACKS", "disabled");
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
	if(irt_g_log_file == NULL) { return; }
	va_list args;
	va_start(args, format);
	vfprintf(irt_g_log_file, format, args);
	fflush(irt_g_log_file);
	va_end(args);
}

void irt_log_cleanup() {
	if(irt_g_log_file == NULL) { return; }
	irt_log_setting_u("irt_g_time_ticks_per_sec", irt_g_time_ticks_per_sec);
	irt_log("# Runtime logging completed on %s\n", _irt_time_string());
	fclose(irt_g_log_file);
}


#else  // (IRT_LOGGING)
void irt_log_init() {}
void irt_log_comment(const char* comment) {}
void irt_log_setting_s(const char* name, const char* value) {}
void irt_log_setting_u(const char* name, uint64 value) {}
void irt_log(const char* format, ...) {}
void irt_log_cleanup() {}
#endif // (IRT_LOGGING)


#endif // ifndef __GUARD_IMPL_IRT_LOGGING_IMPL_H
