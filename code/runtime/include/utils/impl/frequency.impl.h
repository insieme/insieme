/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_UTILS_IMPL_FREQUENCY_IMPL_H
#define __GUARD_UTILS_IMPL_FREQUENCY_IMPL_H

#include "utils/frequency.h"

#if defined(_GEMS)

#include "frequency.gems.impl.h"

#else

#include "frequency.std.impl.h"

#endif

/*
 * sets the frequency of a core of a worker to the value specified by the environment variable
 *
 * MAX = maximum value
 * MIN = minimum value
 * OS = leave it to the OS to do DVFS
 * numerical value = clock frequency in MHz
 *
 * only frequencies offered by the operating system / hardware (c.f. scaling_available_frequencies) are actually written
 */

int32 irt_cpu_freq_set_frequency_worker_env(const irt_worker* worker) {
	char* freq_str_orig = getenv(IRT_CPU_FREQUENCIES);
	if(freq_str_orig) {
		char* freq_str = (char*)alloca((strlen(freq_str_orig) + 1) * sizeof(char)); // needed since strtok modifies the string it's working on
		strcpy(freq_str, freq_str_orig);

		int32 retval;
		uint32 freqs[IRT_INST_MAX_CPU_FREQUENCIES];
		uint32 length;

		if((retval = irt_cpu_freq_get_available_frequencies_worker(worker, freqs, &length)) == 0) {
			char* tok = strtok(freq_str, ",");
			// copies the first entry, to be used by all workers if it was the only one supplied
			char* first_copy = (char*)alloca((strlen(tok) + 1) * sizeof(char));
			strcpy(first_copy, tok);

			// used to check if only one setting was supplied
			int i;
			// skip nonrelevant entries - the n-th tok is the setting for worker with id n
			for(i = 1; i <= worker->id.thread; ++i) {
				tok = strtok(NULL, ",");
				if(!tok) { break; }
			}

			// if i != 1 we did at least one iteration => numbers of workers and frequencies must match
			if(!tok) {
				if(i != 1) {
					IRT_ASSERT(tok != NULL, IRT_ERR_INSTRUMENTATION, "Number of workers is higher than number of cpu frequencies provided!")
				} else {
					tok = first_copy;
				}
			}

			if(strcmp(tok, "OS") == 0) {
				int32 retval = irt_cpu_freq_reset_frequency_worker(worker);
				#ifdef _GEMS_SIM
				// alloca is implemented as malloc
				free(first_copy);
				free(freq_str);
				#endif
				return retval;
			} else if(strcmp(tok, "MAX") == 0) {
				int32 retval = irt_cpu_freq_set_frequency_worker(worker, freqs[0]);
				#ifdef _GEMS_SIM
				// alloca is implemented as malloc
				free(first_copy);
				free(freq_str);
				#endif
				return retval;
			} else if(strcmp(tok, "MIN") == 0) {
				int32 retval = irt_cpu_freq_set_frequency_worker(worker, freqs[length - 1]);
				#ifdef _GEMS_SIM
				// alloca is implemented as malloc
				free(first_copy);
				free(freq_str);
				#endif
				return retval;
			} else {
				uint32 freq = atoi(tok);
				bool available;
				for(uint32 i = 0; i < length; ++i) {
					if(freq == freqs[i]) {
						available = true;
						break;
					}
				}

				if(available) {
					int32 retval = irt_cpu_freq_set_frequency_worker(worker, freq);
					#ifdef _GEMS_SIM
					// alloca is implemented as malloc
					free(first_copy);
					free(freq_str);
					#endif
					return retval;
				} else {
					IRT_DEBUG("Instrumentation: Requested frequency setting %s unknown", getenv(IRT_CPU_FREQUENCIES));
					#ifdef _GEMS_SIM
					// alloca is implemented as malloc
					free(first_copy);
					free(freq_str);
					#endif
					return -1;
				}
			}
			#ifdef _GEMS_SIM
			// alloca is implemented as malloc
			free(first_copy);
			#endif
		} else {
		#ifdef _GEMS_SIM
			// alloca is implemented as malloc
			free(freq_str);
			#endif
			return retval;
		}
		#ifdef _GEMS_SIM
		// alloca is implemented as malloc
		free(freq_str);
		#endif
	} else {
		return 0;
	}
	return 0;
}

#endif // ifndef __GUARD_UTILS_IMPL_FREQUENCY_IMPL_H
