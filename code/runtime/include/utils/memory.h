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

#include <stdio.h>

/*
 * Gets the virtual memory and resident set sizes in KB of the process calling it
 */

void irt_get_memory_usage(unsigned long* virt_size, unsigned long* res_size) {
        static long position_cache_virt = 0, position_cache_res = 0;
        FILE* file = fopen("/proc/self/status", "r");
        if(position_cache_virt == 0) { // first call, no position cached
                fscanf(file, "%*[^B]B VmSize:\t%lu", virt_size); // lookup entry
                position_cache_virt = ftell(file); // save stream position
                fscanf(file, " kB VmLck:\t%*u kB VmHWM:\t%*u kB"); // skip useless info
                fscanf(file, " VmRSS:\t%lu", res_size); // lookup entry
                position_cache_res = ftell(file); // save stream position
        } else { // if not first call, use cached positions, assumes max 8 digits
                char str[9];
                fseek(file, position_cache_virt-8, SEEK_SET);
                fgets(str, 9, file);
                *virt_size = atol(str);
                fseek(file, position_cache_res-8, SEEK_SET);
                fgets(str, 9, file);
                *res_size = atol(str);
        }   
        fclose(file);
}
