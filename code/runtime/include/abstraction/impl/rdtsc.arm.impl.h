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

#include "irt_inttypes.h"
#include <fcntl.h>
#include <sys/mman.h>
#include <stdio.h>

static inline uint32_t cp15_read_cbar(void)
{
	uint32_t cbar;
	__asm volatile ("mrc p15, 4, %[cbar], c15, c0, 0" : [cbar] "=r" (cbar));
	return cbar & ~0x1FFF; // Only [31:13] is valid
}

uint64 irt_time_ticks(void) {

#define MAP_SIZE 4096UL
#define MAP_MASK (MAP_SIZE - 1)

#define GLOBAL_TIMER_BASE 0X48240200U
	return 0;

	int fd;
	void *map_base, *virt_addr;
	uint64 l, u;

	//printf("base = %u\n", cp15_read_cbar());

	if((fd = open("/dev/mem", O_RDWR | O_SYNC)) == -1)
	{
		printf("open\n");
		return 0;
	}

	map_base = mmap(0, MAP_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, GLOBAL_TIMER_BASE & ~MAP_MASK);
	if(map_base == (void *) -1) 
	{
		printf("mmap\n");
		return 0;
	}

	virt_addr = map_base + (GLOBAL_TIMER_BASE & MAP_MASK);
	
	for(int i=0; i<500; i++)
	printf("%d ",*(((char*) map_base) + i));
	printf("\n");

	l = *((uint64*)virt_addr);
	u = *(((uint32*)virt_addr) +1);

	if(munmap(map_base, MAP_SIZE) == -1)
	{
		printf("munmap\n");
		return 0;
	}
	close(fd);

	return ((u << 32) | l);
}

// checks if rdtsc instruction is available
bool irt_time_ticks_available() {
#if 0
	volatile unsigned d;
	__asm__ __volatile__("cpuid" : "=d" (d) : "a" (0x00000001) : "ebx", "ecx");
	if((d & 0x00000010) > 0)
		return 1;
	else
		return 0;
#endif
	return 0;
}

bool irt_time_ticks_constant() {
#if 0
	volatile unsigned d;
	__asm__ __volatile__("cpuid" : "=d" (d) : "a" (0x80000007) : "ebx", "ecx");
	if((d & 0x00000100) > 0)
		return 1;
	else
		return 0;
#endif
	return 1;
}
