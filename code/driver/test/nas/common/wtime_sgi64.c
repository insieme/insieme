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

#include <sys/types.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/syssgi.h>
#include <sys/immu.h>
#include <errno.h>
#include <stdio.h>

/* The following works on SGI Power Challenge systems */

typedef unsigned long iotimer_t;

unsigned int cycleval;
volatile iotimer_t *iotimer_addr, base_counter;
double resolution;

/* address_t is an integer type big enough to hold an address */
typedef unsigned long address_t;



void timer_init() 
{
  
  int fd;
  char *virt_addr;
  address_t phys_addr, page_offset, pagemask, pagebase_addr;
  
  pagemask = getpagesize() - 1;
  errno = 0;
  phys_addr = syssgi(SGI_QUERY_CYCLECNTR, &cycleval);
  if (errno != 0) {
    perror("SGI_QUERY_CYCLECNTR");
    exit(1);
  }
  /* rel_addr = page offset of physical address */
  page_offset = phys_addr & pagemask;
  pagebase_addr = phys_addr - page_offset;
  fd = open("/dev/mmem", O_RDONLY);

  virt_addr = mmap(0, pagemask, PROT_READ, MAP_PRIVATE, fd, pagebase_addr);
  virt_addr = virt_addr + page_offset;
  iotimer_addr = (iotimer_t *)virt_addr;
  /* cycleval in picoseconds to this gives resolution in seconds */
  resolution = 1.0e-12*cycleval; 
  base_counter = *iotimer_addr;
}

void wtime_(double *time) 
{
  static int initialized = 0;
  volatile iotimer_t counter_value;
  if (!initialized) { 
    timer_init();
    initialized = 1;
  }
  counter_value = *iotimer_addr - base_counter;
  *time = (double)counter_value * resolution;
}


void wtime(double *time) 
{
  static int initialized = 0;
  volatile iotimer_t counter_value;
  if (!initialized) { 
    timer_init();
    initialized = 1;
  }
  counter_value = *iotimer_addr - base_counter;
  *time = (double)counter_value * resolution;
}


