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

#include "wi_performance.h"

#define IRT_WI_PD_BLOCKSIZE 4

//used for getting clock cycles
unsigned long long getTicks(void) {
	volatile unsigned long long a, d;
	__asm__ volatile("rdtsc" : "=a" (a), "=d" (d));
	return (a | (d << 32));
}

void resize(irt_wi_pd_table* table) {
	table->size += table->blocksize;
	table->data = realloc(table->data, sizeof(_irt_wi_performance_data)*table->size);
}

irt_wi_pd_table* irt_wi_create_performance_table(unsigned blocksize) {

	irt_wi_pd_table* table = malloc(sizeof(irt_wi_pd_table));
	table->size = blocksize * 2;
	table->number_of_elements = 0;
	table->data = malloc(sizeof(_irt_wi_performance_data) * table->size);
	return table;
}

void irt_wi_destroy_performance_table(irt_wi_pd_table* table) {
	free(table->data);
	free(table);
}

void irt_wi_insert_performance_start(irt_wi_pd_table* table) {

	unsigned long long time = getTicks();

	if(table->number_of_elements >= table->size)
		resize(table);

	(table->data[table->number_of_elements]).start = time;
}

void irt_wi_insert_performance_end(irt_wi_pd_table* table) {

	unsigned long long time = getTicks();

	if(table->number_of_elements >= table->size)
		resize(table);

	table->data[(table->number_of_elements)++].end = time;
}

