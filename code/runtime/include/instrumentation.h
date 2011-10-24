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

//#define IRT_ENABLE_INSTRUMENTATION

typedef struct _irt_wi_performance_data {
	uint64 start;
	uint64 end;
} _irt_wi_performance_data;

typedef struct irt_wi_pd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_wi_performance_data* data;
} irt_wi_pd_table;

irt_wi_pd_table* irt_wi_create_performance_table(unsigned blocksize);
void irt_wi_destroy_performance_table(irt_wi_pd_table* table);

void irt_wi_insert_performance_start(irt_wi_pd_table* table); 
void irt_wi_insert_performance_end(irt_wi_pd_table* table);


