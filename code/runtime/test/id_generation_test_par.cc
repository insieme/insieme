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

#include <gtest/gtest.h>
#include <pthread.h>
#include <omp.h>

#include <id_generation.h>

#include <impl/error_handling.impl.h>

#define TEST_COUNT 10000

// horrible hack incoming
uint32 irt_g_error_key = 0;

IRT_DECLARE_ID_TYPE(id_gen_test);
IRT_MAKE_ID_TYPE(id_gen_test);

irt_id_gen_test_id gen_id;
#pragma omp threadprivate(gen_id)

TEST(id_generation, parallel_ops) {

	irt_id_gen_test_id ids[TEST_COUNT];

	#pragma omp parallel
	{
		gen_id.node = 42;
		gen_id.thread = omp_get_thread_num();
		#pragma omp parallel
		for(int i=0; i<TEST_COUNT; ++i) {
			ids[i] = irt_generate_id_gen_test_id(&gen_id);
		}
	}

	for(int i=0; i<TEST_COUNT; ++i) {
		for(int j=0; j<TEST_COUNT; ++j) {
			if(i == j) continue;
			EXPECT_NE(ids[i].full, ids[j].full);
		}
		EXPECT_EQ(ids[i].node, gen_id.node);
	}
}
