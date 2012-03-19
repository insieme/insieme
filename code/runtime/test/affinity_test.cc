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

#include "utils/impl/affinity.impl.h"
#include "standalone.h"

TEST(affinity, fill) {
	_irt_set_num_cpus(8);
	irt_g_worker_count = 8;

	irt_affinity_policy pol;
	pol.type = IRT_AFFINITY_FILL;

	for(int i=0; i<8; ++i) {
		EXPECT_TRUE((irt_affinity_mask)(1 << i) == irt_get_affinity(i, pol));
	}
}

TEST(affinity, skip) {
	_irt_set_num_cpus(8);
	irt_g_worker_count = 8;
	
	irt_affinity_policy pol;
	pol.type = IRT_AFFINITY_SKIP;

	pol.skip_count = 1;
	EXPECT_TRUE((irt_affinity_mask)(1 << 0) == irt_get_affinity(0, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 2) == irt_get_affinity(1, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 4) == irt_get_affinity(2, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 6) == irt_get_affinity(3, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 1) == irt_get_affinity(4, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 3) == irt_get_affinity(5, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 5) == irt_get_affinity(6, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 7) == irt_get_affinity(7, pol));
	
	pol.skip_count = 2;
	EXPECT_TRUE((irt_affinity_mask)(1 << 0) == irt_get_affinity(0, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 3) == irt_get_affinity(1, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 6) == irt_get_affinity(2, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 2) == irt_get_affinity(3, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 5) == irt_get_affinity(4, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 1) == irt_get_affinity(5, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 4) == irt_get_affinity(6, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 7) == irt_get_affinity(7, pol));
	
	pol.skip_count = 3;
	EXPECT_TRUE((irt_affinity_mask)(1 << 0) == irt_get_affinity(0, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 4) == irt_get_affinity(1, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 1) == irt_get_affinity(2, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 5) == irt_get_affinity(3, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 2) == irt_get_affinity(4, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 6) == irt_get_affinity(5, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 3) == irt_get_affinity(6, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 7) == irt_get_affinity(7, pol));
}


TEST(affinity, maxdist) {
	_irt_set_num_cpus(8);
	
	irt_affinity_policy pol;
	pol.type = IRT_AFFINITY_MAX_DISTANCE;

	irt_g_worker_count = 2;
	EXPECT_TRUE((irt_affinity_mask)(1 << 4) == irt_get_affinity(0, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 0) == irt_get_affinity(1, pol));

	irt_g_worker_count = 4;
	EXPECT_TRUE((irt_affinity_mask)(1 << 2) == irt_get_affinity(0, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 6) == irt_get_affinity(1, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 0) == irt_get_affinity(2, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 4) == irt_get_affinity(3, pol));
	
	irt_g_worker_count = 8;
	EXPECT_TRUE((irt_affinity_mask)(1 << 1) == irt_get_affinity(0, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 3) == irt_get_affinity(1, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 5) == irt_get_affinity(2, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 7) == irt_get_affinity(3, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 0) == irt_get_affinity(4, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 2) == irt_get_affinity(5, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 4) == irt_get_affinity(6, pol));
	EXPECT_TRUE((irt_affinity_mask)(1 << 6) == irt_get_affinity(7, pol));
}
