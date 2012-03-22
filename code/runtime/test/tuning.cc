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

#include "data/tuning.h"
#include "data/metric_table.h"

// horrible hack incoming
uint32 irt_g_error_key = 0;
uint32 irt_g_worker_count = 0;

TEST(tuning, compileable_test) {
	// just testing whether header is compiling

	// check size of irt value element
	EXPECT_LE(sizeof(irt_value), 2*sizeof(void*));

	// just test access
	EXPECT_STREQ("The execution time in nano-seconds", irt_get_metric_info(IRT_METRIC_EXEC_TIME_INDEX)->description);
	EXPECT_STREQ("The execution time in nano-seconds", irt_get_metric_info(IRT_METRIC_EXEC_TIME->index)->description);

	EXPECT_EQ(IRT_METRIC_EXEC_TIME_INDEX, IRT_METRIC_EXEC_TIME->index);

	EXPECT_LT(0, g_num_atomic_metrics);

	const irt_metric** all = g_all_atomic_metrics;
	for(int i=0; i<g_num_atomic_metrics; i++) {
		EXPECT_STRNE("Just testing presence of string", irt_get_metric_info(all[i]->index)->description);
	}

}
