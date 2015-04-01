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

#include <gtest/gtest.h>
#include <omp.h>

#include <irt_all_impls.h>
#include <standalone.h>

// --- compiler generated ---

irt_meta_info_table_entry irt_g_meta_info[] = {
	{ { true, NULL, 8 } },
	{ { true, NULL, 3 } },
	{ { false, NULL, 0 } },
};

// --- compiler generated - end ---

TEST(meta_information, basic) {
	EXPECT_EQ(irt_meta_info_is_effort_estimation_available(&(irt_g_meta_info[0])), true);
	EXPECT_EQ(irt_meta_info_is_effort_estimation_available(&(irt_g_meta_info[2])), false);
	EXPECT_EQ(irt_meta_info_is_opencl_available(&(irt_g_meta_info[0])), false);
	EXPECT_EQ(irt_meta_info_is_opencl_available(&(irt_g_meta_info[1])), false);
	EXPECT_EQ(irt_meta_info_is_opencl_available(&(irt_g_meta_info[2])), false);
	EXPECT_EQ(irt_meta_info_get_effort_estimation(&(irt_g_meta_info[0]))->fallback_estimate, 8);
	EXPECT_EQ(irt_meta_info_get_effort_estimation(&(irt_g_meta_info[1]))->fallback_estimate, 3);
	EXPECT_EQ((void*)irt_meta_info_get_effort_estimation(&(irt_g_meta_info[1]))->estimation_function, (void*)NULL);
}

