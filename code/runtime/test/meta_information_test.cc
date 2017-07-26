/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <gtest/gtest.h>

#include <irt_all_impls.h>
#include <standalone.h>

// --- compiler generated ---

irt_meta_info_table_entry irt_g_meta_info[] = {
    {{true, NULL, 8}}, {{true, NULL, 3}}, {{false, NULL, 0}},
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
