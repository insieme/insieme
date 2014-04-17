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
#include <omp.h>

#include <irt_all_impls.h>
#include <standalone.h>

// #include "meta_information/default_generator.h"
// #include "insieme/meta_information/effort_estimation.def"


// --- compiler generated ---

#include "meta_information/struct_generator.h"
#include "insieme/meta_information/effort_estimation.def"


struct _irt_meta_info_table_entry {
	effort_estimation_info effort_estimation;
};

int irt_g_meta_info_size = 2;

irt_meta_info_table_entry irt_g_meta_info[2] = {
	{ { NULL, 8 } },
	{ { NULL, 3 } },
};

// --- compiler generated - end ---


// --- user code ---

#include "meta_information/accessor_generator.h"
#include "insieme/meta_information/effort_estimation.def"

TEST(meta_information, basic) {
	//if(!irt_is_meta_info_effort_estimation_available()) {
	EXPECT_EQ(irt_get_effort_estimation_info(0)->fallback_estimate, 8);
	EXPECT_EQ(irt_get_effort_estimation_info(1)->fallback_estimate, 3);
	EXPECT_EQ((void*)irt_get_effort_estimation_info(1)->estimation_function, (void*)NULL);
	//}
}

