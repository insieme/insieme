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

#include "insieme/common/utils/gtest_utils.h"

#define IRT_CWBUFFER_LENGTH 2

#define IRT_LIBRARY_MAIN
#define IRT_LIBRARY_NO_MAIN_FUN
#include "irt_library.hxx"

#define N 22

TEST(WIOversubscription, Simple) {
	EXPECT_IN_TIME(10*1000, {
		irt::init(3);
		irt::run([]() {
			irt::merge(irt::parallel(N, [] {
				printf("Bla wi %d\n", irt_wi_get_current()->id.index);
			}));
		});
		irt::shutdown();
	});
}

TEST(WIOversubscription, Barrier) {
	EXPECT_IN_TIME(10*1000, {
		irt::init(3);
		irt::run([]() {
			irt::merge(irt::parallel(N, [] {
				irt::barrier();
				printf("Bla wi %d\n", irt_wi_get_current()->id.index);
				irt::barrier();
			}));
		});
		irt::shutdown();
	});
}

int RecParCount(int n) {
	if(n == 0) {
		return 1;
	}
	int ret = 0;
	irt::merge(irt::parallel(n, [&ret,n] {
		ret = RecParCount(n-1)+1;
	}));
	return ret;
}

TEST(WIOversubscription, Nested) {
	EXPECT_IN_TIME(100*1000, {
		irt::init(3);
		irt::run([]() {
			std::cout << RecParCount(5) << std::endl;
		});
		irt::shutdown();
	});
}
