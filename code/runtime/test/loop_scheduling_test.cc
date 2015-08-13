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
#include <string>
#include <vector>
#include <sstream>
#include <iostream>
using std::string;
using std::ostream;
using std::vector;

#define IRT_LIBRARY_MAIN
#define IRT_LIBRARY_NO_MAIN_FUN
#include "irt_library.hxx"

#define VEC_SIZE 500
#define MAX_PARA 4

struct LoopTestCase {
	irt_loop_sched_policy policy;
	irt_work_item_range range;
};

class LoopSchedTest : public ::testing::TestWithParam<LoopTestCase> {
  public:
	// Initialize runtime library once to use in all tests
	static void SetUpTestCase() {
		irt::init(MAX_PARA);
	}
	static void TearDownTestCase() {
		irt::shutdown();
	}
};

string testCaseToString(const LoopTestCase& testCase) {
	std::stringstream ss;
	auto p = testCase.policy;
	auto r = testCase.range;
	ss << "Policy(type:" << p.type << ", participants:" << p.participants << ", chunk:" << p.param.chunk_size << ") / Range(" << r.begin << ".." << r.end << ":"
	   << r.step << ")";
	return ss.str();
}

TEST_P(LoopSchedTest, RangeCoverage) {
	LoopTestCase testCase = GetParam();
	// std::cout << testCaseToString(testCase) << std::endl;
	irt::run([testCase]() {
		std::vector<int32_t> testVec(VEC_SIZE, 0);
		irt::merge(irt::parallel(16, [testCase, &testVec]() {

			// set currently tested scheduling policy
			irt::master([testCase]() {
				irt_work_group* wg = irt_wi_get_wg(irt_wi_get_current(), 0);
				wg->cur_sched = testCase.policy;
			});
			irt::barrier();

			// execute loop
			irt::pfor_impl(testCase.range.begin, testCase.range.end, testCase.range.step,
			               [&testVec](int64 index) { irt_atomic_add_and_fetch(&testVec[index], 1, int32_t); });
			irt::barrier();

			// check result
			irt::master([testCase, &testVec]() {
				for(int64 i = 0; i < VEC_SIZE; ++i) {
					int32 expected = 0;
					if(i >= testCase.range.begin && i < testCase.range.end && ((i - testCase.range.begin) % testCase.range.step) == 0) { expected = 1; }
					EXPECT_EQ(expected, testVec[i]) << "Failed when testing case " << testCaseToString(testCase) << " / i: " << i;
				}
			});
		}));
	});
}

#define xstr(s) str(s)
#define str(s) #s

vector<LoopTestCase> getAllCases() {
	vector<LoopTestCase> ret;

	vector<irt_loop_sched_policy_type> policies = {IRT_STATIC, IRT_STATIC_CHUNKED, IRT_DYNAMIC, IRT_DYNAMIC_CHUNKED, IRT_GUIDED, IRT_GUIDED_CHUNKED};

	for(auto policy_type : policies) {
		for(int32 chunk_size = 1; chunk_size <= 9; ++chunk_size) {
			for(uint32 participants = 1; participants <= MAX_PARA; ++participants) {
				LoopTestCase t;
				t.policy = {policy_type, participants, {chunk_size}};

				for(int64 step = -4; step <= -1; step++) {
					t.range = {VEC_SIZE, 0, step};
					ret.push_back(t);
				}
				for(int64 step = 1; step < 8; step++) {
					t.range = {0, VEC_SIZE, step};
					ret.push_back(t);
					t.range = {0, VEC_SIZE / 2, step};
					ret.push_back(t);
					t.range = {VEC_SIZE / 3, 2 * (VEC_SIZE / 3), step};
					ret.push_back(t);
				}
			}
		}
	}
	return ret;
}

INSTANTIATE_TEST_CASE_P(RangeCoverageCheck, LoopSchedTest, ::testing::ValuesIn(getAllCases()));
