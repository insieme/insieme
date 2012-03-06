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

void irt_opencl_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group) {
	uint32 ncpus = group->local_member_count;
	irt_wg_set_loop_scheduling_policy(group, &irt_g_loop_sched_policy_default);

	/*irt_loop_sched_policy shares_policy; // NEW LINE
	shares_policy.type = IRT_SHARES;
	shares_policy.participants = ncpus;
	shares_policy.param.shares[0] = 0.05;
	shares_policy.param.shares[1] = 0.1;
	shares_policy.param.shares[2] = 0.1;
	shares_policy.param.shares[3] = 0.1;
	shares_policy.param.shares[4] = 0.1;
	shares_policy.param.shares[5] = 0.1;
	shares_policy.param.shares[6] = 0.1;
	shares_policy.param.shares[7] = 0.1;
	irt_wg_set_loop_scheduling_policy(group, &shares_policy);*/
}
