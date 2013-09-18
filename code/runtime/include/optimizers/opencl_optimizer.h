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
irt_loop_sched_policy irt_g_ocl_shares_policy;

void irt_get_split_values() {
	char* split_str = getenv("IRT_OCL_SPLIT_VALUES");
	if(split_str) {
		char *tok = strtok(split_str, ", ");
		unsigned i = 0;
		while(tok != NULL) {
		  irt_g_ocl_shares_policy.param.shares[i++] = atof(tok);
		  tok = strtok(NULL, ", ");
		}
		if(i != irt_g_worker_count) IRT_WARN("Splitting values specified, but not for all devices.\n");
	} else {
		float split_value = 1.0/irt_g_worker_count;
		for(unsigned i = 0; i < irt_g_worker_count; ++i){
			irt_g_ocl_shares_policy.param.shares[i] = split_value;
		}
	}
}

void irt_opencl_optimizer_context_startup(irt_context *context) {
	//irt_loop_sched_policy shares_policy;
	irt_g_ocl_shares_policy.type = IRT_SHARES;
	irt_g_ocl_shares_policy.participants = irt_g_worker_count;

	irt_get_split_values();
}

void irt_opencl_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group) {
	irt_wg_set_loop_scheduling_policy(group, &irt_g_ocl_shares_policy);
	//irt_wg_set_loop_scheduling_policy(group, &irt_g_loop_sched_policy_default);
}
