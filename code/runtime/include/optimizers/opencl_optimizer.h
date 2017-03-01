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
 *
 */
#pragma once
#ifndef __GUARD_OPTIMIZERS_OPENCL_OPTIMIZER_H
#define __GUARD_OPTIMIZERS_OPENCL_OPTIMIZER_H
irt_loop_sched_policy irt_g_ocl_shares_policy;

void irt_get_split_values() {
	char* split_str = getenv("IRT_OCL_SPLIT_VALUES");
	if(split_str) {
		char* tok = strtok(split_str, ", ");
		unsigned i = 0;
		while(tok != NULL) {
			irt_g_ocl_shares_policy.param.shares[i++] = atof(tok);
			tok = strtok(NULL, ", ");
		}
		if(i != irt_g_worker_count) {
			IRT_WARN("Splitting values specified, but not for all devices.\n %d splitting values\n %d devices", i, irt_g_worker_count);
		}
	} else {
		float split_value = 1.0 / irt_g_worker_count;
		for(unsigned i = 0; i < irt_g_worker_count; ++i) {
			irt_g_ocl_shares_policy.param.shares[i] = split_value;
		}
	}
}

void irt_opencl_optimizer_context_startup(irt_context* context) {
	// irt_loop_sched_policy shares_policy;
	irt_g_ocl_shares_policy.type = IRT_SHARES;
	irt_g_ocl_shares_policy.participants = irt_g_worker_count;
	irt_get_split_values();
}

void irt_opencl_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group) {
	irt_wg_set_loop_scheduling_policy(group, &irt_g_ocl_shares_policy);
	// irt_wg_set_loop_scheduling_policy(group, &irt_g_loop_sched_policy_default);
}


#endif // ifndef __GUARD_OPTIMIZERS_OPENCL_OPTIMIZER_H
