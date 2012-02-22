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

#include "papi.h"

// environment variable holding the papi parameters, separated by whitespaces
#define IRT_INST_PAPI_PARAMS "IRT_INST_PAPI_PARAMS"
#define IRT_PAPI_MAX_COUNTERS 16

uint32 irt_g_number_of_papi_parameters = 0;

typedef struct { const char *key; const int value; } papi_event_table;

// lookuptable containing all papi preset events and their string representation 
static papi_event_table lookuptable[] = {
	{"PAPI_L1_DCM", PAPI_L1_DCM},
	{"PAPI_L1_ICM", PAPI_L1_ICM},
	{"PAPI_L2_DCM", PAPI_L2_DCM},
	{"PAPI_L2_ICM", PAPI_L2_ICM},
	{"PAPI_L3_DCM", PAPI_L3_DCM},
	{"PAPI_L3_ICM", PAPI_L3_ICM},
	{"PAPI_L1_TCM", PAPI_L1_TCM},
	{"PAPI_L2_TCM", PAPI_L2_TCM},
	{"PAPI_L3_TCM", PAPI_L3_TCM},
	{"PAPI_CA_SNP", PAPI_CA_SNP},
	{"PAPI_CA_SHR", PAPI_CA_SHR},
	{"PAPI_CA_CLN", PAPI_CA_CLN},
	{"PAPI_CA_INV", PAPI_CA_INV},
	{"PAPI_CA_ITV", PAPI_CA_ITV},
	{"PAPI_L3_LDM", PAPI_L3_LDM},
	{"PAPI_L3_STM", PAPI_L3_STM},
	{"PAPI_BRU_IDL", PAPI_BRU_IDL},
	{"PAPI_FXU_IDL", PAPI_FXU_IDL},
	{"PAPI_FPU_IDL", PAPI_FPU_IDL},
	{"PAPI_LSU_IDL", PAPI_LSU_IDL},
	{"PAPI_TLB_DM", PAPI_TLB_DM},
	{"PAPI_TLB_IM", PAPI_TLB_IM},
	{"PAPI_TLB_TL", PAPI_TLB_TL},
	{"PAPI_L1_LDM", PAPI_L1_LDM},
	{"PAPI_L1_STM", PAPI_L1_STM},
	{"PAPI_L2_LDM", PAPI_L2_LDM},
	{"PAPI_L2_STM", PAPI_L2_STM},
	{"PAPI_BTAC_M", PAPI_BTAC_M},
	{"PAPI_PRF_DM", PAPI_PRF_DM},
	{"PAPI_L3_DCH", PAPI_L3_DCH},
	{"PAPI_TLB_SD", PAPI_TLB_SD},
	{"PAPI_CSR_FAL", PAPI_CSR_FAL},
	{"PAPI_CSR_SUC", PAPI_CSR_SUC},
	{"PAPI_CSR_TOT", PAPI_CSR_TOT},
	{"PAPI_MEM_SCY", PAPI_MEM_SCY},
	{"PAPI_MEM_RCY", PAPI_MEM_RCY},
	{"PAPI_MEM_WCY", PAPI_MEM_WCY},
	{"PAPI_STL_ICY", PAPI_STL_ICY},
	{"PAPI_FUL_ICY", PAPI_FUL_ICY},
	{"PAPI_STL_CCY", PAPI_STL_CCY},
	{"PAPI_FUL_CCY", PAPI_FUL_CCY},
	{"PAPI_HW_INT", PAPI_HW_INT},
	{"PAPI_BR_UCN", PAPI_BR_UCN},
	{"PAPI_BR_CN", PAPI_BR_CN},
	{"PAPI_BR_TKN", PAPI_BR_TKN},
	{"PAPI_BR_NTK", PAPI_BR_NTK},
	{"PAPI_BR_MSP", PAPI_BR_MSP},
	{"PAPI_BR_PRC", PAPI_BR_PRC},
	{"PAPI_FMA_INS", PAPI_FMA_INS},
	{"PAPI_TOT_IIS", PAPI_TOT_IIS},
	{"PAPI_TOT_INS", PAPI_TOT_INS},
	{"PAPI_INT_INS", PAPI_INT_INS},
	{"PAPI_FP_INS", PAPI_FP_INS},
	{"PAPI_LD_INS", PAPI_LD_INS},
	{"PAPI_SR_INS", PAPI_SR_INS},
	{"PAPI_BR_INS", PAPI_BR_INS},
	{"PAPI_VEC_INS", PAPI_VEC_INS},
	{"PAPI_RES_STL", PAPI_RES_STL},
	{"PAPI_FP_STAL", PAPI_FP_STAL},
	{"PAPI_TOT_CYC", PAPI_TOT_CYC},
	{"PAPI_LST_INS", PAPI_LST_INS},
	{"PAPI_SYC_INS", PAPI_SYC_INS},
	{"PAPI_L1_DCH", PAPI_L1_DCH},
	{"PAPI_L2_DCH", PAPI_L2_DCH},
	{"PAPI_L1_DCA", PAPI_L1_DCA},
	{"PAPI_L2_DCA", PAPI_L2_DCA},
	{"PAPI_L3_DCA", PAPI_L3_DCA},
	{"PAPI_L1_DCR", PAPI_L1_DCR},
	{"PAPI_L2_DCR", PAPI_L2_DCR},
	{"PAPI_L3_DCR", PAPI_L3_DCR},
	{"PAPI_L1_DCW", PAPI_L1_DCW},
	{"PAPI_L2_DCW", PAPI_L2_DCW},
	{"PAPI_L3_DCW", PAPI_L3_DCW},
	{"PAPI_L1_ICH", PAPI_L1_ICH},
	{"PAPI_L2_ICH", PAPI_L2_ICH},
	{"PAPI_L3_ICH", PAPI_L3_ICH},
	{"PAPI_L1_ICA", PAPI_L1_ICA},
	{"PAPI_L2_ICA", PAPI_L2_ICA},
	{"PAPI_L3_ICA", PAPI_L3_ICA},
	{"PAPI_L1_ICR", PAPI_L1_ICR},
	{"PAPI_L2_ICR", PAPI_L2_ICR},
	{"PAPI_L3_ICR", PAPI_L3_ICR},
	{"PAPI_L1_ICW", PAPI_L1_ICW},
	{"PAPI_L2_ICW", PAPI_L2_ICW},
	{"PAPI_L3_ICW", PAPI_L3_ICW},
	{"PAPI_L1_TCH", PAPI_L1_TCH},
	{"PAPI_L2_TCH", PAPI_L2_TCH},
	{"PAPI_L3_TCH", PAPI_L3_TCH},
	{"PAPI_L1_TCA", PAPI_L1_TCA},
	{"PAPI_L2_TCA", PAPI_L2_TCA},
	{"PAPI_L3_TCA", PAPI_L3_TCA},
	{"PAPI_L1_TCR", PAPI_L1_TCR},
	{"PAPI_L2_TCR", PAPI_L2_TCR},
	{"PAPI_L3_TCR", PAPI_L3_TCR},
	{"PAPI_L1_TCW", PAPI_L1_TCW},
	{"PAPI_L2_TCW", PAPI_L2_TCW},
	{"PAPI_L3_TCW", PAPI_L3_TCW},
	{"PAPI_FML_INS", PAPI_FML_INS},
	{"PAPI_FAD_INS", PAPI_FAD_INS},
	{"PAPI_FDV_INS", PAPI_FDV_INS},
	{"PAPI_FSQ_INS", PAPI_FSQ_INS},
	{"PAPI_FNV_INS", PAPI_FNV_INS},
	{"PAPI_FP_OPS", PAPI_FP_OPS},
	{"PAPI_SP_OPS", PAPI_SP_OPS},
	{"PAPI_DP_OPS", PAPI_DP_OPS},
	{"PAPI_VEC_SP", PAPI_VEC_SP},
	{"PAPI_VEC_DP", PAPI_VEC_DP}
};

/*
 * looks up papi event in the table and adds it to the given eventset if found, returns true if so, false outerwise
 */

bool irt_add_papi_from_string(int32* irt_papi_event_set, const char *key) {
	uint32 number_of_entries = sizeof(lookuptable)/sizeof(papi_event_table);
	for(uint32 i = 0; i < number_of_entries; ++i) {
		if(strcmp(lookuptable[i].key, key) == 0) {
			if(PAPI_add_event(*irt_papi_event_set, lookuptable[i].value) != PAPI_OK) {
	                	printf("Error while trying to add PAPI events to event set\n");
				return false;
			} else
				return true;
		}
	}
	printf("Event not found!\n");
	return false;
}

/*
 * parses all papi event names in the environment variable (if not takes default events) to add them to the eventset
 */

void irt_parse_papi_env(int32* irt_papi_event_set) {
	const char papi_params_default[] = "PAPI_TOT_CYC PAPI_L2_TCM PAPI_L3_TCA PAPI_L3_TCM";
	char papi_params[IRT_INST_PAPI_PARAMS*16]; // assuming max 16 chars per name

	// get papi counter names from environment variable if present, take default otherwise
	if(getenv(IRT_INST_PAPI_PARAMS))
		strcpy(papi_params, getenv(IRT_INST_PAPI_PARAMS));
	else
		strcpy(papi_params, papi_params_default);

	char* papi_param_toks[IRT_PAPI_MAX_COUNTERS];
	char* cur_tok;
	uint32 number_of_params_supplied = 0;
	uint32 number_of_params_added = 0;

	// get the first parameter
	if((papi_param_toks[0] = strtok(papi_params, " ")) != NULL)
		number_of_params_supplied++;
	else
		return;
	
	// get all remaininc parameters
	while((cur_tok = strtok(NULL, " ")) != NULL)
		papi_param_toks[number_of_params_supplied++] = cur_tok;

	// add all found parameters to the papi eventset
	for(uint32 j = 0; j < number_of_params_supplied; ++j) {
		if(irt_add_papi_from_string(irt_papi_event_set, papi_param_toks[j]) == true)
			number_of_params_added++;
	}

	irt_g_number_of_papi_parameters = number_of_params_added;
}

/*
 * initializes general papi support, does not provide thread support yet
 */

void irt_initialize_papi() {
	int32 retval = 0;
	// initialize papi and check version
	retval = PAPI_library_init(PAPI_VER_CURRENT);
	if(retval > 0 && retval != PAPI_VER_CURRENT)
		fprintf(stderr, "PAPI version mismatch: require %d but found %d\n", PAPI_VER_CURRENT, retval);
	else if (retval < 0)
		fprintf(stderr, "Error while trying to initialize PAPI: %d\n", retval);
}

/*
 * initialize papi's thread support, create eventset and add events to it
 */

void irt_initialize_papi_thread(uint64 pthread(void), int32* irt_papi_event_set ) {

	int32 retval = 0;
	if((retval = PAPI_thread_init(pthread)) != PAPI_OK)
		IRT_DEBUG("Error while trying to initialize PAPI's thread support: %d\n", retval);

	*irt_papi_event_set = PAPI_NULL; // necessary, otherwise PAPI_create_eventset() will fail

	if(PAPI_create_eventset(irt_papi_event_set) != PAPI_OK)
		IRT_DEBUG("Error while trying to create PAPI event set\n");

	// parse event names and add them	
	irt_parse_papi_env(irt_papi_event_set);
}
