################################################################################
#                       LTE UPLINK RECEIVER PHY BENCHMARK                      #
#                                                                              #
# This file is distributed under the license terms given by LICENSE.TXT        #
################################################################################
# Author: Magnus Sjalander                                                     #
################################################################################

define HELP_TEXT_CORRELATION
echo "Correlation Make Targets"
echo "--------------------------------------------------------------------------------"
echo "correlation        Process logfile to compute core utilization for a set of RB"
echo "                   and layers. Used with PARAMETER_MODEL_CORRELATION_RB[_SEC]" 
echo "error_correlation  Computes the deviation (error) if multiple samples of the"
echo "                   same RB/Layer exist in the files produces by correlation"
echo "help_correlation:  Prints the help text of this section"
echo ""
endef
.PHONY: help_correlation
help_correlation:
	@${HELP_TEXT_CORRELATION}

ifndef ROOT
ROOT     = ..
endif

.PHONY: correlation
correlation: ${LOG_DIR}/${log}_layer[1-4]

${LOG_DIR}/${log}_layer[1-4]:
ifndef log
	@echo "You need to provide a logfile (log=[filename])"
	@echo "The file is to be located in ${LOG_DIR}"
	@exit 1
else
	@${SCRIPT_DIR}/correlation.php ${LOG_DIR}/${log}
endif

.PHONY: error_correlation
error_correlation: ${LOG_DIR}/${log}_layer[1-4]
	@for FN in ${LOG_DIR}/${log}_layer[1-4]; \
	do \
		${SCRIPT_DIR}/correlation-error-bar.php $${FN} > $${FN}_err; \
	done

.PHONY: correlation_data
correlation_data: ${LOG_DIR}/single_user_correlation ${LOG_DIR}/single_user_correlation_L[1-4]_QPSK ${LOG_DIR}/single_user_correlation_L[1-4]_QPSK_err

${LOG_DIR}/single_user_correlation_L[1-4]_QPSK_err ${LOG_DIR}/single_user_correlation_L[1-4]_16QAM_err ${LOG_DIR}/single_user_correlation_L[1-4]_64QAM_err:
	@for FN in ${LOG_DIR}/single_user_correlation_L[1-4]_*; \
	do \
		${SCRIPT_DIR}/correlation-error-bar.php $${FN} > $${FN}_err; \
	done

${LOG_DIR}/single_user_correlation_L[1-4]_QPSK ${LOG_DIR}/single_user_correlation_L[1-4]_16QAM ${LOG_DIR}/single_user_correlation_L[1-4]_64QAM:
	@${SCRIPT_DIR}/correlation.php ${LOG_DIR}/single_user_correlation

${LOG_DIR}/single_user_correlation:
	@make uplink
	make -s run_pci > $@

${LOG_DIR}/estimated_active_cores_processed: ${LOG_DIR}/estimated_active_cores
	@${SCRIPT_DIR}/input_parameters.php $^

${LOG_DIR}/input_parameters_processed: ${LOG_DIR}/input_parameters
	@${SCRIPT_DIR}/input_parameters.php $^

${LOG_DIR}/estimated_load_processsed: ${LOG_DIR}/estimated_load
	@${SCRIPT_DIR}/estimated_load.php $^

${LOG_DIR}/workload_nap_nousleep_processed: ${LOG_DIR}/workload_nap_nousleep
	@${SCRIPT_DIR}/workload.php $^
