################################################################################
#                       LTE UPLINK RECEIVER PHY BENCHMARK                      #
#                                                                              #
# This file is distributed under the license terms given by LICENSE.TXT        #
################################################################################
# Author: Magnus Sjalander                                                     #
################################################################################

define HELP_TEXT_GRAPH
echo "Graph Make Targets"
echo "--------------------------------------------------------------------------------"
echo "correlation_graph  Plots graph with user parameter/core utlization correlation"
echo "static_pow_graph   Plots graph with estimated static power"
echo "dynamic_pow_graph  Plots graph with measured dynamic power"
echo "active_core_graph  Plots graph with number of estimated active cores"
echo "input_PRB_graph    Plots graph with number of PRBs for every 25 subframe"
echo "input_layer_graph  Plots graph with number of layers for every 25 subframe"
echo "input_user_graph   Plots graph with number of layers for every 25 subframe"
echo "load_graph         Plots the estimated and measured workload"
echo "help_graph         Prints the help text of this section"
echo ""
endef
.PHONY: help_graph
help_graph:
	@${HELP_TEXT_GRAPH}

###########################################################
#                      GRAPH TARGETS                      #
###########################################################

${LOG_DIR}/graph/%.pdf: ${LOG_DIR}/%_samples
	@mkdir -p ${LOG_DIR}/graph
	@export GRAPH=$(notdir $(basename $@)) && \
	echo "Creating ${LOG_DIR}/graph/$${GRAPH}.pdf" &&\
	jgraph ${ROOT}/lib/graph/$${GRAPH}.jgr > ${LOG_DIR}/graph/$${GRAPH}.ps && \
	epstopdf ${LOG_DIR}/graph/$${GRAPH}.ps && \
	rm -f ${LOG_DIR}/graph/$${GRAPH}.ps

.PHONY: correlation_graph
correlation_graph: correlation_data
	@pl ${ROOT}/lib/graph/correlation.pl

.PHONY: active_core_graph
active_core_graph: ${LOG_DIR}/estimated_active_cores_processed
	@pl ${ROOT}/lib/graph/estimated_active_cores.pl

.PHONY: dynamic_pow_graph
dynamic_pow_graph:
	@pl ${ROOT}/lib/graph/dynamic-power.pl

.PHONY: static_pow_graph
static_pow_graph:
	@pl ${ROOT}/lib/graph/static-power.pl

.PHONY: input_PRB_graph
input_PRB_graph: ${LOG_DIR}/input_parameters_processed
	@pl ${ROOT}/lib/graph/input_parameters_RB.pl

.PHONY: input_layer_graph
input_layer_graph: ${LOG_DIR}/input_parameters_processed
	@pl ${ROOT}/lib/graph/input_parameters_layer.pl

.PHONY: input_user_graph
input_user_graph: ${LOG_DIR}/input_parameters_processed
	@pl ${ROOT}/lib/graph/input_parameters_users.pl

.PHONY: load_graph
load_graph: ${LOG_DIR}/estimated_load_processsed
	@pl ${ROOT}/lib/graph/estimated_load.pl

.PHONY: workload_graph
workload_graph: ${LOG_DIR}/workload_nap_nousleep_processed
	@pl ${ROOT}/lib/graph/workload-nap.pl

.PHONY: stew2011_graph
stew2011_graph: ${LOG_DIR}/workload_nap_nousleep_processed
	@pl ${ROOT}/lib/graph/stew2011.pl
