#pragma once

#include <utility>
#include <map>
#include <string>

#include "insieme/core/ir_address.h"

#include "insieme/analysis/cba/datalog/context.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	/**
	 * Result type from pointer analysis
	 */
	using PointerResult = std::map<core::NodeAddress,std::pair<bool,std::string>>;


	/**
	 * Run a pointer analysis: To what value does a pointer point to?
	 * @param targets Return values for these expressions
	 * @return a 'PointerResult' map, where the node addresses are mapped to <analysisSuccessful?, value>
	 */
	PointerResult runPointerAnalysis(const std::set<core::ExpressionAddress>& targets);


	/**
	 * Extract the facts (including 'targets') to fact files (to be used with external Soufflé)
	 * @param targets Target nodes from which we want the values
	 * @param outputDir Directory where fact files are created
	 * @return true if successful
	 */
	bool extractPointerFactsToFiles(const std::set<core::ExpressionAddress>& targets, const std::string& outputDir = "/tmp/facts");

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
