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
	using PointerResult = std::pair<bool,std::string>;
	using PointerResults = std::map<core::NodeAddress,PointerResult>;


	/**
	 * Run a pointer analysis: To what value does a pointer point to?
	 * @param target Return value for this expression
	 * @return a 'PointerResult' pair in the form of <analysisSuccessful?, value>
	 */
	PointerResult runPointerAnalysis(Context &context, const core::ExpressionAddress &target);


	/**
	 * Run a pointer analysis: To what value does a pointer point to?
	 * @param targets Return values for these expressions
	 * @return a 'PointerResults' map, where the node addresses are mapped to <analysisSuccessful?, value>
	 */
	PointerResults runPointerAnalysis(Context &context, const std::set<core::ExpressionAddress>& targets);


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
