#pragma once

#include <set>
#include <string>

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {


	using TargetRelations = std::map<string,std::set<core::NodeAddress>>;

	/**
	 * Extract the facts (including 'targets') to fact files (to be used with external Soufflé)
	 * @param targets Target nodes from which we want the values (mapped to the fact file to write them in)
	 * @param edCmds some 'ed' commands that will be printed in a special file. Useful for post-processing scripts to alter generated files
	 * @param outputDir Directory where fact files are created
	 * @return true if successful
	 */
	bool extractPointerFactsToFiles(const TargetRelations &targets, const string &edCmds = "", const std::string &outputDir = "/tmp/facts");

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
