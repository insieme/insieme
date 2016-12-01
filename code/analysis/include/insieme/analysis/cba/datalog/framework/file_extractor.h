#pragma once

#include <set>
#include <string>
#include <fstream>

#include "insieme/analysis/cba/datalog/framework/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	/**
	 * Extracts facts from the given root node and save them in fact files (so that they can be used as input for stand-alone Soufflé)
	 */
	int extractFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer = [](const core::NodePtr&,int){}, bool debug = false, std::ostream &debugOut = std::cout);

	/**
	 * Extracts facts from the given root node and save them in fact files (so that they can be used as input for stand-alone Soufflé)
	 */
	int extractAddressFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer = [](const core::NodeAddress&,int){}, bool debug = false, std::ostream &debugOut = std::cout);

	/**
	 * Add facts manually to fact file. Useful to add facts that cannot be extracted from the IR itself
	 */
	bool addFactsManually(const std::string &folder, const std::string &relationName, const std::set<std::string> &facts, bool debug = false, std::ostream &debugOut = std::cout);


} // end namespace framework
} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
