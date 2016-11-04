#pragma once

#include "insieme/analysis/cba/datalog/framework/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	/**
	 * Extracts facts from the given root node and inserts them into the given program using node pointers.
	 */
	int extractFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer = [](const core::NodePtr&,int){});

	/**
	 * Extracts facts from the given root node and inserts them into the given program using node addresses.
	 */
	int extractAddressFactsToFiles(const std::string &folder, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer = [](const core::NodeAddress&,int){});


} // end namespace framework
} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
