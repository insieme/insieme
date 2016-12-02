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
	int extractFacts(souffle::SouffleProgram& analysis, const core::NodePtr& root, const std::function<void(core::NodePtr,int)>& nodeIndexer = [](const core::NodePtr&,int){}, bool debug = false);

	/**
	 * Extracts facts from the given root node and inserts them into the given program using node addresses.
	 */
	int extractAddressFacts(souffle::SouffleProgram& analysis, const core::NodePtr& root, const std::function<void(core::NodeAddress,int)>& nodeIndexer = [](const core::NodeAddress&,int){}, bool debug = false);


} // end namespace framework
} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
