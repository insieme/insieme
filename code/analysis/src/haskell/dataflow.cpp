#include "insieme/analysis/haskell/dataflow.h"

#include "insieme/analysis/haskell/adapter.h"

#include <iostream>

using namespace insieme::core;

namespace insieme {
namespace analysis {
namespace haskell {

	boost::optional<VariableAddress> getDefinitionPoint(const VariableAddress& var) {
		auto& env = Environment::getInstance();

		auto tree = env.passTree(var.getRootNode());
		auto var_addr = env.passAddress(var);

		auto target = env.findDeclr(tree, var_addr);
		if (!target) {
			return {};
		}

		return target->toNodeAddress(var.getRootNode()).as<VariableAddress>();
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme
