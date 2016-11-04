#pragma once

#include <vector>
#include <utility>
#include <map>
#include <string>

#include "insieme/core/ir_address.h"

#include "insieme/analysis/datalog/context.h"

namespace insieme {
namespace analysis {
namespace datalog {

	using PointerResult = std::map<core::NodeAddress,std::pair<bool,std::string>>;

	PointerResult runPointerAnalysis(const std::vector<core::ExpressionAddress>& targets);

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
