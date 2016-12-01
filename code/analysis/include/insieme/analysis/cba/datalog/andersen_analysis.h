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


	void runAndersen(Context &context, const core::VariableAddress &targetVariable);


} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
