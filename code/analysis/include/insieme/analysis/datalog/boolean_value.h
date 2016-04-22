#pragma once

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace datalog {


	bool isTrue(const core::ExpressionAddress& expr);

	bool isFalse(const core::ExpressionAddress& expr);

	bool mayBeTrue(const core::ExpressionAddress& expr);

	bool mayBeFalse(const core::ExpressionAddress& expr);


} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
