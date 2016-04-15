#pragma once

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace datalog {

	/**
	 * Determines whether the two given expressions may reference the same object.
	 * Both expression addresses need to be rooted by the same node.
	 *
	 * @param a the first expression
	 * @param b the second expression
	 * @return true if they may reference the same object, false otherwise
	 */
	bool mayAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b);

	/**
	 * Determines whether the two given expressions are for sure referencing the same object.
	 * Both expression addresses need to be rooted by the same node.
	 *
	 * @param a the first expression
	 * @param b the second expression
	 * @return true if they are referencing the same object, otherwise false
	 */
	bool areAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b);


} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
