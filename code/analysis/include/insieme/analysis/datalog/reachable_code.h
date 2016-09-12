#pragma once

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace datalog {

	/**
	 * Determines whether the addressed expression can be reached
	 * from the entry point of its root node.
	 *
	 * @param expr the expression to be reached, rooted by the entry point of the program
	 * @return true if reachable, false otherwise
	 */
	bool isReachabe(const core::ExpressionAddress& expr);

	/**
	 * Searches for dead code within the given code fragment and
	 * returns the root addresses of statements and expressions
	 * never to be executed.
	 *
	 * @param root the root of the code fragment to be analysed
	 * @return a list of expressions and statements that can not be reached
	 */
	std::vector<core::NodeAddress> getDeadCode(const core::NodePtr& root);

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
