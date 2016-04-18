#pragma once

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace datalog {

	/**
	 * Determines whether the given type is a polymorph type.
	 */
	bool isPolymorph(const core::TypePtr& type, bool debug = false);

	/**
	 * Collects all returns within a lambda.
	 *
	 * @param lambda the lambda to be analyzed
	 * @return the addresses of all return statements within the lambda
	 */
	std::vector<core::ReturnStmtAddress> getReturns(const core::LambdaPtr& lambda);




} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
