/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace checks {

namespace {
	NodeAddress firstAddress(NodeAddress start, NodePtr node) {
		NodeAddress retval;
		visitDepthFirstInterruptible(start, [&](const NodeAddress& addr) -> bool {
			if(*node == *addr.getAddressedNode()) {
				retval = addr;
				return true;
			}
			return false;
		});
		return retval;
	}
}

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)

OptionalMessageList ScalarArrayIndexRangeCheck::visitCallExpr(const CallExprAddress& curcall) {
	OptionalMessageList res;
	auto& mgr = curcall->getNodeManager();
	auto& basic = mgr.getLangBasic();
	IRBuilder builder(mgr);

	CallExprPtr curPtr = curcall.getAddressedNode();

	for(unsigned argIndex = 0; argIndex < curPtr->getArguments().size(); ++argIndex) {
		 // the potential outer call to scalar.to.array in one of curcall's parameters
		ExpressionPtr curArg = curPtr->getArgument(argIndex);
		if (!core::analysis::isCallOf(curArg, basic.getScalarToArray())) continue;
		LambdaExprPtr called = dynamic_pointer_cast<const LambdaExpr>(curPtr->getFunctionExpr());
		if(!called) continue;
		VariablePtr param = called->getParameterList()[argIndex];
		//LOG(INFO) << "**************************************\n====\nparam:\n " << printer::PrettyPrinter(param) << "\n*********************\n";
		NodeAddress addr = firstAddress(curcall, called->getBody());
		if(addr) {
			visitDepthFirst(addr, [&](const VariableAddress& var) {
				if(*var.getAddressedNode() != *param) return;
				if(var.isRoot()) return;
				if(var.getParentAddress(1).getNodeType() != NT_CallExpr) return;
				CallExprAddress useCallAdr = var.getParentAddress(1).as<CallExprAddress>();
				CallExprPtr usecall = useCallAdr;
				if(usecall) {
					if(basic.isArrayRefElem1D(usecall->getFunctionExpr())) {
						try {
							auto formula = arithmetic::toFormula(usecall->getArgument(1));
							if(formula.isZero()) {
								// correct use
							} else {
								add(res, Message(useCallAdr,
									EC_SEMANTIC_ARRAY_INDEX_OUT_OF_RANGE,
									format("Potentially unsafe indexing of single-element array %s using formula %s", 
										toString(*(param)).c_str(), toString(formula).c_str()),
									Message::WARNING));
							}
						} catch(const arithmetic::NotAFormulaException& e) {
							add(res, Message(useCallAdr,
								EC_SEMANTIC_ARRAY_INDEX_OUT_OF_RANGE,
								format("Potentially unsafe indexing of single-element array %s using expression %s", 
									toString(*(param)).c_str(), toString(*(usecall->getArgument(1))).c_str()),
								Message::WARNING));
						}
					} else {
						// warn here as well? (used in unexpected call)
					}
				} else {
					// warn here as well? (used in non-call)
				}
			});
		}
	}

	return res;
}

OptionalMessageList UndefinedCheck::visitCallExpr(const CallExprAddress& curcall) {
	OptionalMessageList res;

	if (curcall.isRoot()) {
		return res;
	}

	auto& mgr = curcall->getNodeManager();
	auto& basic = mgr.getLangBasic();
	if(!core::analysis::isCallOf(curcall.getAddressedNode(), basic.getUndefined())) return res;

	// find first non-marker / helper parent
	NodeAddress cur = curcall.getParentAddress();
	while(!cur.isRoot() && cur->getNodeCategory() == NC_Support) {
		cur = cur.getParentAddress();
	}

	NodePtr parent = cur.getAddressedNode();

	// check if parent in allowed set
	NodeType pnt = parent->getNodeType();
	if(core::analysis::isCallOf(parent, basic.getRefNew()) 
		|| core::analysis::isCallOf(parent, basic.getRefVar()) 
		|| (core::analysis::isConstructorExpr(parent) && pnt != NT_JobExpr)) {

		return res;
	}
	// error if not
	std::cout << "\n~~~~~~~~~~~~~~~~~~ Node type of parent: " << pnt << std::endl;
	add(res, Message(curcall,
		EC_SEMANTIC_INCORRECT_UNDEFINED,
		string("Call to undefined(...) not enclosed within ref.new, ref.var or constructor expression"),
		Message::ERROR));
	return res;
}

#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
