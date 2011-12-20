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

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/backend/ocl_kernel/kernel_poly.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

using namespace insieme::annotations::ocl;
using namespace insieme::core;

// shortcut
#define BASIC builder.getNodeManager().getLangBasic()

namespace {

class KernelToLoopnestMapper : public core::transform::CachedNodeMapping {

	NodeManager& mgr;
	IRBuilder builder;

	// counters for the local and global dimensions
	size_t globalDim, groupDim, localDim;
	// loop variables for the loops to be introduced
	VariablePtr globalVars[3], localVars[3];

	/*
	 * checks if the first argument of the passed call is an integer literal. If yes and the value is between 0 and 2,
	 * it's value is returned, otherwise an assertion is raised
	 * @param
	 * call A CallExprPtr with an integer literal as first argument
	 * @return
	 * the value of the first argument
	 */
	int extractIndexFromArg(CallExprPtr call) {
		return 0;
	}

public:

	KernelToLoopnestMapper(NodeManager& manager) : mgr(manager), builder(manager), globalDim(0), groupDim(0), localDim(0) {

		// create the loop variables
		for(size_t i = 0; i < 3; ++i) {
			globalVars[i] = builder.variable(BASIC.getUInt4());
			localVars[i] = builder.variable(BASIC.getUInt4());
		}

	}

	const core::NodePtr resolveElement(const core::NodePtr& ptr) {
		if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(ptr))
			// replace calls to get_*_id with accesses to the appropriate loop variable
			if(isGetGlobalID(call)){
				return globalVars[extractIndexFromArg(call)];
			}

		return ptr->substitute(builder.getNodeManager(), *this);
	}
};

} // end anonymous namespace

ExpressionAddress KernelPoly::transformKernelToLoopnest(ExpressionAddress kernel){
	KernelToLoopnestMapper ktlm(program->getNodeManager());
//	kernel->getAddressedNode()->substitute(program->getNodeManager(), ktlm);

	return kernel;
}

void KernelPoly::genWiDiRelation() {
	// find the kernels inside the program
	auto lookForKernel = makeLambdaVisitor([&](const NodeAddress& node) {
		if(const LambdaExprAddress lambda = dynamic_address_cast<const LambdaExpr>(node)) {
			if(isKernel(lambda.getAddressedNode()))
				kernels.push_back(lambda);
		}
	});

	visitDepthFirstOnce(Address<Program>(program), lookForKernel);

	// analyze the kernels using the polyhedral model
	for_each(kernels, [&](ExpressionAddress& kernel) {
		transformKernelToLoopnest(kernel);
	});

}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
    
