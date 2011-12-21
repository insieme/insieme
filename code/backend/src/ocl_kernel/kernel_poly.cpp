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

	const NodeManager& mgr;
	const IRBuilder builder;

	// counters for the local and global dimensions
	size_t globalDim, groupDim, localDim;
	// loop variables for the loops to be introduced
	VariablePtr globalVars[3], localVars[3];
	// the vectors containing the 3D size of the kernel
	const ExpressionPtr globalSize, localSize;

	/*
	 * checks if the first argument of the passed call is an integer literal. If yes and the value is between 0 and 2,
	 * it's value is returned, otherwise an assertion is raised
	 * @param
	 * call A CallExprPtr with an integer literal as first argument
	 * @return
	 * the value of the first argument
	 */
	size_t extractIndexFromArg(CallExprPtr call) const {
		ExpressionList args = call->getArguments();
		assert(args.size() > 0 && "Call to opencl get id function must have one argument");
		size_t retval = 0;

		// try to read literal
		ExpressionPtr arg = args.at(0);
		// remove casts
		CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(arg);
		while(cast) {
			arg = cast->getSubExpression();
			cast = dynamic_pointer_cast<const CastExpr>(arg);
		}
		if(const LiteralPtr dim = dynamic_pointer_cast<const Literal>(arg))
			retval = dim->getValueAs<size_t>();

		assert(retval <= 2 && "Argument of opencl get id function must be a literal between 0 an 2");
		return retval;
	}

public:

	KernelToLoopnestMapper(NodeManager& manager, const ExpressionPtr globalSize, const ExpressionPtr localSize) :
		mgr(manager), builder(manager), globalDim(0), groupDim(0), localDim(0), globalSize(globalSize), localSize(localSize) {

		// create the loop variables
		for(size_t i = 0; i < 3; ++i) {
			globalVars[i] = builder.variable(BASIC.getUInt8());
			localVars[i] = builder.variable(BASIC.getUInt8());
		}

	}

	const core::NodePtr resolveElement(const core::NodePtr& ptr) {
		if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(ptr)) {
			const ExpressionPtr fun = call->getFunctionExpr();
			//std::cout << "Call: " << ptr << std::endl;
			// replace calls to get_*_id with accesses to the appropriate loop variable
			if(isGetGlobalID(fun)){
				size_t dim = extractIndexFromArg(call);
				globalDim = std::max(dim, globalDim);
				return globalVars[dim];
			}
			if(isGetLocalID(fun)) {
				size_t dim = extractIndexFromArg(call);
				localDim = std::max(dim, localDim);
				return localVars[extractIndexFromArg(call)];
			}
			if(0) {//isGetGroupID(call)))
				size_t dim = extractIndexFromArg(call);
				globalDim = std::max(dim, globalDim);
				localDim = std::max(dim, localDim);
				return builder.callExpr(BASIC.getUInt8(), BASIC.getUnsignedIntDiv(), globalVars[dim],
					builder.callExpr(BASIC.getUInt8(), BASIC.getArraySubscript1D(), localSize, builder.literal(BASIC.getUInt8(), toString(dim))));
			}
	}
		return ptr->substitute(builder.getNodeManager(), *this);
	}

	/*
	 * returns the information for the global loop nest
	 * @param
	 * loopVars A pointer that will be set to the array of the three loop variables
	 * @retrun
	 * the number of (global) loops needed to represent the kernels semantics
	 */
	size_t getGlobalDim(VariablePtr** loopVars) {
		*loopVars = globalVars;
		return globalDim;
	}
	/*
	 * returns the information for the local loop nest
	 * @param
	 * loopVars A pointer that will be set to the array of the three loop variables
	 * @retrun
	 * the number of (local) loops needed to represent the kernels semantics
	 */
	size_t getLocalDim(VariablePtr** loopVars) {
		*loopVars = localVars;
		return localDim;
	}
};

} // end anonymous namespace

ExpressionAddress KernelPoly::transformKernelToLoopnest(ExpressionAddress kernel){
	// collect global and local size argument
	// TODO maybe use the argument known to the outside code?
/*	const CallExprPtr kernelCall = dynamic_pointer_cast<const CallExpr>(kernel.getParentNode());
	assert(kernelCall && "Parent of kernel is not a call expression");
	size_t nArgs = kernelCall.getArguments().size();
	ExpressionPtr globalSize = kernelCall.getArguments().at(nArgs-1);
	ExpressionPtr localSize = kernelCall.getArguments().at(nArgs-2);*/

	const LambdaExprPtr kernelCall = dynamic_pointer_cast<const LambdaExpr>(kernel.getAddressedNode());
	assert(kernelCall && "Parent of kernel is not a call expression");
	std::vector<VariablePtr> params = kernelCall->getParameterList()->getElements();
	size_t nArgs = params.size();
	ExpressionPtr globalSize = params.at(nArgs-1);
	ExpressionPtr localSize = params.at(nArgs-2);

	std::cout << "GT: " << globalSize->getType() << std::endl;

	// prepare kernel function for analyis with the polyhedral model
	KernelToLoopnestMapper ktlm(program->getNodeManager(), globalSize, localSize);
	StatementPtr transformedKernel = dynamic_pointer_cast<const StatementPtr>(kernelCall->getBody()->substitute(program->getNodeManager(), ktlm));
	assert(transformedKernel && "KernelToLoopnestMapper corrupted the kernel function body");

	//replace kernel call with a loop nest
	VariablePtr *gLoopVars, *lLoopVars;
	size_t nGlobalLoops = ktlm.getGlobalDim(&gLoopVars);
	size_t nLocalLoops = ktlm.getLocalDim(&lLoopVars);

	IRBuilder builder = IRBuilder(program->getNodeManager());
	for(size_t i = 0; i <= nLocalLoops; --i) {

		ExpressionPtr upperBound = builder.callExpr(BASIC.getUInt8(),
				BASIC.getArraySubscript1D(), localSize, builder.literal(BASIC.getUInt8(), toString(i)));

		transformedKernel = builder.forStmt(lLoopVars[i], builder.literal(BASIC.getUInt8(), "0"), upperBound, builder.literal(BASIC.getUInt8(), "1"),
				transformedKernel);
	}
	for(size_t i = 0; i <= nGlobalLoops; --i) {

		ExpressionPtr upperBound = builder.callExpr(BASIC.getUInt8(),
				BASIC.getArraySubscript1D(), globalSize, builder.literal(BASIC.getUInt8(), toString(i)));
		ExpressionPtr stepsize = builder.callExpr(BASIC.getUInt8(),
				BASIC.getArraySubscript1D(), localSize, builder.literal(BASIC.getUInt8(), toString(i)));

		transformedKernel = builder.forStmt(gLoopVars[i], builder.literal(BASIC.getUInt8(), "0"), upperBound, stepsize, transformedKernel);
	}

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
    
