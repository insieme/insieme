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

#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/ocl/ocl_annotations.h"
/*
#include "insieme/transform/polyhedral/transform.h"
#include "insieme/analysis/polyhedral/scop.h"
*/
#include "insieme/backend/ocl_kernel/kernel_poly.h"
//#include "insieme/backend/ocl_kernel/kernel_to_loop_nest.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

//using namespace insieme::transform::polyhedral;
using namespace insieme::annotations::ocl;
using namespace insieme::core;

// shortcut
#define BASIC builder.getNodeManager().getLangBasic()


StatementPtr KernelPoly::transformKernelToLoopnest(ExpressionAddress kernel){
	IRBuilder builder(program->getNodeManager());
	StatementPtr transformedKernel;
#if 0
	avoid including of kernel_to_loop_nest.h
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

	// prepare kernel function for analyis with the polyhedral model
	KernelToLoopnestMapper ktlm(program->getNodeManager(), globalSize, localSize);
	// statement(0) is the declaration of group-index-vector
	// statement(1) is the parallel of the kernel
	// statement(2) is the outer mergeAll
	// statement(3) is the return 0
	StatementList toConvert = kernelCall->getBody()->getStatements();
	// drop merge and return
	toConvert.pop_back(), toConvert.pop_back();
	StatementPtr transformedKernel = dynamic_pointer_cast<const StatementPtr>(ktlm.map(0, builder.compoundStmt(toConvert)));
	assert(transformedKernel && "KernelToLoopnestMapper corrupted the kernel function body");

    // remove returns at the first level
	NodeMapping* h;
	auto mapper = makeLambdaMapper([&](unsigned index, const NodePtr& element)->NodePtr{
		if(element->getNodeType() == NT_ReturnStmt){
			return builder.getNoOp();
		}

		if(element->getNodeType() == core::NT_LambdaExpr)
			return element;

		return element->substitute(program->getNodeManager(), *h);
	});

	h = &mapper;
	transformedKernel = h->map(0, transformedKernel);

	// try to use the induction variable were ever possible
//	transformedKernel = dynamic_pointer_cast<const Statement>(
//			core::transform::replaceAll(builder.getNodeManager(), transformedKernel, ktlm.getReplacements()));
//	assert(transformedKernel && "Variable replacing corrupted the loop nest");

	//replace kernel call with a loop nest
	VariablePtr *gLoopVars, *lLoopVars;
	size_t nGlobalLoops = ktlm.getGlobalDim(&gLoopVars);
	size_t nLocalLoops = ktlm.getLocalDim(&lLoopVars);

	for(size_t i = 0; i <= nLocalLoops; ++i) {

		ExpressionPtr upperBound = builder.callExpr(BASIC.getUInt8(),
				BASIC.getArraySubscript1D(), localSize, builder.literal(BASIC.getUInt8(), toString(i)));

		transformedKernel = builder.forStmt(lLoopVars[i], builder.literal(BASIC.getUInt8(), "0"), upperBound, builder.literal(BASIC.getUInt8(), "1"),
				transformedKernel);
	}
	for(size_t i = 0; i <= nGlobalLoops; ++i) {

		ExpressionPtr upperBound = builder.callExpr(BASIC.getUInt8(),
				BASIC.getArraySubscript1D(), globalSize, builder.literal(BASIC.getUInt8(), toString(i)));
		ExpressionPtr stepsize = builder.callExpr(BASIC.getUInt8(),
				BASIC.getArraySubscript1D(), localSize, builder.literal(BASIC.getUInt8(), toString(i)));

		transformedKernel = builder.forStmt(gLoopVars[i], builder.literal(BASIC.getUInt8(), "0"), upperBound, stepsize, transformedKernel);
	}
#endif
	return transformedKernel;
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
		loopNests.push_back(transformKernelToLoopnest(kernel));
	});
/* dropped the polyhedral idea
	for_each(loopNests, [&](StatementPtr& nest) {
		std::cout << analysis::scop::mark(nest) << std::endl;
	});
	*/
}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
    
