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

#include "insieme/transform/sequential/constant_folding.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/pattern/ir_generator.h"

/*
#include "insieme/transform/polyhedral/transform.h"
#include "insieme/analysis/polyhedral/scop.h"
*/
#include "insieme/backend/ocl_kernel/kernel_poly.h"
//#include "insieme/backend/ocl_kernel/kernel_to_loop_nest.h"
#include "insieme/backend/ocl_kernel/kernel_analysis_utils.h"

#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

//using namespace insieme::transform::polyhedral;
using namespace insieme::annotations::ocl;
using namespace insieme::core;
using namespace insieme::transform::pattern;
namespace irg = insieme::transform::pattern::generator::irg;

// shortcut
#define BASIC builder.getNodeManager().getLangBasic()

/*
 * gets an expression after the replacements of insertIndutionVariable have been done and fixes some ref and cast related errors
 */
ExpressionPtr KernelPoly::cleanUsingMapper(const ExpressionPtr& expr) {
	NodeMapping* cleaner;
	NodeManager& mgr = basic.getNodeManager();
	auto mapper = makeLambdaMapper([&builder, &mgr, &basic, &cleaner](unsigned index, const NodePtr& element)->NodePtr{
		// stop recursion at type level
		if (element->getNodeCategory() == NodeCategory::NC_Type) {
			return element;
		}

		NodePtr newElement = element->substitute(mgr, *cleaner);

		if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(newElement)) {
			// remove unnecessary deref
			if(basic.isRefDeref(call->getFunctionExpr())) {
				const ExpressionPtr arg = call->getArgument(0);
				if(arg->getType()->getNodeType() != NT_RefType)
					return arg;
			}
		}

		if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(newElement)) {
			// remove illegal casts from non-ref to ref types
			if(const RefTypePtr resTy = dynamic_pointer_cast<const RefType>(cast->getType())) {
				ExpressionPtr subExpr = cast.getSubExpression();
				if(subExpr->getType()->getNodeType() != NT_RefType) {
					if(*resTy->getElementType() == *subExpr)
						return subExpr;
					return builder.castExpr(resTy->getElementType(), subExpr);
				}
			}

			// remove illegal casts from ref to non-ref types
			if(cast->getType()->getNodeType() != NT_RefType) {
				ExpressionPtr subExpr = cast.getSubExpression();
				if(subExpr->getType()->getNodeType() == NT_RefType)
					return builder.castExpr(cast->getType(), builder.deref(subExpr));
			}
		}


		return newElement;
	});

	cleaner = &mapper;
	return  cleaner->map(0, expr);
}



/*
 * tries to identify kernel functions
 */
ExpressionPtr KernelPoly::isKernelFct(const CallExprPtr& call){

	TreePatternPtr kernelCall = irp::callExpr( irp::literal("call_kernel"), irp::callExpr( irp::literal("_ocl_kernel_wrapper"), var("kernel") << *any) << *any);

	MatchOpt&& match = kernelCall->matchPointer(call);
	if(match) {
		return dynamic_pointer_cast<const Expression>(match->getVarBinding("kernel").getValue());
	}
	return Pointer<Expression>(NULL);
}

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

ExpressionPtr KernelPoly::insertInductionVariables(ExpressionPtr kernel) {
	InductionVarMapper ivm(program.getNodeManager());

//	const CallExprPtr kernelCall = dynamic_pointer_cast<const CallExpr>(kernel);
//	assert(kernelCall && "Parent of kernel is not a call expression");

	ExpressionPtr transformedKernel = dynamic_pointer_cast<const ExpressionPtr>(ivm.map(0, kernel));
/*	for_each(ivm.getReplacements(), [](std::pair<NodePtr, NodePtr>r){
		std::cout << r.first << " - " << r.second << std::endl;
	});
*/
	insieme::core::printer::PrettyPrinter pp(transformedKernel);
//std::cout << "Transformed Kernel " << pp << std::endl;
	return cleanUsingMapper(transformedKernel);
}

/*
 * Generates a map with one entry for every global variable and an Expression for the lower and upper boundary for its accesses
 */
AccessMap KernelPoly::collectArrayAccessIndices(ExpressionPtr kernel) {
	IRBuilder builder(program->getNodeManager());
	AccessExprCollector aec(builder);

	visitDepthFirstOnce(kernel, aec);

	return aec.getAccesses();
}

/*
 * Checks if the variable var is an induction variable of a loop inside kernel
 */
bool KernelPoly::isInductionVariable(VariablePtr var, LambdaExprPtr kernel, ExpressionPtr& lowerBound, ExpressionPtr& upperBound) {
	auto visitLoopHeader = makeLambdaVisitor([&](const NodePtr& node) {
		if(const ForStmtPtr loop = dynamic_pointer_cast<const ForStmt>(node)) {
			if(*var == *loop->getDeclaration()->getVariable()) {
				lowerBound = loop->getDeclaration()->getInitialization();
				upperBound = builder.sub(loop->getEnd(), builder.intLit(1, true));
				return true;
			}
		}

		return false; // continue visit
	});

	return visitDepthFirstOnceInterruptible(kernel, visitLoopHeader);
}
/*
 * Checks if the variable var is a parameter of LambdaExpr kernel
 */
bool KernelPoly::isParameter(VariablePtr var, LambdaExprPtr kernel) {
//	std::cout << "Params "  << kernel->getLambda()->getParameterList() << std::endl;
	assert(var && "You passed a null pointer");

	VariableList params = kernel->getLambda()->getParameterList();

	for(auto I = params.begin(); I != params.end(); ++I) {
		if(*var == **I)
			return true;
	}
	return false;
}

/*
 * Takes the expression of the index argument of a subscript to a global variable and generates the lower and upper boundary of it,
 * trying to get rid of local and loop-induction variables. If this is not possible 0 (lower bound) and infinity (upper bound) are returned
 */
std::pair<ExpressionPtr, ExpressionPtr> KernelPoly::genBoundaries(ExpressionPtr access, ExpressionPtr kernelExpr, ACCESS_TYPE accessType) {
	// Todo add support for reversed loops
	LambdaExprPtr kernel = static_pointer_cast<const LambdaExpr>(kernelExpr);
	IRBuilder builder(kernel->getNodeManager());
//	const Extensions& extensions(kernel->getNodeManager().getLangExtension<Extensions>());
	VariableList usedParams;

	std::vector<VariablePtr> neededArgs; // kernel arguments needed to evaluate the boundary expressions
	// transformations to be applied to make lower/upper boundary evaluable at runtime
	utils::map::PointerMap<NodePtr, NodePtr> lowerBreplacements, upperBreplacements;


	bool fail = false;

//	IRVisitor<bool>* visitAccessPtr;
	auto visitAccess = makeLambdaVisitor([&](const NodePtr& node) {
		if (node->getNodeCategory() == NodeCategory::NC_Type || node->getNodeCategory() == NC_Value || node->getNodeCategory() == NC_IntTypeParam ||
				node->getNodeCategory() == NC_Support) {
			return false;
		}

		if(node->getNodeType() == NT_Literal || node->getNodeType() == NT_CastExpr)
			return false; // continue visit

		if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(node)){
			ExpressionPtr fun = call->getFunctionExpr();

			if(basic.isLinearIntOp(fun) || basic.isRefOp(fun) || fun->toString().find("get_global_id") != string::npos)
				return false;

			// modulo can only be handled when reading from it
//			if((basic.isSignedIntMod(fun) || basic.isUnsignedIntMod(fun)) && accessType == ACCESS_TYPE::read)
//				return false;

// too optimistic :(
//			if(fun->getNodeType() !=  NT_LambdaExpr)
//				return false;
		}
		if(const VariablePtr var = dynamic_pointer_cast<const Variable>(node)) {
			if(isParameter(var, kernel)) {
				usedParams.push_back(var);
				return false;
			}

			ExpressionPtr lowerBound, upperBound;
			if(isInductionVariable(var, kernel, lowerBound, upperBound)) {
				lowerBreplacements[var] = genBoundaries(lowerBound, kernel, accessType).first;
				upperBreplacements[var] = genBoundaries(upperBound, kernel, accessType).second;
				return false;
			}

		}

//std::cout << "\nFailing at " << node << " -  " << access << std::endl;
		return true; // found something I cannot handle, stop visiting
	});
//	visitAccessPtr = &visitAccess;

	fail = visitDepthFirstOnceInterruptible(access, visitAccess);

	if(fail)
		return std::make_pair(builder.literal(BASIC.getInt4(), "0"), builder.literal(BASIC.getInt4(), "2147483646")); // int4 min and max-1

	// return the acces expression with the needed replacements for lower and upper boundary applied
	return std::make_pair(static_pointer_cast<const ExpressionPtr>(core::transform::replaceAll(kernel->getNodeManager(), access, lowerBreplacements)),
			static_pointer_cast<const ExpressionPtr>(core::transform::replaceAll(kernel->getNodeManager(), access, upperBreplacements)));
}

void KernelPoly::genWiDiRelation() {
	// find the kernels inside the program
	auto lookForKernel = makeLambdaVisitor([&](const NodeAddress& node) {
		if(const CallExprAddress lambda = dynamic_address_cast<const CallExpr>(node)) {
			CallExprPtr&& call = lambda.getAddressedNode();
			ExpressionPtr kernel = isKernelFct(call);
			if(kernel) {
				kernels.push_back(kernel);
			}
		}
	});

	visitDepthFirstOnce(Address<Program>(program), lookForKernel);
/*
	// analyze the kernels using the polyhedral model
	for_each(kernels, [&](ExpressionAddress& kernel) {
		loopNests.push_back(transformKernelToLoopnest(kernel));
	});
 dropped the polyhedral idea
	for_each(loopNests, [&](StatementPtr& nest) {
		std::cout << analysis::scop::mark(nest) << std::endl;
	});
	*/

	for_each(kernels, [&](ExpressionPtr& kernel) {
		transformedKernels.push_back(insertInductionVariables(kernel));
	});

	NodeManager& mgr = program.getNodeManager();
	IRBuilder builder(mgr);
	size_t cnt = 0;

	for_each(transformedKernels, [&](ExpressionPtr& kernel) {
		AccessMap accesses = collectArrayAccessIndices(kernel);
		std::vector<annotations::Range> ranges;

//insieme::core::printer::PrettyPrinter pp(kernel);
//std::cout << "TRansromfed kernel: \n" << pp << std::endl;

		// for boundaries we always use int4
		TypePtr int4 = BASIC.getInt4();


		//FIXME remove this, we can do better, right?
		size_t dirtyLimiter;

		//construct min and max expressions
		for_each(accesses, [&](std::pair<VariablePtr, insieme::utils::map::PointerMap<core::ExpressionPtr, ACCESS_TYPE> > variable){
//			std::cout << "\n" << variable.first << std::endl;
			ExpressionPtr lowerBoundary;
			ExpressionPtr upperBoundary;
			bool splittable = true;
			ACCESS_TYPE accessType = ACCESS_TYPE::null;
			for(auto I = variable.second.begin(); I != variable.second.end(); ++I) {
				dirtyLimiter = 0;
				std::pair<ExpressionPtr, ACCESS_TYPE> access = *I;
				std::pair<ExpressionPtr, ExpressionPtr> boundaries = genBoundaries(access.first, kernel, access.second);

				if( splittable) { // check if buffer is splittable if not already marked as unsplittable
					ExpressionPtr lower = *boundaries.first->getType() == *int4 ? boundaries.first : builder.castExpr(int4, boundaries.first);
					ExpressionPtr upper = *boundaries.second->getType() == *int4 ? boundaries.second : builder.castExpr(int4, boundaries.second);

					++dirtyLimiter;

					if(boundaries.first->toString().find("get_global_id") == string::npos ||
						boundaries.second->toString().find("get_global_id") == string::npos
						|| dirtyLimiter > 5) {
						// not splittable, use the  entire array
						splittable = false;
						lowerBoundary = builder.literal(BASIC.getInt4(), "0");
						upperBoundary = builder.literal(BASIC.getInt4(), "2147483646");
					} else {
						if(!lowerBoundary) { // first iteration, just copy the first access
							lowerBoundary = lower;
							upperBoundary = upper;
						} else { // later iterations, construct nested min/max expressions
							lowerBoundary = builder.callExpr(mgr.getLangBasic().getSelect(), lowerBoundary, lower, mgr.getLangBasic().getSignedIntGt());
							upperBoundary = builder.callExpr(mgr.getLangBasic().getSelect(), upperBoundary, upper, mgr.getLangBasic().getSignedIntLt());
						}
					}
				}
				// if there is one reading access, threat the variable as to be read
				accessType = ACCESS_TYPE(accessType | access.second);
//				std::cout << "\t" << access.first << std::endl;
			}

			// add one to upper boundary because the upper boundary of a range is not included in it
			// the highest value of gid however is included... obviously
			upperBoundary = builder.add(upperBoundary, builder.intLit(1, true));

			annotations::Range tmp(variable.first, lowerBoundary, upperBoundary, accessType, splittable);
			ranges.push_back(tmp);
		});

		// construct range annotation
		annotations::DataRangeAnnotationPtr rangeAnnotation = std::make_shared<annotations::DataRangeAnnotation>(
				annotations::DataRangeAnnotation(ranges));

		// add annotation to kernel call, assuming the kernels and the transformed kernels are in the same order
		kernels.at(cnt)->addAnnotation(rangeAnnotation);
		/*if(kernels.at(cnt)->hasAnnotation(annotations::DataRangeAnnotation::KEY)) {
			std::cout << "ANNOT is there \n";
			std::cout << *kernels.at(cnt)->getAnnotation(annotations::DataRangeAnnotation::KEY) << std::endl;
		}*/
		++cnt;
	});
}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
    
