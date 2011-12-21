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

#include "insieme/transform/polyhedral/transform.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/backend/ocl_kernel/kernel_poly.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

using namespace insieme::transform::polyhedral;
using namespace insieme::annotations::ocl;
using namespace insieme::core;

// shortcut
#define BASIC builder.getNodeManager().getLangBasic()

namespace {

class KernelToLoopnestMapper : public core::transform::CachedNodeMapping {

	NodeManager& mgr;
	const IRBuilder builder;

	// counters for the local and global dimensions
	size_t globalDim, groupDim, localDim;
	// loop variables for the loops to be introduced
	VariablePtr globalVars[3], localVars[3];
	// the vectors containing the 3D size of the kernel
	const ExpressionPtr globalSize, localSize;

	NodeMap replacements;

	/*
	 * checks if the passed variable is one of the 6 loop induction variables
	 * @param
	 * var the variable to be checked
	 * @return
	 * true if the passed variable is one of the loop induction variables, false otherwise
	 */
	bool isInductionVar(ExpressionPtr& var) {
		if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(var)) {
			var = cast->getSubExpression();
			return isInductionVar(var);
		}

		for(size_t i = 0; i < 3; ++i) {
			if(*var == *globalVars[0])
				return true;
			if(*var == *localVars[0])
				return true;
		}
		return false;
	}

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

	const NodePtr resolveElement(const NodePtr& ptr) {
	    // stopp recursion at type level
	    if (ptr->getNodeCategory() == core::NodeCategory::NC_Type) {
	        return ptr;
	    }

		if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(ptr)) {
			const ExpressionPtr fun = call->getFunctionExpr();

			// remove parallel-job construct
			if(BASIC.isParallel(fun)){
				const JobExprPtr job = static_pointer_cast<const JobExpr>(call->getArgument(0));

				BindExprPtr bind = static_pointer_cast<const core::BindExpr>(job->getDefaultExpr());
				LambdaExprPtr bindLambda = static_pointer_cast<const core::LambdaExpr>(bind->getCall()->getFunctionExpr());

				//use only the first statement = parallel of the outer parallel
				if(const CallExprPtr innerCall = dynamic_pointer_cast<const CallExpr>(bindLambda->getBody()->getStatement(0))){
					if(BASIC.isParallel(innerCall->getFunctionExpr())) {
						return resolveElement(bindLambda->getBody()->getStatement(0));
					}
				}

				return bindLambda->getBody()->substitute(mgr, *this);
			}

			// remove mergeAll
			if(BASIC.isMergeAll(fun))
				return builder.getNoOp();
			// translate return statements to breaks

			// replace calls to get_*_id with accesses to the appropriate loop variable
			if(isGetGlobalID(fun)){
//std::cout << "\nGlobalID " << call << std::endl;
				size_t dim = extractIndexFromArg(call);
				globalDim = std::max(dim, globalDim);
				return globalVars[dim];
			}
			if(isGetLocalID(fun)) {
//std::cout << "\nLocalID " << call << std::endl;
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

		if(const TypePtr type = dynamic_pointer_cast<const Type>(ptr))
			std::cout << "type " << *type << std::endl;

		// replace variable with loop induction variable if semantically correct
		if(const VariablePtr var = dynamic_pointer_cast<const Variable>(ptr)) {
//			std::cout << "Variable: " << *var << " " << replacements.size() << std::endl;
			if(replacements.find(var) != replacements.end()){
				VariablePtr replacement =  static_pointer_cast<const Variable>(replacements[var]);
				if(*replacement->getType() == *var->getType())
					return replacement;

				// add a cast expression to the type of the node we are replacing
				return builder.castExpr(var->getType(), replacement);
			}
		}

		NodePtr sub = ptr;

		// try to replace varariables with loop-induction variables whereever possible
		if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(sub)) {
			if(BASIC.isRefAssign(call->getFunctionExpr())) {
				ExpressionPtr rhs = call->getArgument(1)->substitute(mgr, *this);
				if(isInductionVar(rhs)) {// an induction variable is assigned to another variable. Use the induction variable instead
					replacements[call->getArgument(0)] = rhs;
					return builder.getNoOp();
				}
			}
		}
		if(const DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(sub)) {
			ExpressionPtr init = decl->getInitialization()->substitute(mgr, *this);

			// use of variable as argument of ref.new or ref.var
			if(const CallExprPtr initCall = dynamic_pointer_cast<const CallExpr>(init))
				if(BASIC.isRefNew(initCall->getFunctionExpr()) || BASIC.isRefVar(initCall->getFunctionExpr()))
					init = initCall->getArgument(0);

			// remove cast
			while(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(init))
				init = cast->getSubExpression();

			std::cout << "INIT: " << init << std::endl;
			// plain use of variable as initialization
			if(isInductionVar(init)) {
				replacements[decl->getVariable()] = init;
				return builder.getNoOp();
			}
		}

		return sub->substitute(mgr, *this);
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

	/*
	 * returns the replacements for variables which can be replaced with loop induction variables
	 */
	NodeMap& getReplacements() { return replacements; }
};

} // end anonymous namespace

StatementPtr KernelPoly::transformKernelToLoopnest(ExpressionAddress kernel){
	IRBuilder builder(program->getNodeManager());

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

    // replace returns at the first level with breaks
	NodeMapping* h;
	auto mapper = makeLambdaMapper([&](unsigned index, const NodePtr& element)->NodePtr{
		if(element->getNodeType() == NT_ReturnStmt){
			return builder.breakStmt();
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

	for_each(loopNests, [&](StatementPtr& nest) {
		std::cout << analysis::scop::mark(nest) << std::endl;
	});
}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme
    
