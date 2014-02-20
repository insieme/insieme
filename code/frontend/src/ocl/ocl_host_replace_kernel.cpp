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
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/ocl/ocl_host_replace_kernel.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {

}

KernelReplacer::KernelReplacer(core::NodePtr prog) : prog(prog){
	findKernelNames();
	collectArguments();

//	std::cout << printer::PrettyPrinter(this->prog) << std::endl;
}

void KernelReplacer::findKernelNames() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	NodeAddress pA(prog);

	TreePatternPtr nameLiteral = pattern::var("kernel_name", pattern::any);
	TreePatternPtr mayEncapsulatedNameLiteral = irp::callExpr(pattern::any, pattern::any, *pattern::any << nameLiteral << *pattern::any) | nameLiteral;
	TreePatternPtr clCreateKernel = irp::callExpr(pattern::any, irp::literal("clCreateKernel"),	pattern::any <<
			mayEncapsulatedNameLiteral << pattern::var("err", pattern::any) );
	TreePatternPtr createKernelPattern = pattern::var("clCreateKernel", irp::callExpr(pattern::any, irp::atom(mgr.getLangBasic().getRefAssign()),
			pattern::var("kernel", pattern::any) << clCreateKernel));

	irp::matchAllPairs(createKernelPattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createKernel) {
//std::cout << "kernel: " << *createKernel["clCreateKernel"].getValue() << std::endl;

		std::string kernelName = createKernel["kernel_name"].getValue().as<LiteralPtr>()->getStringValue();
		ExpressionAddress kernelExpr = matchAddress >> createKernel["kernel"].getValue().as<ExpressionAddress>();
		ExpressionAddress kernelVar = utils::extractVariable(kernelExpr).as<ExpressionAddress>();

		kernelVar = utils::getRootVariable(kernelVar).as<ExpressionAddress>();

//std::cout << "varAddr: " << kernelVar << " - " << *kernelVar << std::endl;
		kernelNames[kernelName] = kernelVar;

		// remove the clCreateKernel call including the assignment and its lhs
		prog = transform::replaceAll(mgr, prog, NodePtr(createKernel["clCreateKernel"].getValue()), builder.getNoOp(), false);
	});
}


void KernelReplacer::collectArguments() {
	NodeManager& mgr = prog->getNodeManager();
	const lang::BasicGenerator&	gen = mgr.getLangBasic();
	IRBuilder builder(mgr);

	NodeAddress pA(prog);
	NodeMap replacements;

	TreePatternPtr localOrGlobalVar = node(node(node(single(aT(irp::genericType("_cl_kernel"))) << single(pattern::any))
			<< single(pattern::any)) << single(pattern::any));

	TreePatternPtr clSetKernelArg = var("clSetKernelArg", irp::callExpr(pattern::any, irp::literal("clSetKernelArg"),
			(aT(pattern::var("kernel", localOrGlobalVar))) << pattern::var("arg_index", irp::literal())
			<< pattern::var("arg_size", pattern::any) << pattern::var("arg_value", pattern::any) ));


	irp::matchAllPairs(clSetKernelArg, pA, [&](const NodeAddress& matchAddress, const AddressMatch& setArg) {
//		std::cout << "Kernel: " << setArg["kernel"].getValue() << "\n\tidx: " << *setArg["arg_index"].getValue() << " val "
//				<< *getVarOutOfCrazyInspireConstruct1(setArg["arg_value"].getValue().as<ExpressionPtr>(), IRBuilder(mgr)) << std::endl;

		ExpressionAddress kernel = utils::getRootVariable(matchAddress >> setArg["kernel"].getValue().as<ExpressionAddress>()).as<ExpressionAddress>();
		LiteralPtr idx = setArg["arg_index"].getValue().as<LiteralPtr>();
		ExpressionPtr arg = setArg["arg_value"].getValue().as<ExpressionPtr>();
		ExpressionPtr sizeArg = setArg["arg_size"].getValue().as<ExpressionPtr>();

		int argIdx = insieme::utils::numeric_cast<int>(idx->getStringValue());

		// reserve memory
		for(int i = kernelTypes[kernel].size(); i <= argIdx; ++i)
			kernelTypes[kernel].push_back(TypePtr());

		VariablePtr tuple = builder.variable(kernel->getType());

		VariablePtr src = builder.variable(arg->getType());
		VariableList params;
		params.push_back(tuple);
		StatementList body;
		ExpressionPtr idxArg = (gen.isUInt8(idx->getType()) ? idx.as<ExpressionPtr>() :	builder.castExpr(gen.getUInt8(), idx));

		if(utils::isNullPtr(arg)) {
			// in this case arg is a local variable which has to be declared in host code
			// need to read size parameter
			ExpressionPtr size;
			TypePtr type;
			ExpressionPtr hostPtr;
			utils::extractSizeFromSizeof(sizeArg, size, type);
			assert(size && "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.");

			//collect types of the arguments
			kernelTypes[kernel].at(argIdx) = (type);


			// refresh variables used in the generation of the local variable
			VariableMap varMapping;
			utils::refreshVariables(size, varMapping, builder);

			ExpressionPtr tupleAccess = builder.callExpr(gen.getTupleRefElem(), tuple, idxArg, builder.getTypeLiteral(type));
			ExpressionPtr allocMemory = builder.refVar(builder.callExpr(type, gen.getArrayCreate1D(), builder.getTypeLiteral(type), size));

			body.push_back(builder.callExpr(gen.getUnit(), gen.getRefAssign(), tupleAccess, allocMemory));
			body.push_back(builder.returnStmt(builder.intLit(0)));

			//ßßß		kernelArgs[kernel].push_back(builder.getTypeLiteral((builder.refType(type))));

			if(varMapping.empty()) { // no variable from outside needed
				FunctionTypePtr fTy = builder.functionType(kernel->getType(), gen.getInt4());
				LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));
				replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, kernel);
				return;
			}

			TypeList argTys;
			ExpressionList args;
			args.push_back(kernel);
			argTys.push_back(kernel->getType());
			// add the needed variables
			for_each(varMapping, [&](std::pair<VariablePtr, VariablePtr> varMap) {
				args.push_back(varMap.first);
				argTys.push_back(varMap.first->getType());
				params.push_back(varMap.second);
			});

			FunctionTypePtr fTy = builder.functionType(argTys, gen.getInt4());
			LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));
			replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, args);


			// initialize local memory place with undefined
	/*		return builder.callExpr(gen.getUnit(), gen.getRefAssign(), builder.callExpr(builder.refType(type), gen.getTupleRefElem(), kernel,
					(gen.isUInt8(idx) ? idx :	builder.castExpr(gen.getUInt8(), idx)),
					builder.getTypeLiteral(type)), builder.refVar(builder.callExpr(type, gen.getArrayCreate1D(), builder.getTypeLiteral(type), size)));
	*/
			return;
		}

		//collect types of the arguments
		kernelTypes[kernel].at(argIdx) = (arg->getType());

		arg = utils::getVarOutOfCrazyInspireConstruct(arg);

	//	kernelArgs[kernel] = builder.variable(builder.tupleType(argTypes));
	//ßßß	kernelArgs[kernel].push_back(arg);
	std::cout << "ARGUMENT: \t" << *arg << std::endl;

		FunctionTypePtr fTy = builder.functionType(toVector(kernel->getType().as<TypePtr>(), utils::tryDeref(arg)->getType()), gen.getInt4());
		params.push_back(src);

		body.push_back(builder.assign( builder.callExpr(gen.getTupleRefElem(), tuple, idxArg,
										builder.getTypeLiteral(src->getType())), src));
		body.push_back(builder.returnStmt(builder.intLit(0)));
		LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));

		// store argument in a tuple
		replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, kernel, utils::tryDeref(arg));


for_each(kernelTypes[kernel], [](NodePtr doll) {
	std::cout << "new arg: " << (doll) << std::endl;
});
	});

	// perform the replacements
	prog = transform::replaceAll(mgr, prog, replacements, false);
}

} //namespace ocl
} //namespace frontend
} //namespace insieme

