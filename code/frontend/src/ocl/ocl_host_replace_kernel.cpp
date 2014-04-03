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
#include <fstream>


#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/frontend/ocl/ocl_host_replace_kernel.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"
#include "insieme/frontend/extensions/ocl_kernel_extension.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
bool KernelCodeRetriver::saveString(const core::LiteralPtr& lit) {
	path = lit->getStringValue();
	return true;
}

bool isPtrDecay(const core::ExpressionPtr& lit){
	auto& gen = lit.getNodeManager().getLangBasic();
	return (gen.isRefVectorToRefArray(lit) || gen.isRefVectorToSrcArray(lit) );
}


bool KernelCodeRetriver::saveString(const core::CallExprPtr& call) {
	if (const LiteralPtr lit = dynamic_pointer_cast<const Literal>(call->getFunctionExpr())) {
		if (isPtrDecay(lit)) {
			if (const LiteralPtr pl = dynamic_pointer_cast<const Literal>(utils::tryRemove(gen.getRefDeref(), call->getArgument(0)))) {
				path = pl->getStringValue();
				return true;
			}
		}

		if(lit->getStringValue() == "shrFindFilePath") {

			const ExpressionPtr arg = utils::tryRemove(gen.getRefDeref(), call->getArgument(0));
			if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(arg))
				return saveString(call);
			if(const LiteralPtr lit = dynamic_pointer_cast<const Literal>(arg))
				return saveString(lit);
			if(const VariablePtr var = dynamic_pointer_cast<const Variable>(arg)){
				KernelCodeRetriver nkcr(var, call, program);
				visitDepthFirstInterruptible(program, nkcr);
				string pathFound = nkcr.getKernelFilePath();
				if(pathFound.size() > 0) {
					path = pathFound;
					return true;
				}
			}
		}
	}
	return false;
}

bool KernelCodeRetriver::visitNode(const core::NodePtr& node) {
	if (node == breakingStmt) {
		return true; // stop recursion
	}
	return false; // go on with search
}

bool KernelCodeRetriver::visitCallExpr(const core::CallExprPtr& callExpr) {
	if (callExpr->getFunctionExpr() != gen.getRefAssign())
		return false;
	// check if it is the variable we are looking for
	if (const VariablePtr pathVar = dynamic_pointer_cast<const Variable>(pathToKernelFile)) {
		if (const VariablePtr lhs = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0))) {
			if (lhs->getId() != pathVar->getId())
				return false;
		}

		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
			return saveString(rhs);
		}
	}

	if(callExpr->getArgument(0) == pathToKernelFile) {
		if (const CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(1))) {
			return saveString(rhs);
		}

	}

	return false;
}

bool KernelCodeRetriver::visitDeclarationStmt(const core::DeclarationStmtPtr& decl) {
	if (const VariablePtr& pathVar = dynamic_pointer_cast<const Variable>(pathToKernelFile)) {
		if (decl->getVariable()->getId() != pathVar->getId())
			return false;

		if (const CallExprPtr initCall = dynamic_pointer_cast<const CallExpr>(utils::tryRemoveAlloc(decl->getInitialization()))) {
			return saveString(initCall);
		}
	}
	return false;
}

const ProgramPtr loadKernelsFromFile(string path, const IRBuilder& builder, const std::vector<boost::filesystem::path>& includeDirs) {
	// delete quotation marks form path
	if (path[0] == '"')
		path = path.substr(1, path.length() - 2);
//std::cout << "Path: " << path << std::endl;
	std::ifstream check;
			string root = path;
	size_t nIncludes = includeDirs.size();
	// try relative path first
	check.open(path);
	// if not found now, check the include directories
	for (size_t i = 0; i < nIncludes && check.fail(); ++i) {
		check.close();
		// try with include paths
		path = (includeDirs.at(i) / root).string();
		check.open(path);
	}
	// if there is still no match, try the paths of the input files
/*	no job, no current file :/
	if (check.fail()) {
		assert(job.getFiles().size() == 1u);
		string ifp = job.getFiles()[0].string();
		size_t slash = ifp.find_last_of("/");
		path = ifp.substr(0u, slash + 1) + root;
		check.open(path);
	}
*/
	check.close();

	if (check.fail()) {// no kernel file found, leave the error printing to the compiler frontend
			std::cerr << "FAIL! " << path << std::endl;
		path = root;
	}

	LOG(INFO) << "Converting kernel file '" << path << "' to IR...";
	ConversionJob kernelJob(path, includeDirs);
	kernelJob.registerFrontendPlugin<extensions::OclKernelPlugin>();
	kernelJob.setDefinition("INSIEME", "");

//	kernelJob.setFiles(toVector<frontend::path>(path));
	return kernelJob.execute(builder.getNodeManager(), false);
}

/* Assumptions:
 * 1. the work dimension is a scalar in the arguments
 * 2. The cast to void* of the local/global size happens in the argument
*/
const ExpressionPtr anythingToVec3(ExpressionPtr workDim, ExpressionPtr size) {
	NodeManager& mgr = workDim->getNodeManager();
	IRBuilder builder(mgr);
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	const TypePtr vecTy = builder.vectorType(gen.getUInt8(), builder.concreteIntTypeParam(static_cast<size_t>(3)));
	TypePtr argTy;
	VariablePtr param;
	ExpressionPtr arg;
	unsigned int wd;

	if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(workDim)) {
		workDim = cast->getSubExpression();
	}

	if(const CallExprPtr call = dynamic_pointer_cast<const CallExprPtr>(workDim)) {
		if (gen.isScalarCast(call->getFunctionExpr()))
			workDim = call[0];
	}

	dumpDetail(workDim);

	// check work dimension
	const LiteralPtr dim = dynamic_pointer_cast<const Literal>(workDim);
	assert(dim && "Cannot determine work_dim of clEnqueueNDRangeKernel. Should be a literal!");
	wd = dim->getValueAs<unsigned int>();
	//    std::cout << "*****************WorkDim: " << dim->getValue() << std::endl;
	assert(wd < 3u && "Invalid work_dim. Should be 1 - 3!");

	// check if there is a x to array called
	if(const CallExprPtr toArray = dynamic_pointer_cast<const CallExpr>(size)) {
		if(toArray->getFunctionExpr() == gen.getScalarToArray()) {
			// check consitency with workDim, should be 1
			assert(wd == 1 && "Scalar group size passed to a multi dimensional work_dim");

			argTy = toArray->getArgument(0)->getType();
			param = builder.variable(argTy);
			arg = toArray->getArgument(0);
		} else if(isPtrDecay(toArray->getFunctionExpr())) {
			arg = toArray->getArgument(0);
			argTy = arg->getType();
			param = builder.variable(argTy);

		} else if(gen.isRefReinterpret(toArray->getFunctionExpr())){

			arg   = toArray[0];
			argTy = arg->getType();
			param = builder.variable(argTy);

// LUIS: all this code is dead and dangerous
//		} else if(toArray->getFunctionExpr() == gen.getRefVar() ) {
//			if(const CallExprPtr vta = dynamic_pointer_cast<const CallExpr>(toArray->getArgument(0))) {
//	// throw away ref.var
//	// TODO only a dirty fix, check it
//	// this will no longer happen, vector-to-array is gone
////				if(vta->getFunctionExpr() == gen.getVectorToArray()) {
////					arg = vta->getArgument(0);
////					argTy = arg->getType();
////					param = builder.variable(argTy);
////				}
//			}
		} else {
			std::cerr << "Unexpected Function: " << toArray << " of type " << toArray->getArgument(0)->getType() << std::endl;
			assert(false && "Unexpected function in OpenCL size argument");
		}
	} else { // the argument is an array
		size = utils::removeDoubleRef(size);
		assert(size->getType()->getNodeType() == NT_RefType && "Called clEnqueueNDRangeKernel with invalid group argument");
		argTy = size->getType();
		param = builder.variable(argTy);
		arg = size;
	}

	ExpressionPtr init = utils::removeDoubleRef(param);

	bool isArray = builder.matchType("ref<array<'a, 1>>", init->getType());
	isArray |= builder.matchType("ref<vector<'a, #n>>", init->getType());
	isArray |= builder.matchType("array<'a, 1>", init->getType());
	isArray |= builder.matchType("vector<'a, #n>", init->getType());

	DeclarationStmtPtr vDecl;
	if(wd == 1) {
		if(isArray)
			init = builder.arrayAccess(init, builder.literal(gen.getUInt8(), "0"));
		if(init->getType() != gen.getUInt8()) {
			init = builder.castExpr(gen.getUInt8(), utils::tryDeref(init));
		}
		vDecl = builder.declarationStmt(vecTy,
				builder.vectorExpr(toVector<ExpressionPtr>(init, builder.literal(gen.getUInt8(), "1"), builder.literal(gen.getUInt8(), "1"))));
	} else {
		assert(isArray && "Size argument of multidimensional group is no vector or array");

		vector<ExpressionPtr> subscripts;
		subscripts.push_back(builder.arrayAccess(init, builder.literal(gen.getUInt8(), "0")));
		subscripts.push_back(builder.arrayAccess(init, builder.literal(gen.getUInt8(), "1")));
		subscripts.push_back(wd == 3 ? (ExpressionPtr)builder.arrayAccess(init, builder.literal(gen.getUInt8(), "2")) :
				(ExpressionPtr)builder.literal(gen.getUInt8(), "1"));

		for_each(subscripts, [&](ExpressionPtr& r) {
					if(r->getType() != gen.getUInt8())
					r = builder.castExpr(gen.getUInt8(), utils::tryDeref(r));
				});

		vDecl = builder.declarationStmt(vecTy, builder.vectorExpr(subscripts));
	}

	FunctionTypePtr fctTy = builder.functionType(toVector(argTy), vecTy);

	return builder.callExpr(vecTy, builder.lambdaExpr(fctTy, toVector(param) , builder.compoundStmt(vDecl,
							builder.returnStmt(vDecl->getVariable()))), arg);
}
}

KernelReplacer::KernelReplacer(core::NodePtr prog, const std::vector<boost::filesystem::path>& includeDirs) : prog(prog), includeDirs(includeDirs){ }

ExpressionPtr KernelReplacer::handleArgument(const TypePtr& argTy, const TypePtr& memberTy, const ExpressionPtr& tupleMemberAccess, StatementList& body) {
	NodeManager& mgr = argTy->getNodeManager();
	IRBuilder builder(mgr);
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	ExpressionPtr argument;
	// check for local memory arguments
	if(*memberTy == *gen.getUInt8()) {
		VariablePtr localMemArg = builder.variable(argTy);
		TypePtr elemTy = argTy.as<RefTypePtr>()->getElementType().as<ArrayTypePtr>()->getElementType();
		ExpressionPtr lmSize = builder.div(tupleMemberAccess, builder.callExpr(gen.getSizeof(), builder.getTypeLiteral(elemTy)));
		DeclarationStmtPtr localMemDecl = builder.declarationStmt(localMemArg, builder.refVar(builder.callExpr(gen.getArrayCreate1D(),
				builder.getTypeLiteral(elemTy), lmSize)));
		body.push_back(localMemDecl);

		argument = localMemArg;
	} else {
		argument = builder.callExpr(gen.getArrayRefElem1D(), tupleMemberAccess, builder.castExpr(gen.getUInt8(), builder.intLit(0)));

		if(utils::tryDeref(argument)->getType() != argTy) {// e.g. argument of kernel is an ocl vector type

			argument = builder.callExpr(argTy, gen.getRefReinterpret(), argument, builder.getTypeLiteral(argTy));
		}
		argument = utils::tryDeref(argument);
	}

	return argument;
}

ExpressionPtr KernelReplacer::createKernelCallLambda(const ExpressionAddress localKernel,
		const ExpressionPtr work_dim, const ExpressionPtr local_work_size, const ExpressionPtr global_work_size) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	ExpressionPtr k = utils::getRootVariable(localKernel).as<ExpressionPtr>();

	// try to find coresponding kernel function
	assert(kernelFunctions.find(k) != kernelFunctions.end() && "Cannot find OpenCL Kernel");
	const ExpressionPtr local = anythingToVec3(work_dim, local_work_size);
	const ExpressionPtr global = anythingToVec3(work_dim, global_work_size);

//	const ExpressionPtr kernelFun = kernelFunctions[k];

//		dumpPretty(kernelFun);
//for_each(kernelTypes[k], [&](TypePtr ty) {
//	std::cout << "->\t" << *ty << std::endl;
//});

	LambdaExprPtr lambda = kernelFunctions[k].as<LambdaExprPtr>();

	/*    assert(kernelArgs.find(k) != kernelArgs.end() && "No arguments for call to kernel function found");
	const VariablePtr& args = kernelArgs[k];
	const TupleTypePtr& argTypes = dynamic_pointer_cast<const TupleType>(args->getType());*/
	const VariableList& interface = lambda->getParameterList()->getElements();

	vector<ExpressionPtr> innerArgs;
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	// construct call to kernel function
//		if(localMemDecls.find(k) == localMemDecls.end() || localMemDecls[k].size() == 0) {
//std::cout << "lmd " << localMemDecls[k] << std::endl;
	TypeList kernelType = kernelTypes[k];

	/* body of a newly created function which replaces clNDRangeKernel. It contains
	 *  Declarations of the local memory arguments
	 *  the kernel call
	 *  return 0;
	 */
	StatementList body;

	// Kernel variable to be used inside the newly created function
	VariablePtr innerKernel = builder.variable(builder.tupleType(kernelType));
	for(size_t i = 0; i < interface.size() -2 /*argTypes->getElementTypes().size()*/; ++i) {
//??			TypePtr argTy = utils::vectorArrayTypeToScalarArrayType(interface.at(i)->getType(), builder);
		TypePtr argTy = interface.at(i)->getType();
		TypePtr memberTy = kernelType.at(i);
		ExpressionPtr tupleMemberAccess = builder.callExpr(memberTy, gen.getTupleMemberAccess(), utils::removeDoubleRef(innerKernel),
				builder.literal(gen.getUInt8(), toString(i)), builder.getTypeLiteral(memberTy));

		ExpressionPtr argument = handleArgument(argTy, memberTy, tupleMemberAccess, body);

		innerArgs.push_back(argument);
	}

	const TypePtr vecTy = builder.vectorType(gen.getUInt8(), builder.concreteIntTypeParam(static_cast<size_t>(3)));

	// local and global size to be used inside the newly created function
	VariablePtr innerGlobal = builder.variable(vecTy);
	VariablePtr innerLocal = builder.variable(vecTy);

	// add global and local size to arguments
	innerArgs.push_back(innerGlobal);
	innerArgs.push_back(innerLocal);

	ExpressionPtr kernelCall = builder.callExpr(gen.getInt4(), lambda, innerArgs);
	body.push_back(kernelCall);							   // calling the kernel function
	body.push_back(builder.returnStmt(builder.intLit(0))); // return CL_SUCCESS

	// create function type for inner function: kernel tuple, global size, local size
	TypeList innerFctInterface;
	innerFctInterface.push_back(innerKernel->getType());
	innerFctInterface.push_back(vecTy);
	innerFctInterface.push_back(vecTy);

	FunctionTypePtr innerFctTy = builder.functionType(innerFctInterface, gen.getInt4());

	// collect inner function parameters
	VariableList innerFctParams;
	innerFctParams.push_back(innerKernel);
	innerFctParams.push_back(innerGlobal);
	innerFctParams.push_back(innerLocal);

	// create lambda for inner function
	LambdaExprPtr innerLambda = builder.lambdaExpr(innerFctTy, builder.parameters(innerFctParams), builder.compoundStmt(body));

	return builder.callExpr(gen.getInt4(), innerLambda, localKernel, global, local);
}

std::vector<std::string> KernelReplacer::findKernelNames(TreePatternPtr clCreateKernel) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	NodeAddress pA(prog);
	const lang::BasicGenerator& gen = mgr.getLangBasic();

	// vector to store filenames in case of icl_lib compilation
	std::vector<std::string> kernelFileNames;

	TreePatternPtr kernelDecl = irp::declarationStmt(pattern::var("kernel", pattern::any),
			irp::callExpr(pattern::any, irp::atom(gen.getRefVar()), pattern::single(clCreateKernel)));
	TreePatternPtr kernelAssign = var("assignment", irp::callExpr(pattern::any, irp::atom(gen.getRefAssign()),
			pattern::var("kernel", pattern::any) << clCreateKernel));
	TreePatternPtr createKernelPattern = kernelDecl | kernelAssign;

	irp::matchAllPairs(createKernelPattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createKernel) {
		core::ExpressionPtr kernelNameExpr = createKernel["kernel_name"].getValue().as<core::ExpressionPtr>();

		std::string kernelName = utils::extractQuotedString(kernelNameExpr);

		ExpressionAddress kernelExpr = matchAddress >> createKernel["kernel"].getValue().as<ExpressionAddress>();
		ExpressionAddress kernelVar = utils::extractVariable(kernelExpr).as<ExpressionAddress>();

		kernelVar = utils::getRootVariable(kernelVar).as<ExpressionAddress>();

		kernelNames[kernelName] = kernelVar;
		NodePtr createKernelExpr = createKernel.getRoot();

		// remove the clCreateKernel call including the assignment and its lhs, not the declaration
		if(createKernel.isVarBound("assignment")) prog = transform::replaceAll(mgr, prog, createKernelExpr, (builder.getNoOp()), false);

		if(createKernel.isVarBound("file_name")) kernelFileNames.push_back(utils::extractQuotedString(createKernel["file_name"].getValue()));
	});

	return kernelFileNames;
}


void KernelReplacer::collectArguments() {
	NodeManager& mgr = prog->getNodeManager();
	const lang::BasicGenerator& gen = mgr.getLangBasic();
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
		ExpressionPtr arg = utils::tryRemove(gen.getRefReinterpret(), setArg["arg_value"].getValue().as<ExpressionPtr>());
		ExpressionPtr sizeArg = setArg["arg_size"].getValue().as<ExpressionPtr>();

		unsigned int argIdx = insieme::utils::numeric_cast<unsigned int>(idx->getStringValue());

		// reserve memory
		for(unsigned int i = kernelTypes[kernel].size(); i <= argIdx; ++i)
			kernelTypes[kernel].push_back(TypePtr());

		VariablePtr tuple = builder.variable(kernel->getType());

		VariablePtr src = builder.variable(arg->getType());
		VariableList params;
		params.push_back(tuple);
		StatementList body;
		ExpressionPtr idxArg = (gen.isUInt8(idx->getType()) ? idx.as<ExpressionPtr>() :	builder.castExpr(gen.getUInt8(), idx));

		if(frontend::utils::isNullPtrExpression(arg)) {
			// in this case arg is a local variable which has to be declared in host code
			// need to read size parameter
/*+			ExpressionPtr size;
			TypePtr type;
			ExpressionPtr hostPtr;
			utils::extractSizeFromSizeof(sizeArg, size, type);
			assert(size && "Unable to deduce type from clSetKernelArg call when allocating local memory: No sizeof call found, cannot translate to INSPIRE.");
not needed since we store the entire sizeArg to avoid confusions with different types
+*/
			// add ref<array< >> to type
//+			TypePtr arrType = builder.refType(builder.arrayType(type));

			//collect types of the arguments
//+			kernelTypes[kernel].at(argIdx) = (arrType);
			// only store size of local mem arrays, therefore it's always uint8
			kernelTypes[kernel].at(argIdx) = gen.getUInt8();
			// store the information that this is a local memory argument
			localMemArgs[kernel].insert(argIdx);


			// refresh variables used in the generation of the local variable
			VariableMap varMapping;
//+			utils::refreshVariables(size, varMapping, builder);
			utils::refreshVariables(sizeArg, varMapping, builder);

//+			ExpressionPtr tupleAccess = builder.callExpr(gen.getTupleRefElem(), tuple, idxArg, builder.getTypeLiteral(arrType));
			ExpressionPtr tupleAccess = builder.callExpr(gen.getTupleRefElem(), tuple, idxArg, builder.getTypeLiteral(gen.getUInt8()));
//+			ExpressionPtr allocMemory = builder.refVar(builder.callExpr(arrType, gen.getArrayCreate1D(), builder.getTypeLiteral(type), size));

//+			body.push_back(builder.callExpr(gen.getUnit(), gen.getRefAssign(), tupleAccess, allocMemory));
			body.push_back(builder.callExpr(gen.getUnit(), gen.getRefAssign(), tupleAccess, sizeArg));
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

//	std::cout << "ARGUMENT: \t" << *arg->getType() << "  " << *arg << std::endl;
		//collect types of the arguments
		kernelTypes[kernel].at(argIdx) = (arg->getType());

//		arg = utils::getVarOutOfCrazyInspireConstruct(arg);

	//	kernelArgs[kernel] = builder.variable(builder.tupleType(argTypes));
	//ßßß	kernelArgs[kernel].push_back(arg);

		FunctionTypePtr fTy = builder.functionType(toVector(kernel->getType().as<TypePtr>(), (arg)->getType()), gen.getInt4());
		params.push_back(src);

		body.push_back(builder.assign( builder.callExpr(gen.getTupleRefElem(), tuple, idxArg, builder.getTypeLiteral(src->getType())), src));
		body.push_back(builder.returnStmt(builder.intLit(0)));
		LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));

		// store argument in a tuple
		replacements[setArg["clSetKernelArg"].getValue()] = builder.callExpr(gen.getInt4(), function, kernel, arg);

/*
		dumpPretty(replacements[setArg["clSetKernelArg"].getValue()]);
for_each(kernelTypes[kernel], [](NodePtr doll) {
	std::cout << "new arg: " << (doll) << std::endl;
});
*/
	});

	// perform the replacements
	prog = transform::replaceAll(mgr, prog, replacements, false);
}

void KernelReplacer::replaceKernels() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	// generate tuple variables for each kernel and replace kernel variables
	ExpressionMap kernelReplacements;

	for_each(kernelTypes, [&](std::pair<ExpressionPtr, std::vector<TypePtr> > kT) {
		ExpressionPtr k = kT.first;
		TupleTypePtr tt = builder.tupleType(kT.second);

		ExpressionPtr replacement = k.isa<VariablePtr>() ? builder.variable(builder.refType(tt)).as<ExpressionPtr>()
				: builder.literal(builder.refType(tt), k.as<LiteralPtr>()->getStringValue());

		kernelReplacements[k] = replacement;
	});

	prog = transform::replaceVarsRecursiveGen(mgr, prog, kernelReplacements);
}

void KernelReplacer::storeKernelLambdas(std::vector<ExpressionPtr>& kernelEntries, std::map<string, int>& checkDuplicates) {
			for_each(kernelEntries, [&](ExpressionPtr entryPoint) {
			if(const LambdaExprPtr lambdaEx = dynamic_pointer_cast<const LambdaExpr>(entryPoint)) {
                std::string cname = insieme::core::annotations::getAttachedName(lambdaEx);
                assert(!cname.empty() && "cannot find the name of the kernel function");
				assert(checkDuplicates[cname] == 0 && "Multiple kernels with the same name not supported");
				checkDuplicates[cname] = 1;
				kernelFunctions[kernelNames[cname]] = lambdaEx;
			}

		});
}

void KernelReplacer::loadKernelCode(core::pattern::TreePatternPtr createKernel) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	NodeAddress pA(prog);

	findKernelNames(createKernel);

	TreePatternPtr clCreateProgramWithSource = var("cpws", irp::callExpr(irp::atom(builder.refType(builder.arrayType(builder.genericType("_cl_program")))),
			irp::literal("clCreateProgramWithSource"), pattern::any <<
			pattern::any << var("kernelCodeString", pattern::any) << pattern::any << pattern::var("err", pattern::any) ));
	TreePatternPtr kernelAssign = irp::callExpr(irp::atom(mgr.getLangBasic().getRefAssign()),
			var("kernelVar", pattern::any), (clCreateProgramWithSource));
	TreePatternPtr kernelDecl = irp::declarationStmt(aT(var("kernelVar", pattern::any)) ,aT(clCreateProgramWithSource));
	TreePatternPtr clCreateProgramWithSourcePattern = kernelAssign | kernelDecl;

	std::map<string, int> checkDuplicates;
	irp::matchAllPairs(clCreateProgramWithSourcePattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createProgram) {

		std::vector<ExpressionPtr> kernelEntries = lookForKernelFilePragma(createProgram["kernelVar"].getValue().as<ExpressionPtr>()->getType(),
				createProgram["cpws"].getValue().as<ExpressionPtr>());

		storeKernelLambdas(kernelEntries, checkDuplicates);
	});
}

void KernelReplacer::inlineKernelCode() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	NodeAddress pA(prog);

	NodeMap replacements;
	// Replace clEnqueueNDRangeKernel calls with calls to the associated function
	TreePatternPtr clEnqueueNDRangeKernel = var("enrk", irp::callExpr(pattern::any, irp::literal("clEnqueueNDRangeKernel"),	pattern::any <<
			var("kernel", pattern::any) << var("work_dim", pattern::any) << pattern::var("global_work_offset", pattern::any) <<
			pattern::var("global_work_size", pattern::any) << pattern::var("local_work_size", pattern::any) <<
			pattern::any << pattern::any << pattern::var("err", pattern::any) ));

	irp::matchAllPairs(clEnqueueNDRangeKernel, pA, [&](const NodeAddress& matchAddress, const AddressMatch& ndrangeKernel) {

		ExpressionAddress localKernel = (matchAddress >> ndrangeKernel["kernel"].getValue()).as<ExpressionAddress>();

		ExpressionPtr newKernelCall = createKernelCallLambda(localKernel, ndrangeKernel["work_dim"].getValue().as<ExpressionPtr>(),
				ndrangeKernel["local_work_size"].getValue().as<ExpressionPtr>(), ndrangeKernel["global_work_size"].getValue().as<ExpressionPtr>());


		transform::utils::migrateAnnotations(ndrangeKernel["enrk"].getValue().as<NodePtr>(), newKernelCall.as<NodePtr>());

		replacements[ndrangeKernel["enrk"].getValue()] = newKernelCall;


//	std::cout << "\nreplacing " << printer::PrettyPrinter(ndrangeKernel["enrk"].getValue()) << "\n\twith " << printer::PrettyPrinter(kernelCall);
	});

	prog = transform::replaceAll(mgr, prog, replacements, false);

}

ProgramPtr KernelReplacer::findKernelsUsingPathString(const ExpressionPtr& path, const ExpressionPtr& root, const ProgramPtr& mProgram) {
	std::set<string> kernelFileCache;
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen(mgr);
	ConversionJob job;
	ProgramPtr kernels;

	if(const CallExprPtr callSaC = dynamic_pointer_cast<const CallExpr>(path)) {
		if(const LiteralPtr stringAsChar = dynamic_pointer_cast<const Literal>(callSaC->getFunctionExpr())) {
			if(isPtrDecay(stringAsChar)) {
				if(const LiteralPtr path = dynamic_pointer_cast<const Literal>(callSaC->getArgument(0))) {
					// check if file has already been added
//std::cout << "\n using path string " << path->getStringValue() << " \n\n";
					if(kernelFileCache.find(path->getStringValue()) == kernelFileCache.end()) {
						kernelFileCache.insert(path->getStringValue());
						kernels = loadKernelsFromFile(path->getStringValue(), builder, includeDirs);
					}
					//return;
// set source string to an empty char array
//					ret = builder.refVar(builder.literal("", builder.arrayType(gen.getChar())));
				}
			}
		}
	}

	string kernelFilePath;
	if(const VariablePtr pathVar = dynamic_pointer_cast<const Variable>(utils::tryRemove(gen.getRefDeref(), path))) {
		KernelCodeRetriver kcr(pathVar, root, mProgram);
		visitDepthFirst(mProgram, kcr);
		kernelFilePath = kcr.getKernelFilePath();
	}

	if(const CallExprPtr pathCall = dynamic_pointer_cast<const CallExpr>(utils::tryRemove(gen.getRefDeref(), path))) {
		if(gen.isCompositeRefElem(pathCall->getFunctionExpr())) {
			KernelCodeRetriver kcr(pathCall, root, mProgram);
			visitDepthFirst(mProgram, kcr);
			kernelFilePath = kcr.getKernelFilePath();
		}
	}
	if(kernelFilePath.size() > 0) {
		// check if file has already been added
		if(kernelFileCache.find(kernelFilePath) == kernelFileCache.end()) {
			kernelFileCache.insert(kernelFilePath);
			kernels = loadKernelsFromFile(kernelFilePath, builder, includeDirs);
		}
	}

	return kernels;
}

std::vector<ExpressionPtr> KernelReplacer::lookForKernelFilePragma(const core::TypePtr& type, const core::ExpressionPtr& createProgramWithSource) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	lang::BasicGenerator gen(mgr);
	ConversionJob job;
	// set to store paths of loaded kernel files

	std::vector<ExpressionPtr> kernelEntries;

	if(CallExprPtr cpwsCall = dynamic_pointer_cast<const CallExpr>(utils::tryRemoveAlloc(createProgramWithSource))) {
		if(insieme::annotations::ocl::KernelFileAnnotationPtr kfa = dynamic_pointer_cast<insieme::annotations::ocl::KernelFileAnnotation>
				(createProgramWithSource->getAnnotation(insieme::annotations::ocl::KernelFileAnnotation::KEY))) {
			const string& path = kfa->getKernelPath();

			// check if file has already been added
			if(kernelFileCache.find(path) == kernelFileCache.end()) {
				kernelFileCache.insert(path);
				const ProgramPtr kernels = loadKernelsFromFile(path, builder, includeDirs);

//std::cout << "\nProgram: " << printer::PrettyPrinter(kernels) << std::endl;

				for_each(kernels->getEntryPoints(), [&](ExpressionPtr kernel) {
						kernelEntries.push_back(kernel);
				});
			}
		}
	}
	return kernelEntries;
}

NodePtr KernelReplacer::getTransformedProgram() {
	TreePatternPtr nameLiteral = pattern::var("kernel_name", pattern::any);
	TreePatternPtr clCreateKernel = irp::callExpr(pattern::any, irp::literal("clCreateKernel"),	pattern::any <<
			nameLiteral << pattern::var("err", pattern::any) );
	loadKernelCode(clCreateKernel);
	collectArguments();
	inlineKernelCode();
	replaceKernels();

//	std::cout << printer::PrettyPrinter(this->prog) << std::endl;

	return prog;
}

NodePtr IclKernelReplacer::getTransformedProgram() {
	TreePatternPtr nameLiteral = pattern::var("kernel_name", pattern::any);
	TreePatternPtr icl_create_kernel = irp::callExpr(pattern::any, irp::literal("icl_create_kernel"), pattern::any << pattern::var("file_name", pattern::any) <<
			nameLiteral << pattern::any <<	pattern::any);
	loadKernelCode(icl_create_kernel);
	collectArguments();
	inlineKernelCode();
	replaceKernels();

//	std::cout << printer::PrettyPrinter(this->prog) << std::endl;

	return prog;
}

void IclKernelReplacer::loadKernelCode(core::pattern::TreePatternPtr createKernel) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	std::vector<std::string> kernelPaths = findKernelNames(createKernel);

	std::vector<ExpressionPtr> kernelEntries;
	std::map<string, int> checkDuplicates;

	for_each(kernelPaths, [&](std::string path) {
		// check if file has already been added
		if(kernelFileCache.find(path) == kernelFileCache.end()) {
			kernelFileCache.insert(path);
			const ProgramPtr kernels = loadKernelsFromFile(path, builder, includeDirs);

			for_each(kernels->getEntryPoints(), [&](ExpressionPtr kernel) {
					kernelEntries.push_back(kernel);
			});
		}

	});

	storeKernelLambdas(kernelEntries, checkDuplicates);
}


//void icl_run_kernel(const icl_kernel* kernel, cl_uint work_dim, const size_t* global_work_size, const size_t* local_work_size,
//	icl_event* wait_event, icl_event* event, cl_uint num_args, ...) {
void IclKernelReplacer::collectArguments() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	NodeAddressMap replacements;

	TreePatternPtr iclRunKernel = irp::callExpr(pattern::any, irp::literal("icl_run_kernel"),
			(var("kernel", pattern::any)) << var("work_dim", pattern::any) <<
			pattern::var("global_work_size", pattern::any) << pattern::var("local_work_size", pattern::any) <<
			pattern::any << pattern::any << var("num_args", pattern::any ) << //var("args", pattern::any) );
			irp::callExpr(pattern::any, pattern::atom(gen.getVarlistPack()), pattern::single(var("args", pattern::any)) ));

	NodeAddress pA(prog);
	irp::matchAllPairs(iclRunKernel, pA, [&](const NodeAddress& matchAddress, const AddressMatch& runKernel) {
		ExpressionAddress kernel = matchAddress >> runKernel["kernel"].getValue().as<ExpressionAddress>();

		TupleExprPtr arguments = runKernel["args"].getValue().as<TupleExprPtr>();
		ExpressionsPtr member = arguments->getExpressions();

		StatementList tupleCreation;

		for(unsigned i = 0; i < member.size(); ++i) {
			// first look at the size of the argument
			bool isIcl_buffer = analysis::isZero(member[i]);
//std::cout << member[i] << " is zero: " << isIcl_buffer << std::endl;
			++i;
			// then look at the value of the argument
			ExpressionPtr storedArg =  isIcl_buffer ? builder.callExpr(gen.getScalarToArray(),  member[i]) : member[i];

			ExpressionPtr addToTuple;
//std::cout << utils::getRootVariable(matchAddress >> kernel) << std::endl << *utils::getRootVariable(matchAddress >> kernel) << std::endl;

			if(analysis::isZero(storedArg)) { // local memory argument
				kernelTypes[utils::getRootVariable(kernel).as<ExpressionPtr>()].push_back(gen.getUInt8());
				// store the information that this is a local memory argument
//				localMemArgs[kernel].insert(member[i-1]);
				addToTuple = member[i-1];
			} else {
				kernelTypes[utils::getRootVariable(kernel).as<ExpressionPtr>()].push_back(storedArg->getType());
				addToTuple = storedArg;
			}

			tupleCreation.push_back(builder.assign(builder.callExpr(addToTuple->getType(), gen.getTupleRefElem(), kernel, builder.intLit(i/2),
					builder.getTypeLiteral(addToTuple->getType())), addToTuple));
		}

		// add kernel call function
		ExpressionPtr newKernelCall = createKernelCallLambda(kernel, runKernel["work_dim"].getValue().as<ExpressionPtr>(),
				runKernel["local_work_size"].getValue().as<ExpressionPtr>(), runKernel["global_work_size"].getValue().as<ExpressionPtr>());


		transform::utils::migrateAnnotations(runKernel.getRoot().as<NodePtr>(), newKernelCall.as<NodePtr>());

		// add kernel call to compound expression
		tupleCreation.push_back(newKernelCall);

		// replace the kernel call by a compound statement which collects all the arguments in a tuple and does the kernel call (to be replaced later)
		replacements[matchAddress >> runKernel.getRoot()] = builder.getNoOp();// builder.createCallExprFromBody(builder.compoundStmt(tupleCreation), gen.getUnit());
	});

//for(std::pair<NodePtr, NodePtr> cur : replacements) {
//	dumpPretty(cur.second);
//}

	prog = transform::replaceAll(mgr, replacements);

}

void IclKernelReplacer::inlineKernelCode() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	TreePatternPtr iclRunKernel = irp::callExpr(pattern::any, irp::literal("icl_run_kernel"),
			var("kernel", pattern::any) << var("work_dim", pattern::any) <<
			pattern::var("global_work_size", pattern::any) << pattern::var("local_work_size", pattern::any) <<
			pattern::any << pattern::any << var("num_args", pattern::any ) << //var("args", pattern::any) );
			irp::callExpr(pattern::any, pattern::atom(gen.getVarlistPack()), pattern::single(var("args", pattern::any)) ));

	NodeAddress pA(prog);
	irp::matchAllPairs(iclRunKernel, pA, [&](const NodeAddress& matchAddress, const AddressMatch& runKernel) {
	});
}

} //namespace ocl
} //namespace frontend
} //namespace insieme

