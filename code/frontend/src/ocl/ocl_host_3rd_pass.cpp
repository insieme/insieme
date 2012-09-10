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

#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_3rd_pass.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/parser/ir_parse.h"

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {
using namespace insieme::core;

namespace {


// visitor finds calls to array.create.1D to check if the type is still correct
class ArrayCreat1DFinder: public IRVisitor<bool> {
	const IRBuilder& builder;
//	TypePtr match;

public:
	ArrayCreat1DFinder(const IRBuilder& build): IRVisitor<bool>(false), builder(build) {}

	bool visitNode(const NodePtr& node) {
		return false;
	}

	bool visitCallExpr(const CallExprPtr& call) {
		if(call->getFunctionExpr() == BASIC.getArrayCreate1D()) {
			if(call->getArgument(0)->getType()->toString().find("array<_cl_") != string::npos) {
/*				match = call->getArgument(0)->getType();
				if(const GenericTypePtr gt = dynamic_pointer_cast<const GenericType>(match))
					match = gt->getTypeParameter()[0]; // taken from typechecks.cpp check type literal*/
				return true;
			}
		}
		return false;
	}

//	TypePtr getMatch(){ return match; }
};
}

// Generates a function which, taking the kernel name as a string as argument, returns the corresponding lambda
const ExpressionPtr HostMapper3rdPass::genGetKernelLambda() {
/*	std::vector<StatementPtr> ifs;
	ExpressionPtr arg;

	std::pair<ExpressionPtr, LambdaExprPtr> l = *kernelLambdas.begin();

	const ExpressionPtr& condition = builder.callExpr(BASIC.getCharArrayEq(), arg, l.first);
	ifs.push_back(builder.ifStmt(condition, builder.returnStmt(l.second)));

*//*
	for_each(kernelLambdas, [&](std::pair<ExpressionPtr, LambdaExprPtr> l) {
		ifs.push_back(builder.ifStmt(builder.callExpr(BASIC.getEq(), arg, l->first), builder.returnStmt(l->second)));
	});*//*
	StatementPtr body = builder.compoundStmt(ifs);
	FunctionTypePtr ft = builder.functionType(toVector(builder.arrayType(BASIC.getChar())), )

	builder.lambdaExpr()
*/
	return builder.intLit(0);
}


// Takes a function which argument's may have changed and which return value depends on the argument to create a new function
// with an appropriate return value
bool HostMapper3rdPass::updateReturnVal(const core::CallExprPtr& oldCall, core::NodePtr& newCall) {
// TODO test if this function is still needed at all

	ExpressionPtr fun = oldCall->getFunctionExpr();
	const TypePtr oldType = oldCall->getType();
	// check if return type of array/vector subscript calls are still valid
	if(oldType->toString().find("array<_cl_mem,1>") != string::npos) {

		if(BASIC.isSubscriptOperator(fun)) {
//		std::cout << "-----------------------\n" << oldCall << std::endl << std::endl << oldCall->getArgument(0)->getType() << std::endl;
			if(const SingleElementTypePtr seType = dynamic_pointer_cast<const SingleElementType>((oldCall->getArgument(0)->getType()))) {
				const TypePtr oldBType = getBaseType(oldCall);
/*						if(BASIC.isArrayOp(oldCall->getFunctionExpr()))
					std::cout << "ARRRARY " << oldType << " " << getBaseType(seType->getElementType()) << std::endl;
				else if(BASIC.isVectorOp(oldCall->getFunctionExpr()))
					std::cout << "VECTOR " << seType << std::endl;
				else
					std::cout << "NOTHING " << seType << oldCall->getFunctionExpr() << std::endl;
*///std::cout << "seType " << seType << " oldbt " << oldBType << std::endl;
assert(seType->toString().find("_cl_kernel") == string::npos && "Kernel variable has not been replaced nor removed");

				if(getBaseType(seType) != oldBType) {
					TypePtr newRetType = dynamic_pointer_cast<const Type>(core::transform::replaceAll(builder.getNodeManager(), oldType,
							oldBType, getBaseType(seType)));
					newCall = builder.callExpr(newRetType, oldCall->getFunctionExpr(), oldCall->getArguments());
					return true;
				}
			}
		}

		if(fun == BASIC.getRefNew()) {
			if(oldType != builder.refType(oldCall->getArgument(0)->getType())) {
				newCall = builder.refNew(oldCall->getArgument(0));
				return true;
			}
		}
		if(fun == BASIC.getRefVar()) {
			if(oldType != builder.refType(oldCall->getArgument(0)->getType())) {
				newCall = builder.refVar(oldCall->getArgument(0));
				return true;
			}
		}
	}

	if(fun == BASIC.getArrayRefElem1D()) {

		const TypePtr retTy = builder.refType(static_pointer_cast<const SingleElementType>(static_pointer_cast<const RefType>(
				oldCall->getArgument(0)->getType())->getElementType())->getElementType());

		if(*oldType != *retTy) {
			// type of array has been uptdated, update call
			newCall = builder.callExpr(retTy, fun, oldCall->getArguments());
			return true;
		}
	}

	if(fun == BASIC.getArraySubscript1D()) {
		const TypePtr& retTy = static_pointer_cast<const SingleElementType>(oldCall->getArgument(0)->getType())->getElementType();
		if(oldType != retTy) {
			// type of array has been uptdated, update call
			newCall = builder.callExpr(retTy, fun, oldCall->getArguments());
			return true;
		}
	}

	return false;
}


// returns a 0-literal of the corresponding type or NoOp in cas of unit
const ExpressionPtr HostMapper3rdPass::getZeroElem(const TypePtr& type) {
	return builder.literal(type, "00");
	TypeList noArgs;
	std::vector<VariablePtr> noParams;
	std::vector<ExpressionPtr> noExpr;
/*
	if(type == BASIC.getUnit()) {
		LambdaExprPtr zeroCall = builder.lambdaExpr(builder.functionType(noArgs, type), noParams, builder.returnStmt(BASIC.getUnit()));
		return builder.callExpr(type, zeroCall);
	}
*//*
	const LambdaExprPtr& zeroCall = builder.lambdaExpr(builder.functionType(noArgs, type), noParams, builder.returnStmt(builder.literal(type, "0")));
	return builder.callExpr(type, zeroCall);*/
}



// gets the innermost type out of an array/ref nest
// TODO change to something that makes more sense
const TypePtr HostMapper3rdPass::getInnermostType(const TypePtr& type){
	if(const ArrayTypePtr at = dynamic_pointer_cast<const ArrayType>(type))
		return getInnermostType(at->getElementType());
	if(const RefTypePtr rt = dynamic_pointer_cast<const RefType>(type))
		return getInnermostType(rt->getElementType());

	return type;
}

/* Assumptions:
 * 1. the work dimension is a scalar in the arguments
 * 2. The cast to void* of the local/global size happens in the argument
*/
const ExpressionPtr HostMapper3rdPass::anythingToVec3(ExpressionPtr workDim, ExpressionPtr size) {
	const TypePtr vecTy = builder.vectorType(BASIC.getUInt8(), builder.concreteIntTypeParam(static_cast<size_t>(3)));
	TypePtr argTy;
	VariablePtr param;
	ExpressionPtr arg;
	unsigned int wd;

	if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(workDim)) {
		workDim = cast->getSubExpression();
	}

	// check work dimension
	const LiteralPtr dim = dynamic_pointer_cast<const Literal>(workDim);
	assert(dim && "Cannot determine work_dim of clEnqueueNDRangeKernel. Should be a literal!");
	wd = dim->getValueAs<unsigned int>();
	//    std::cout << "*****************WorkDim: " << dim->getValue() << std::endl;
	assert(workDim < 3u && "Invalid work_dim. Should be 1 - 3!");

	// check if there is a x to array called
	if(const CallExprPtr toArray = dynamic_pointer_cast<const CallExpr>(size)) {
		if(toArray->getFunctionExpr() == BASIC.getScalarToArray()) {
			// check consitency with workDim, should be 1
			assert(wd == 1 && "Scalar group size passed to a multi dimensional work_dim");

			argTy = toArray->getArgument(0)->getType();
			param = builder.variable(argTy);
			arg = toArray->getArgument(0);
		} else if(toArray->getFunctionExpr() == BASIC.getRefVectorToRefArray()) {
			arg = toArray->getArgument(0);
			argTy = arg->getType();
			param = builder.variable(argTy);
		} else if(toArray->getFunctionExpr() == BASIC.getRefVar() ) {
			if(const CallExprPtr vta = dynamic_pointer_cast<const CallExpr>(toArray->getArgument(0))) {
	// throw away ref.var
	// TODO only a dirty fix, check it
	// this will no longer happen, vector-to-array is gone
//				if(vta->getFunctionExpr() == BASIC.getVectorToArray()) {
//					arg = vta->getArgument(0);
//					argTy = arg->getType();
//					param = builder.variable(argTy);
//				}
			}
		} else {
			std::cerr << "Unexpected Function: " << toArray << " of type " << toArray->getArgument(0)->getType() << std::endl;
			assert(false && "Unexpected function in OpenCL size argument");
		}
	} else { // the argument is an array
		size = tryDeref(size, builder);
		assert(size->getType()->getNodeType() == NT_ArrayType && "Called clEnqueueNDRangeKernel with invalid group argument");
		argTy = size->getType();
		param = builder.variable(argTy);
		arg = size;
	}

	ExpressionPtr init = param;

	if(RefTypePtr ref = dynamic_pointer_cast<const RefType>(param->getType())) {
		init = builder.deref(param);
		//        argTy = ref->getElementType();
	}

	TypePtr fieldTy;
	if(const ArrayTypePtr array = dynamic_pointer_cast<const ArrayType>(init->getType()))
	fieldTy = array->getElementType();

	if(const VectorTypePtr vector = dynamic_pointer_cast<const VectorType>(init->getType()))
	fieldTy = vector->getElementType();

	DeclarationStmtPtr vDecl;
	if(wd == 1) {
		if(fieldTy)
		init = builder.callExpr(fieldTy, BASIC.getArraySubscript1D(), init, builder.literal(BASIC.getUInt8(), "0"));
		if(init->getType() != BASIC.getUInt8()) {
			init = builder.castExpr(BASIC.getUInt8(), init);
		}
		vDecl = builder.declarationStmt(vecTy,
				builder.vectorExpr(toVector<ExpressionPtr>(init, builder.literal(BASIC.getUInt8(), "1"), builder.literal(BASIC.getUInt8(), "1"))));
	} else {
		assert(fieldTy && "Size argument of multidimensional group is no vector or array");

		vector<ExpressionPtr> subscripts;
		subscripts.push_back(builder.callExpr(fieldTy, BASIC.getArraySubscript1D(), init, builder.literal(BASIC.getUInt8(), "0")));
		subscripts.push_back(builder.callExpr(fieldTy, BASIC.getArraySubscript1D(), init, builder.literal(BASIC.getUInt8(), "1")));
		subscripts.push_back(wd == 3 ? (ExpressionPtr)builder.callExpr(fieldTy, BASIC.getArraySubscript1D(), init, builder.literal(BASIC.getUInt8(), "2")) :
				(ExpressionPtr)builder.literal(BASIC.getUInt8(), "1"));

		for_each(subscripts, [&](ExpressionPtr& r) {
					if(r->getType() != BASIC.getUInt8())
					r = builder.castExpr(BASIC.getUInt8(), r);
				});

		vDecl = builder.declarationStmt(vecTy, builder.vectorExpr(subscripts));
	}

	FunctionTypePtr fctTy = builder.functionType(toVector(argTy), vecTy);

	return builder.callExpr(vecTy, builder.lambdaExpr(fctTy, toVector(param) , builder.compoundStmt(vDecl,
							builder.returnStmt(vDecl->getVariable()))), arg);
}

const NodePtr HostMapper3rdPass::handleNDRangeKernel(const CallExprPtr& callExpr, const CallExprPtr& newCall, const size_t offset) {
    // get kernel function
    ExpressionPtr k = callExpr->getArgument(1-offset);

    // check if argument is a call to ref.deref
    k = tryRemove(BASIC.getRefDeref(), k, builder);
    // get corresponding lambda expression
/*equal_variables cmp(builder, program);
for_each(kernelLambdas, [](std::pair<ExpressionPtr, LambdaExprPtr> ka) {
std::cout << "\nArguments: " << ka.first << "\n";
//for_each(ka.second, [](ExpressionPtr a){std::cout << a->getType() << " " << a << std::endl;});
});
std::cout << "\nk " << k << "\ny " << kernelLambdas.begin()->first << "\n compare: " <<  cmp(kernelLambdas.begin()->first, k) << std::endl; //*/
/*std::cout << "\nREACHED " << *k << std::endl;
std::cout << "\nLambda: " << kernelLambdas.begin()->first << std::endl;//*/
//    equal_variables shit(builder, program);

    assert(kernelLambdas.find(k) != kernelLambdas.end() && "No lambda expression for kernel call found");

    LambdaExprPtr lambda = kernelLambdas[k];

/*    assert(kernelArgs.find(k) != kernelArgs.end() && "No arguments for call to kernel function found");
    const VariablePtr& args = kernelArgs[k];
    const TupleTypePtr& argTypes = dynamic_pointer_cast<const TupleType>(args->getType());*/
    const VariableList& interface = lambda->getParameterList()->getElements();
 //   assert(argTypes && "The kernel arguments have to be stored in a tuple");

	// make a three element vector out of the global and local size
	const ExpressionPtr global = anythingToVec3(newCall->getArgument(2-offset), newCall->getArgument(4-2*offset));
	const ExpressionPtr local = anythingToVec3(newCall->getArgument(2-offset), newCall->getArgument(5-2*offset));

	vector<ExpressionPtr> newArgs;

	// construct call to kernel function
	size_t cnt = 0;
	if(localMemDecls.find(k) == localMemDecls.end() || localMemDecls[k].size() == 0) {
//std::cout << "lmd " << localMemDecls[k] << std::endl;
		// if there is no local memory in argument, the arguments can simply be copied
		if(kernelArgs.find(k) == kernelArgs.end()) { // ndRangeKernel
			for(size_t i = 0; i < interface.size() -2 /*argTypes->getElementTypes().size()*/; ++i) {
				TypePtr argTy = vectorArrayTypeToScalarArrayType(interface.at(i)->getType(), builder);
				ExpressionPtr tupleMemberAccess = builder.callExpr(argTy, BASIC.getTupleMemberAccess(), removeDoubleRef(k, builder),
						builder.literal(BASIC.getUInt8(), toString(i)), builder.getTypeLiteral(argTy));
				if(*argTy != *interface.at(i)->getType()) // argument of kernel is an ocl vector type
					tupleMemberAccess = builder.callExpr(interface.at(i)->getType(), BASIC.getRefReinterpret(),
							tupleMemberAccess, builder.getTypeLiteral(removeSingleRef(interface.at(i)->getType())));

				newArgs.push_back(tupleMemberAccess);
			}
		} else for_each(kernelArgs[k], [&](ExpressionPtr kArg) { // icl_run_kernel
			ExpressionPtr argAccess = removeDoubleRef(this->resolveElement(kArg).as<ExpressionPtr>(), builder);

			if(*interface.at(cnt)->getType() != *argAccess->getType()) {
				TypePtr reinterpretedType = builder.refType(interface.at(cnt)->getType());
				argAccess = builder.deref(builder.callExpr(reinterpretedType, BASIC.getRefReinterpret(),
						tryRemove(BASIC.getRefDeref(), argAccess, builder), builder.getTypeLiteral(interface.at(cnt)->getType())));


			}
			newArgs.push_back(argAccess);

			++cnt;
		});

		/*ßßß
		// if there is no local memory in argument, the arguments can simply be copied
		if(kernelArgs.find(k) == kernelArgs.end()) { // ndRangeKernel
			for(size_t i = 0; i < interface.size() -2 ; ++i) {
				ExpressionPtr argAccess = removeDoubleRef(this->resolveElement(kernelArgs[k].at(i)).as<ExpressionPtr>(), builder);

				if(*interface.at(cnt)->getType() != *argAccess->getType()) {
					TypePtr reinterpretedType = builder.refType(interface.at(cnt)->getType());
					argAccess = builder.deref(builder.callExpr(reinterpretedType, BASIC.getRefReinterpret(),
							tryRemove(BASIC.getRefDeref(), argAccess, builder), builder.getTypeLiteral(interface.at(cnt)->getType())));


				}
				newArgs.push_back(argAccess);
			}
		} else for_each(kernelArgs[k], [&](ExpressionPtr kArg) { // icl_run_kernel
std::cout << "\nkarg" << k->getType() << std::endl;
				TypePtr argTy = kArg->getType();
				bool kernelIsRef = k->getType().as<RefTypePtr>()->getElementType()->getNodeType() == NT_RefType;
std::cout << "\nARGTY " << argTy << std::endl;
				ExpressionPtr tupleMemberAccess = builder.callExpr(argTy, kernelIsRef ? BASIC.getTupleRefElem() : BASIC.getTupleMemberAccess(), (k),
						builder.literal(BASIC.getUInt8(), toString(cnt)), builder.getTypeLiteral(argTy));
std::cout << "TMA " << tupleMemberAccess << std::endl;
				if(*argTy != *(interface.at(cnt)->getType())) { // argument of kernel is an ocl vector type
					TypePtr refArgTy = kernelIsRef ? builder.refType(interface.at(cnt)->getType()) : interface.at(cnt)->getType();
					tupleMemberAccess = builder.callExpr(refArgTy, BASIC.getRefReinterpret(),
							tupleMemberAccess, builder.getTypeLiteral(tryDeref(interface.at(cnt), builder)->getType()));
std::cout << "\nReinterpret " << tupleMemberAccess << std::endl;
				}
				newArgs.push_back(kernelIsRef ? builder.deref(tupleMemberAccess) : tupleMemberAccess);

				++cnt;
		});

		 */

		// add global and local size to arguments
		newArgs.push_back(global);
		newArgs.push_back(local);

		NodePtr kernelCall = builder.callExpr(BASIC.getInt4(), lambda, newArgs);
		copyAnnotations(callExpr, kernelCall);

		return kernelCall;
	}

	// icl_run_kernel with local memory arguments
	// add declarations for argument local variables if any, warping a function around it
	assert(kernelArgs.find(k) != kernelArgs.end() && "No kernel arguments for local variable declarations found");
	std::vector<core::ExpressionPtr>& args = kernelArgs[k];

	// generate fresh variables for all variables needed to determine local variable sizes
	VariableMap neededVarsMap = refreshVariables(localMemDecls[k], builder);

	vector<StatementPtr> declsAndKernelCall;
	for_each(localMemDecls[k], [&](DeclarationStmtPtr& decl) {
		assert(!!decl && "Kernel has illegal local memory argument");
		declsAndKernelCall.push_back(decl);
	});

	VariableList params;
	vector<ExpressionPtr> innerArgs;
	vector<TypePtr> wrapperInterface;

	cnt = 0;
	for_each(args, [&](ExpressionPtr arg) {
		VariablePtr param = interface.at(cnt);
		assert(!!arg && "Kernel has illegal global memory argument");
		bool local = false;
		//global and private memory arguments must be variables
		arg = getVarOutOfCrazyInspireConstruct(arg, builder);
		// local args are declared in localMemDecls
		for_each(localMemDecls[k], [&](DeclarationStmtPtr& decl) {
			assert(!!decl && "Kernel has illegal local memory argument");
			if(arg == decl->getVariable()) {
				// will be declared inside wrapper function
//				assert(false);
				local = true;
			}
		});


		if(!local) {
			// global and private memory arguments will be passed to the wrapper function as argument
			ExpressionPtr argAccess = removeDoubleRef(this->resolveElement(arg).as<ExpressionPtr>(), builder);

			// add refReinterpret if needed
			argAccess = tryRefReinterpret(argAccess, param->getType(), builder);

			newArgs.push_back(argAccess);

			//newArgs.push_back(builder.callExpr(tryDeref(newArg, builder)->getType(),BASIC.getRefDeref(), newArg));
			wrapperInterface.push_back(newArgs.back()->getType());

			// kernel funtion will take a new variable as argument
			params.push_back(builder.variable(newArgs.back()->getType()));
			// the kernel call will use the params of the outer call as arguments, they must be an expression
			innerArgs.push_back(params.back());
		} else {
			// furthermore we have to add local variables
			innerArgs.push_back(tryRefReinterpret(arg, param->getType(), builder));
		}
		++cnt;
	});

	// add variables needed for local variable initialization to wrapper interface
	for_each(neededVarsMap, [&](std::pair<VariablePtr, VariablePtr> varMapping) {
		newArgs.push_back(varMapping.first);
		wrapperInterface.push_back(varMapping.first->getType());
		params.push_back(varMapping.second);
	});

	// add global and local size to arguments
	TypePtr vec3type = builder.vectorType(BASIC.getUInt8(), builder.concreteIntTypeParam(static_cast<size_t>(3)));
	newArgs.push_back(global);
	VariablePtr globalVar = builder.variable(vec3type);
	params.push_back(globalVar);
	innerArgs.push_back(globalVar);
	wrapperInterface.push_back(vec3type);

	newArgs.push_back(local);
	VariablePtr localVar = builder.variable(vec3type);
	params.push_back(localVar);
	innerArgs.push_back(localVar);
	wrapperInterface.push_back(vec3type);


	NodePtr kernelCall = builder.returnStmt(builder.callExpr(BASIC.getInt4(), lambda, innerArgs));
	copyAnnotations(callExpr, kernelCall);

	declsAndKernelCall.push_back(dynamic_pointer_cast<const Statement>(kernelCall));
	const FunctionTypePtr wrapperType = builder.functionType(wrapperInterface, BASIC.getInt4());
	return builder.callExpr(BASIC.getInt4(), builder.lambdaExpr(builder.lambda(wrapperType, params,
			builder.compoundStmt(declsAndKernelCall))), newArgs);
}

void HostMapper3rdPass::addTupletoStruct(const core::NamedValuePtr& oldInitMember, core::NamedValueList& newInitMembers,
		core::NamedTypeList& newMembers, const VariablePtr& var, const size_t i) {

	LambdaSearcher lambdaSearcher(builder, var, program);
	IdSearcher ids(oldInitMember->getName());
	for_each(kernelLambdas, [&](std::pair<core::ExpressionPtr, core::LambdaExprPtr> kl) {
		const NodeAddress& lAddr = core::Address<const core::Variable>::find(getVariableArg(kl.first, builder), program);

		lambdaSearcher.setLambdaVariable(getVariableArg(kl.first, builder));
		// check if the variable is right
		if(var == getVariableArg(kl.first, builder) || visitPathBottomUpInterruptible(lAddr, lambdaSearcher)) {
			// check identifier
			if(visitDepthFirstInterruptible(kl.first, ids)) {
				// now we found the right kernelLambda
				core::VariableList pl = kl.second->getParameterList()->getElements();
				TypeList elementTypes;
				for(size_t j = 0; j < pl.size()-2 /*global and local size not considered*/; ++j) {
					elementTypes.push_back(vectorArrayTypeToScalarArrayType(pl.at(j)->getType(), builder));
				}

				const TupleTypePtr tty = builder.tupleType(elementTypes);
				TypePtr newType = dynamic_pointer_cast<const Type>(transform::replaceAll(builder.getNodeManager(),
						newMembers.at(i)->getType(), builder.refType(builder.arrayType(builder.genericType("_cl_kernel"))),
						tty));

				VariablePtr newVar = static_pointer_cast<const Variable>(transform::replaceAll(builder.getNodeManager(),
						cl_mems[var], builder.refType(builder.arrayType(builder.genericType("_cl_kernel"))), tty));

				cl_mems[var] = newVar;

				core::NamedValuePtr newInitMember = builder.namedValue(oldInitMember->getName(),
						builder.callExpr(newType, BASIC.getUndefined(), builder.getTypeLiteral(newType)));
				newInitMembers.push_back(newInitMember);
			}
		}
	});
}


const NodePtr HostMapper3rdPass::resolveElement(const NodePtr& element) {
	// stopp recursion at type level
	if (element->getNodeCategory() == NodeCategory::NC_Type) {
		return element;//->substitute(builder.getNodeManager(), *this);
	}

	// TODO make more efficient
/*	if(replacements.find(element) != replacements.end()) {
//		std::cout << "Replacing " << element << " with " << replacements[element];
		return replacements[element];
	}
*/
	if(const VariablePtr var = dynamic_pointer_cast<const Variable>(element)) {
		if(cl_mems.find(var) != cl_mems.end()) {
//			std::cout << "cl_mems: " << var->getType() << " " << var << " -> " << cl_mems[var]->getType() << " " << cl_mems[var] << std::endl;
			return cl_mems[var];
		}
	}

	if(const DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(element)) {
		const VariablePtr var = decl->getVariable();

		bool toReplace = cl_mems.find(var) != cl_mems.end();

		// delete the declaration of icl_kernel variables
		if((var->getType()->toString().find("struct<kernel:ref<array<_cl_kernel,1>>") != string::npos) && !toReplace) {
			return builder.getNoOp();
		}

		if(toReplace) {
			if(const StructTypePtr sType = dynamic_pointer_cast<const StructType>(getNonRefType(cl_mems[var]))) {
				// throw elements which are not any more in the struct out of the initialization expression and update init values for the remaining ones
				// look into ref.new
				if(const CallExprPtr refNew = dynamic_pointer_cast<const CallExpr>(decl->getInitialization())) {
					if(refNew->getFunctionExpr() == BASIC.getRefNew() || refNew->getFunctionExpr() == BASIC.getRefVar()) {
						if(const StructExprPtr oldInit = dynamic_pointer_cast<const StructExpr>(refNew->getArgument(0))) {
							core::NamedValueList newInitMembers;
							core::NamedCompositeType::Entries newMembers = sType->getEntries();
							size_t i = 0;
							for_each(oldInit->getMembers()->getElements(), [&](const core::NamedValuePtr& oldInitMember) {
								// assuming that the order of the (exisiting) elements in newMembers and oldMember is the same,
								// we always have to compare only one element
								if(newMembers.size() > i && *oldInitMember->getName() == *newMembers.at(i)->getName()) {
									// check if the type of the init expression is the same as the type of the field (type of field may changed)
									if(*newMembers.at(i)->getType() != *oldInitMember->getValue()->getType()) {
										// always init as undefined in this case
										const TypePtr initType = newMembers.at(i)->getType();
										NamedValuePtr newInitMember = builder.namedValue(oldInitMember->getName(),
												builder.callExpr(initType, BASIC.getUndefined(), builder.getTypeLiteral(initType)));
										newInitMembers.push_back(newInitMember);
//std::cout << "\nMembers: " << newMembers.at(i) << " \n" << kernelLambdas.begin()->first << std::endl;
									} else if(newMembers.at(i)->getType()->toString().find("array<_cl_kernel,1>") != string::npos /*&&
											kernelLambdas.find(var) != kernelLambdas.end()*/) {
										addTupletoStruct(oldInitMember, newInitMembers, newMembers, var, i);
									} else
										newInitMembers.push_back(oldInitMember);
									++i;
								}
							});

							// create a new Declaration Statement which's init expression contains only the remaining fields
							return builder.declarationStmt(cl_mems[var], builder.refNew(builder.structExpr(newInitMembers)));
						}
					}
				}
			}


			// check if the init funcition of the variable is still of cl_mem type
			TypePtr oldType = decl->getInitialization()->getType();

			if(const CallExprPtr initFct = dynamic_pointer_cast<const CallExpr>(this->resolveElement(decl->getInitialization()))) {
				LiteralPtr typeLiteral;
				if(initFct->getArguments().size() > 0)
				if(const CallExprPtr undefined = dynamic_pointer_cast<const CallExpr>(initFct->getArgument(0))) { // default init
					if(undefined->getFunctionExpr() == BASIC.getUndefined()) {
						typeLiteral = dynamic_pointer_cast<const Literal>(undefined->getArgument(0));
						assert(typeLiteral && "Unexpected argument when initializing variable with undefined");
						oldType = static_pointer_cast<GenericTypePtr>(typeLiteral->getType())->getTypeParameter(0);
					}
				}

				if(initFct->getArguments().size() > 0)
				if(const CastExprPtr castToClMem = dynamic_pointer_cast<const CastExpr>(initFct->getArgument(0))) {
					oldType = (castToClMem->getType());
					assert(oldType && "Unexpected argument when initializing variable by cast");
				}

			}

			// handling of cl_mem arrays
			if(oldType->toString().find("array<_cl_mem,1>") != string::npos) {
				// get new element type
				while(const SingleElementTypePtr & interType = dynamic_pointer_cast<const SingleElementType>(oldType) )
					oldType = interType->getElementType();
//				if(initFct->getArgument(0) == builder.callExpr(BASIC.getUndefined(), BASIC.getTypeLiteral(builder.arrayType(builder.genericType("_cl_mem")))) // default init
//						|| initFct->getArgument(0) == builder.castExpr(builder.arrayType(builder.genericType("_cl_mem")),
//								builder.callExpr(BASIC.getGetNull(), BASIC.getTypeLiteral(BASIC.getInt8())))) { // init with NULL
					// overwrite default initialization to cl_mem with default initialization to array<whatever>
					TypePtr newType = dynamic_pointer_cast<const Type>(core::transform::replaceAll(builder.getNodeManager(), oldType,
							builder.genericType("_cl_mem"),	cl_mems[var]->getType()));
					if(const RefTypePtr rt = dynamic_pointer_cast<const RefType>(cl_mems[var]->getType()))
						newType = rt->getElementType();
					else
						newType = cl_mems[var]->getType();

					NodePtr ret = builder.declarationStmt(cl_mems[var], builder.refNew(builder.callExpr(newType, BASIC.getUndefined(),
							builder.getTypeLiteral(newType))));
					copyAnnotations(decl, ret);
					return ret;
			//	}
			}
		} else {
			if(var->getType()->toString().find("array<_cl_kernel,1>") != string::npos) {
				// declare tuple to hold the arguments for the kernel
				LambdaSearcher lambdaSearcher(builder, var, program);
				NodePtr newDecl;
				for_each(kernelLambdas, [&](std::pair<core::ExpressionPtr, core::LambdaExprPtr> kl) {
					const NodeAddress& lAddr = core::Address<const core::Variable>::find(getVariableArg(kl.first, builder), program);

					lambdaSearcher.setLambdaVariable(getVariableArg(kl.first, builder));
					// check if the variable is right
					if(var == getVariableArg(kl.first, builder) || visitPathBottomUpInterruptible(lAddr, lambdaSearcher)) {
						core::VariableList pl = kl.second->getParameterList()->getElements();
						TypeList elementTypes;
						for(size_t i = 0; i < pl.size()-2 /*global and local size not considered*/; ++i) {
							elementTypes.push_back(vectorArrayTypeToScalarArrayType(pl.at(i)->getType(), builder));
						}
						const TupleTypePtr tty = builder.tupleType(elementTypes);
						const TypePtr newType = static_pointer_cast<const Type>(transform::replaceAll(builder.getNodeManager(),
								var->getType(), builder.refType(builder.arrayType(builder.genericType("_cl_kernel"))),
								tty));

						const VariablePtr newVar = builder.variable(newType);
						const TypePtr initType = static_pointer_cast<const RefType>(newType)->getElementType();

						cl_mems[var] = newVar;

						newDecl = builder.declarationStmt(newVar, builder.callExpr(newType, BASIC.getRefNew(),
								builder.callExpr(initType, BASIC.getUndefined(), builder.getTypeLiteral(initType))));
					}
				});
				if(newDecl) {
					copyAnnotations(decl, newDecl);
					return newDecl;
				}
			}

// remove delarations of opencl type variables. Should not be used any more
			if(var->getType()->toString().find("array<_cl_") != string::npos
					&& getNonRefType(var)->getNodeType() != NT_StructType) //TODO remove this line!
			{
				return builder.getNoOp();
			}
		}

	}

	if(const CallExprPtr callExpr = dynamic_pointer_cast<const CallExpr>(element)) {
		const ExpressionPtr fun = callExpr->getFunctionExpr();

		if(const LiteralPtr lit = dynamic_pointer_cast<const Literal>(fun)) {
			if(lit->toString().find("Buffer") != string::npos) {
				LOG(ERROR) << "FOUND " << lit << std::endl;
				assert(false && "Buffer has not been removed during compilation");
			}

		}

		if(fun == BASIC.getRefAssign()) {

			if(const CallExprPtr lCall = dynamic_pointer_cast<const CallExpr>(callExpr->getArgument(0))) {
				// insert correct types for calls to the kernel argument tuple
				if(BASIC.isTupleRefElem(lCall->getFunctionExpr())) {
					const ExpressionPtr& rhs = static_pointer_cast<const Expression>(this->resolveElement(callExpr->getArgument(1)));
					return builder.callExpr(BASIC.getUnit(), BASIC.getRefAssign(), builder.callExpr(builder.refType(rhs->getType()), BASIC.getTupleRefElem(),
							lCall->getArgument(0), lCall->getArgument(1), builder.getTypeLiteral(rhs->getType())), rhs);
				}

			}

			// get rid of some not needed functions
			const CallExprPtr& newCall = static_pointer_cast<const CallExpr>(callExpr->substitute(builder.getNodeManager(), *this));

			if(CallExprPtr rhs = dynamic_pointer_cast<const CallExpr>(newCall->getArgument(1))) {
				// check if it is embedded in a ref.deref
				if(rhs->getArguments().size() > 0)
				if(const CallExprPtr deref = dynamic_pointer_cast<const CallExpr>(rhs->getArgument(0)))
					if(rhs->getFunctionExpr() == BASIC.getRefDeref())
						rhs = deref;
				if(rhs->getFunctionExpr()->toString() == "clCreateContext")
					return builder.getNoOp();
				if(rhs->getFunctionExpr()->toString() == "clCreateCommandQueue")
					return builder.getNoOp();

				// need to update array.create.1D type if type of variable did change or is still a cl_ type
				ArrayCreat1DFinder ac1df(builder);
				if(visitDepthFirstInterruptible(newCall, ac1df)) {
					const TypePtr newType = getInnermostType(newCall->getArgument(0)->getType());
					const TypePtr oldType = getInnermostType(newCall->getArgument(1)->getType());

					NodePtr ret;
					if(oldType != newType) {
						ret = transform::replaceAll(builder.getNodeManager(), newCall, oldType, newType);
					} else {
						// if there are cl_ types for which we have no replacement we remove them
						if(oldType->toString().find("_cl_") != string::npos)
							return builder.getNoOp();
						ret= callExpr;
					}
					copyAnnotations(callExpr, ret);
					return ret;
				}
			}
		}

		if(const CallExprPtr newCall = dynamic_pointer_cast<const CallExpr>(callExpr->substitute(builder.getNodeManager(), *this))) {
			if(const LiteralPtr fun = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr())) {
				if(fun->getStringValue() == "clEnqueueNDRangeKernel") {
					return handleNDRangeKernel(callExpr, newCall, 0);
				}
				if(fun->getStringValue() == "icl_run_kernel" ) {
					return handleNDRangeKernel(callExpr, newCall, 1);
				}
			}

			if(newCall->getFunctionExpr() == BASIC.getCompositeRefElem()) {
				// replace variable with new version if necessary
				const VariablePtr newStruct = dynamic_pointer_cast<const Variable>(newCall->getArgument(0));
				const VariablePtr oldStruct = dynamic_pointer_cast<const Variable>(callExpr->getArgument(0));
//				std::cout << "OldStruct: " << callExpr->getArgument(0) << "\nNewStruct: " << newCall->getArgument(0) << std::endl;
				//TODO test quickfix if NULL, e.g. struct is inside an array
				//assert(oldStruct && newStruct && "First argument of composite.ref.elem must be a struct variable");

				if(!!newStruct && (newStruct != oldStruct)) { // struct variable has been replaced, may need to update type of composite.ref.elem
					const StructTypePtr newType = dynamic_pointer_cast<const StructType>(getNonRefType(newStruct->getType()));

					assert(newType && "First argument of composite.ref.elem must be a struct variable");

					const LiteralPtr oldIdLit = dynamic_pointer_cast<const Literal>(callExpr->getArgument(1));
					const LiteralPtr oldTypeLit = dynamic_pointer_cast<const Literal>(callExpr->getArgument(2));
					assert(oldIdLit && oldTypeLit && "Second and third argument of composite.ref.elem must be a literals");
					// check if the field has been replaced with another field of the same struct with a different identifier
					const LiteralPtr newIdLit = replacements.find(oldIdLit) != replacements.end() ?
							static_pointer_cast<const Literal>(replacements[oldIdLit]) : oldIdLit;
					const StringValuePtr id = newIdLit->getValue();
					const TypePtr oldType = dynamic_pointer_cast<const GenericType>(oldTypeLit->getType())->getTypeParameter(0);
//					for_each(replacements, [&](std::pair<const NodePtr, NodePtr> n) {
//					});

					const core::TypePtr memberTy = newType->getTypeOfMember(id);

					if(!memberTy) { // someone requested a field which has been removed from the struct -> will be deleted anyways
						return newCall;
					}

					if(memberTy != oldType) { // need to update the type argument of the call
						NodePtr retExpr = builder.callExpr( builder.refType(memberTy), BASIC.getCompositeRefElem(),
								toVector<ExpressionPtr>(newCall->getArgument(0), newIdLit, builder.getTypeLiteral(memberTy) ));

						copyAnnotations(newCall, retExpr);
						return retExpr;

					}
				}
			}

			NodePtr ret;
			if(updateReturnVal(newCall, ret)) {
				copyAnnotations(callExpr, ret);
				return ret;
			}

			// shortcut to avoid replacing functions which return cl_ types
			// have to be replaced elsewhere coz we do not know the correct replacement here
/*			if(newCall->getType()->toString().find("array<_cl_") != string::npos) {
//				std::cout << newCall->getFunctionExpr() << "(" << newCall->getArguments() << ")" << std::endl;
				return newCall;
			}*/

			if(newCall->getType()->toString().find("array<_cl_") == string::npos) {
				// remove remaining calls using cl_ objects, just return the zero-element
				const auto& list = newCall->getArguments();
				for(auto I = list.begin(); I != list.end(); ++I) {
					// extra check to not remove e.g. sizeof
					if(const LiteralPtr lit = dynamic_pointer_cast<const Literal>(newCall->getFunctionExpr())) {
						if(lit->getStringValue().find("sizeof") == string::npos && lit->getStringValue().find("composite.ref.elem") == string::npos ) {
							if((*I)->getType()->toString().find("array<_cl_") != string::npos) {
								return getZeroElem(newCall->getType());
							}
						}
					}
				}
			}
		}

	}

/*	if(const ExpressionPtr callExpr = dynamic_pointer_cast<const Expression>(element))
	if(callExpr->getType()->toString().find("_cl_kernel") != string::npos)
		std::cout << "\nElephants of the Apokalypse\n" << callExpr->getType() << " " << callExpr << std::endl;*/

	assert(element);
	NodePtr ret = element->substitute(builder.getNodeManager(), *this);
/*
	if(const CallExprPtr newCall = dynamic_pointer_cast<const CallExpr>(ret)) {
		if(newCall->getType()->toString().find("type") != string::npos)
		if(BASIC.isCompositeRefElem(newCall->getFunctionExpr()))
		std::cout << "\n   Riders of the Apokalypse\n" << newCall->getType() << " " << newCall << "(\n\t" <<
			newCall->getArgument(0)->getType() << " " << newCall->getArgument(0) << ", \n\t" <<
			newCall->getArgument(1)->getType() << " " << newCall->getArgument(1) << ", \n\t" <<
			newCall->getArgument(2)->getType() << " " << newCall->getArgument(2) << ")\n" << std::endl;
	}
*/

	copyAnnotations(element, ret);
	return ret;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
