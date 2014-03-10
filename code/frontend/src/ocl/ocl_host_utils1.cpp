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

#include "insieme/frontend/ocl/ocl_host_utils1.h"
#include "insieme/core/printer/pretty_printer.h"

namespace icp = insieme::core::pattern;
namespace pirp = insieme::core::pattern::irp;
using namespace insieme::core;

namespace insieme {
namespace frontend {
namespace ocl {
namespace utils {

/*
 * Returns either the expression itself or the first argument if expression was a call to function
 */
ExpressionAddress tryRemove(const ExpressionPtr& function, const ExpressionAddress& expr) {
	ExpressionAddress e = expr;
	while(const CallExprAddress& call = dynamic_address_cast<const CallExpr>(e)) {
		if(call->getFunctionExpr() == function)
			e = call->getArgument(0);
		else
			break;
	}
	return e;
}

ExpressionPtr tryRemove(const ExpressionPtr& function, const ExpressionPtr& expr) {
	ExpressionPtr e = expr;
	while(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(e)) {
		if(call->getFunctionExpr() == function)
			e = call->getArgument(0);
		else
			break;
	}
	return e;
}

/*
 * Returns either the expression itself or the expression inside a nest of ref.new/ref.var calls
 */
ExpressionAddress tryRemoveAlloc(const ExpressionAddress& expr) {
	NodeManager& mgr = expr->getNodeManager();
	if(const CallExprAddress& call = dynamic_address_cast<const CallExpr>(expr)) {
		if(mgr.getLangBasic().isRefNew(call->getFunctionExpr()) || mgr.getLangBasic().isRefVar(call->getFunctionExpr()))
			return tryRemoveAlloc(call->getArgument(0));
	}
	return expr;
}
ExpressionPtr tryRemoveAlloc(const ExpressionPtr& expr) {
	NodeManager& mgr = expr->getNodeManager();
	if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(expr)) {
		if(mgr.getLangBasic().isRefNew(call->getFunctionExpr()) || mgr.getLangBasic().isRefVar(call->getFunctionExpr()))
			return tryRemoveAlloc(call->getArgument(0));
	}
	return expr;
}

/*
 * Builds a ref.deref call around an expression if the it is of type ref<ref<'a>>
 */
core::ExpressionPtr removeDoubleRef(const core::ExpressionPtr& expr){
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		 // on non array types remove also a single ref
		if(refTy->getElementType()->getNodeType() == core::NT_RefType || refTy->toString().find("array") == string::npos) {
			core::NodeManager& mgr = expr->getNodeManager();
			const core::IRBuilder& builder(mgr);
			const lang::BasicGenerator& gen = builder.getLangBasic();
			return builder.callExpr(refTy->getElementType(), gen.getRefDeref(), expr);
		}
	}
	return expr;
}

/*
 * removes the returns 'a if type is ref<'a>, type otherwise
 */
core::TypePtr removeSingleRef(const core::TypePtr& type){
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
			return refTy->getElementType();
	}
	return type;
}

/*
 * takes a type ref<array<vector<'b,#l>,1>> and creates ref<array<'b>,1> from it
 */
core::TypePtr vectorArrayTypeToScalarArrayType(core::TypePtr arrayTy, const core::IRBuilder& builder) {
	if(const core::RefTypePtr refTy = dynamic_pointer_cast<const core::RefType>(arrayTy)) {
		if(const core::ArrayTypePtr arrTy = dynamic_pointer_cast<const core::ArrayType>(refTy->getElementType()))
			if(const core::VectorTypePtr vecTy = dynamic_pointer_cast<const core::VectorType>(arrTy->getElementType())) {
				return builder.refType(builder.arrayType(vecTy->getElementType()));
			}
	}

	return arrayTy;
}

/*
 * takes the expression passed to size (in bytes) and tries to extract the size in number of elements as well as the type to be used
 */
bool extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type, bool foundMul) {
	// get rid of casts
	NodePtr uncasted = arg;
	while (uncasted->getNodeType() == core::NT_CastExpr) {
		uncasted = static_pointer_cast<CastExprPtr>(uncasted)->getType();
	}

	if (const CallExprPtr call = dynamic_pointer_cast<const CallExpr> (uncasted)) {
		// check if there is a multiplication
		if(call->getFunctionExpr()->toString().find(".mul") != string::npos && call->getArguments().size() == 2) {
			IRBuilder builder(arg->getNodeManager());
			// recursively look into arguments of multiplication
			if(extractSizeFromSizeof(call->getArgument(0), size, type, true)) {
				if(size)
					size = builder.callExpr(call->getType(), call->getFunctionExpr(), size, call->getArgument(1));
				else
					size = call->getArgument(1);
				return true;
			}
			if(extractSizeFromSizeof(call->getArgument(1), size, type, true)){
				if(size)
					size = builder.callExpr(call->getType(), call->getFunctionExpr(), call->getArgument(0), size);
				else
					size = call->getArgument(0);
				return true;
			}
		}
		// check if we reached a sizeof call
		if (call->toString().substr(0, 6).find("sizeof") != string::npos) {
			// extract the type to be allocated
			type = dynamic_pointer_cast<GenericTypePtr>(call->getArgument(0)->getType())->getTypeParameter(0);
			assert(type && "Type could not be extracted!");

			if(!foundMul){ // no multiplication, just sizeof alone is passed as argument -> only one element
				IRBuilder builder(arg->getNodeManager());
				size = builder.literal(arg->getNodeManager().getLangBasic().getUInt8(), "1");
				return true;
			}

			return true;
		}
	}
	return false;
}

/*
 * Returns either the expression itself or the expression inside a nest of ref.deref calls
 */
ExpressionAddress tryRemoveDeref(const ExpressionAddress& expr) {
	NodeManager& mgr = expr->getNodeManager();
	if(const CallExprAddress& call = dynamic_address_cast<const CallExpr>(expr)) {
		if(mgr.getLangBasic().isRefDeref(call->getFunctionExpr()))
			return tryRemoveDeref(call->getArgument(0));
	}
	return expr;
}

ExpressionAddress extractVariable(ExpressionAddress expr) {
	const lang::BasicGenerator& gen = expr->getNodeManagerPtr()->getLangBasic();

	if(expr->getNodeType() == NT_Variable) // return variable
		return expr;

	if(expr->getNodeType() == NT_Literal) // return literal, e.g. global variable
		return expr;

	if(CallExprAddress call = dynamic_address_cast<const CallExpr>(expr)) {
		if(gen.isSubscriptOperator(call->getFunctionExpr()))
			return expr;

		if(gen.isCompositeRefElem(call->getFunctionExpr())) {
			return expr;
		}
	}

	if(CastExprAddress cast = dynamic_address_cast<const CastExpr>(expr))
		return extractVariable(cast->getSubExpression());

	if(CallExprAddress call = dynamic_address_cast<const CallExpr>(expr)){
//		if (gen.isRefDeref(call->getFunctionExpr())){
			return extractVariable(call->getArgument(0)); // crossing my fingers that that will work ;)
//		}

	}

	return expr;
}

/*
 * Builds a ref.deref call around an expression if the it is of ref-type
 */
core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr) {
	NodeManager& mgr = expr.getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = expr->getNodeManager().getLangBasic();

	// core::ExpressionPtr retExpr = expr;
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), gen.getRefDeref(), expr);
	}
	return expr;
}

NodeAddress getRootVariable(NodeAddress scope, NodeAddress var) {
	// if the variable is a literal, its a global variable and should therefore be the root
	if(var.isa<LiteralAddress>()) {
//std::cout << "found literal " << *var << std::endl;
		return var;
	}

	// search in declaration in siblings
	NodeManager& mgr = var.getNodeManager();

	icp::TreePatternPtr localOrGlobalVar = pirp::variable() | pirp::literal(pirp::refType(icp::any), icp::any);
	icp::TreePatternPtr valueCopy = icp::var("val", pirp::variable()) |
			pirp::callExpr(mgr.getLangBasic().getRefDeref(), icp::var("val", pirp::variable())) |
			pirp::callExpr(mgr.getLangBasic().getRefNew(), icp::var("val", pirp::variable())) |
			pirp::callExpr(mgr.getLangBasic().getRefVar(), icp::var("val", pirp::variable()));
	icp::TreePatternPtr valueCopyCast = valueCopy | pirp::castExpr(icp::any, valueCopy);
	icp::TreePatternPtr assign = pirp::callExpr((mgr.getLangBasic().getRefAssign()), //	single(icp::any));
			icp::var("lhs", pirp::variable()) << valueCopyCast);

//std::cout << "\nscope: " << (scope.getChildAddresses().size()) << std::endl;

	vector<NodeAddress> childAddresses = scope.getChildAddresses();
	for(auto I = childAddresses.rbegin(); I != childAddresses.rend(); ++I) {
//std::cout << "iterating " << *I << std::endl;

		NodeAddress child = *I;
//std::cout << "Tolles I " << NodePtr(child) << std::endl;

		/* will be implplemented when a propper testcase can be found
		if(CallExprAddress call = dynamic_address_cast<const CallExpr>(child)) {
			// if there is an assignment, continue with the variable found at the right hand side
			if(AddressMatchOpt assignment = assign->matchAddress(call)){
std::cout << "assigning  " << printer::PrettyPrinter(assignment->getVarBinding("val").getValue()) << std::endl;
std::cout << " to " << printer::PrettyPrinter(assignment->getVarBinding("lhs").getValue()) << std::endl;
				if(assignment->getVarBinding("lhs").getValue() == var) {
					return getRootVariable(scope, assignment->getVarBinding("val").getValue());
				}
			}
		}
		*/

		if(child.getDepth() >= 4) {
			if(LambdaAddress lambda = dynamic_address_cast<const Lambda>(child)) {
	//std::cout << "Lambda: " << lambda << "\n var " << var << std::endl;
				// if var is a parameter, continue search for declaration of corresponding argument in outer scope

	//for(int i = 1; i < 5; ++i)
	//	std::cout << "\nlp: " << lambda << " - " << printer::PrettyPrinter(lambda) << std::endl;

				CallExprAddress call = lambda.getParentAddress(4).as<CallExprAddress>();
				NodeAddress nextScope, nextVar;

				for_range(make_paired_range(call->getArguments(), lambda->getParameters()->getElements()),
						[&](const std::pair<const ExpressionAddress, const VariableAddress>& pair) {
					if(*var == *pair.second) {
						nextScope = call.getParentAddress(1);
						nextVar = tryRemoveDeref(pair.first);
						return;
					}
				});
				return getRootVariable(nextScope, nextVar);
			}
		}

		if(DeclarationStmtAddress decl = dynamic_address_cast<const DeclarationStmt>(child)) {
			if(*(decl->getVariable()) == *var) {
				// check if init expression is another variable
				if(icp::AddressMatchOpt valueInit = valueCopy->matchAddress(decl->getInitialization())) {
					// if so, continue walk with other variable
					return getRootVariable(scope, valueInit->getVarBinding("val").getValue());
				}
//std::cout << "found decl of " << *var << std::endl;
				// if init is no other varable, the root is found
				return decl->getVariable();
			}
		}
	}

	if(CallExprAddress call = dynamic_address_cast<const CallExpr>(var))
		return extractVariable(call->getArgument(0)); // crossing my fingers that that will work ;)


	//compound expressions may not open a new scope, therefore declaration can be in the parent
	return getRootVariable(scope.getParentAddress(), var);
}


NodeAddress getRootVariable(NodeAddress var) {
	// search in declaration in siblings
	NodeAddress parent = var.getParentAddress(1);
	return getRootVariable(parent, var);
}

core::ExpressionPtr getVarOutOfCrazyInspireConstruct(const core::ExpressionPtr& arg) {
	core::NodeManager& mgr = arg->getNodeManager();
	core::IRBuilder builder(mgr);
	const core::lang::BasicGenerator& gen = mgr.getLangBasic();

// remove stuff added by (void*)&
	core::CallExprPtr stripped = arg.isa<core::CallExprPtr>();

	if (!stripped) {
		return arg;
	}

	auto funExpr = stripped->getFunctionExpr();
	if(builder.getNodeManager().getLangBasic().isScalarToArray(funExpr) ||
			builder.getNodeManager().getLangBasic().isRefDeref(funExpr) || gen.isRefReinterpret(funExpr)) {
		return getVarOutOfCrazyInspireConstruct(stripped->getArgument(0));
	}

	return arg;
}

bool isNullPtr(const ExpressionPtr& expr) {
	const lang::BasicGenerator& gen = expr->getNodeManager().getLangBasic();

	// cast to void pointer
	if (gen.isRefNull(expr))
		return true;

	// null literal
	const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(expr);
	const ExpressionPtr idxExpr = cast ? cast->getSubExpression() : expr;
	const LiteralPtr idx = dynamic_pointer_cast<const Literal>(idxExpr);
	if(!!idx && idx->getValueAs<int>() == 0)
		return true;


	return false;
}

void refreshVariables(core::ExpressionPtr& localMemInit, core::VariableMap& varMapping, const core::IRBuilder& builder){
	core::visitDepthFirstOnce(localMemInit, core::makeLambdaVisitor([&](const core::NodePtr& node) {
		if(core::VariablePtr var = dynamic_pointer_cast<const core::Variable>(node))
		if(varMapping.find(var) == varMapping.end()) // variable does not have a replacement in map now
			varMapping[var] = builder.variable(var->getType());
	}));

	for_each(varMapping, [&](std::pair<core::VariablePtr, core::VariablePtr> replacement) {
//		std::cout << "\nreplaceinig " << replacement.first << " with " << replacement.second << std::endl;
		localMemInit = static_pointer_cast<const core::Expression>(
				core::transform::replaceAll(builder.getNodeManager(), localMemInit, replacement.first, replacement.second));
	});
}

}
}
}
}
