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

core::ExpressionPtr getVarOutOfCrazyInspireConstruct1(const core::ExpressionPtr& arg, const core::IRBuilder& builder) {
// remove stuff added by (void*)&
	core::CallExprPtr stripped = arg.isa<core::CallExprPtr>();

	if (!stripped) {
		return arg;
	}

	auto funExpr = stripped->getFunctionExpr();
	if(builder.getNodeManager().getLangBasic().isScalarToArray(funExpr) ||
			builder.getNodeManager().getLangBasic().isRefDeref(funExpr) || builder.getNodeManager().getLangBasic().isRefReinterpret(funExpr)) {
		return getVarOutOfCrazyInspireConstruct1(stripped->getArgument(0), builder);
	}

	return arg;
}


}
}
}
