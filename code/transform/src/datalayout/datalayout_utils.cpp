/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/transform/datalayout/datalayout_utils.h"

#include "insieme/analysis/defuse_collect.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;


ExpressionAddress extractVariable(ExpressionAddress expr) {

	if(expr->getNodeType() == NT_Variable) // return variable
		return expr;

	if(expr->getNodeType() == NT_Literal && expr->getType()->getNodeType() == NT_RefType) // return literal, e.g. global variable
		return expr;

//	if(CallExprAddress call = expr.isa<CallExprAddress>()) {
//		const lang::BasicGenerator& gen = expr->getNodeManagerPtr()->getLangBasic();
//		if(gen.isSubscriptOperator(call->getFunctionExpr()))
//			return expr;
//
//		if(gen.isCompositeRefElem(call->getFunctionExpr())) {
//			return expr;
//		}
//	}

	if(CastExprAddress cast = expr.isa<CastExprAddress>())
		return extractVariable(cast->getSubExpression());

	if(CallExprAddress call = expr.isa<CallExprAddress>()){
		return extractVariable(call->getArgument(0)); // crossing my fingers that that will work ;)

	}

	return expr;
}

ExpressionAddress extractNonTupleVariable(ExpressionAddress expr) {
	ExpressionAddress ret = extractVariable(expr);

	pattern::TreePattern tupleType = pattern::aT(pattern::irp::tupleType(*pattern::any));

	if(tupleType.matchAddress(ret->getType())) {
		return ExpressionAddress();
	}

	return ret;
}


ExpressionPtr getBaseExpression(ExpressionPtr expr) {
	ExpressionPtr baseExpr;
	ia::RefList&& refs = ia::collectDefUse(expr);

	auto first = refs.begin();
	if(first != refs.end())
		baseExpr = (*first)->getBaseExpression();

	return baseExpr;
}

/*
 * Returns either the expression itself or the expression inside a nest of ref.deref calls
 */
ExpressionAddress tryRemoveDeref(const ExpressionAddress& expr) {
	NodeManager& mgr = expr->getNodeManager();
	if(const CallExprAddress& call = expr.isa<CallExprAddress>()) {
		if(mgr.getLangBasic().isRefDeref(call->getFunctionExpr()))
			return tryRemoveDeref(call->getArgument(0));
	}
	return expr;
}

/*
 * Checks if the variable addressed in var is the same as the one declared in decl. Save to be used in programs with multiple scopes as long as var and decl
 * refer to the same base address
 */
bool sameAsDecl(const ExpressionAddress var, const ExpressionAddress decl) {
	if(*var != *decl)
		return false;

	ExpressionAddress varDecl = getDeclaration(var);

	if(varDecl == decl)
		return true;

	return false;
}

core::ExpressionAddress getDeclaration(const NodeAddress scope, const ExpressionAddress var) {
	// if the variable is a literal, its a global variable and should therefore be the root
	if(LiteralAddress lit = var.isa<LiteralAddress>()) {
		if(lit.getType().isa<RefTypeAddress>()) { // check if literal was a global variable in the input code
			return var;
		}
	}

	vector<NodeAddress> childAddresses = scope.getChildAddresses();
	for(auto I = childAddresses.rbegin(); I != childAddresses.rend(); ++I) {
		NodeAddress child = *I;

		// look for declaration in parameter lists
		if(LambdaAddress lambda = child.isa<LambdaAddress>()) {
			for(ExpressionAddress param : lambda->getParameters()) {
				if(*param == *var)
					return param;
			}
		}

		// look for declaration in siblings of the scope
		if(DeclarationStmtAddress decl = child.isa<DeclarationStmtAddress>()) {
			if(*(decl->getVariable()) == *var)
				return decl->getVariable();
		}
	}

	//check if we already reached the top
	if(scope.getDepth() <= 1)
		return ExpressionAddress();

	return getDeclaration(scope.getParentAddress(), var);
}

core::ExpressionAddress getDeclaration(const ExpressionAddress& var) {
	if(!var)
		return ExpressionAddress();
	return getDeclaration(var.getParentAddress(), var);
}

NodeAddress getRootVariable(const NodeAddress scope, NodeAddress var) {

//std::cout << "\nlooking for var " << *var << std::endl;
	// if the variable is a literal, its a global variable and should therefore be the root
	if(LiteralAddress lit = var.isa<LiteralAddress>()) {
		if(lit.getType().isa<RefTypeAddress>()) { // check if literal was a global variable in the input code
//std::cout << "found literal " << *var << std::endl;
			return var;
		}
	}

	// search in declaration in siblings
	NodeManager& mgr = var.getNodeManager();

//	pattern::TreePattern localOrGlobalVar = pirp::variable() | pirp::literal(pirp::refType(pattern::any), pattern::any);
	pattern::TreePattern valueCopy = pattern::var("val", pirp::variable()) |
			pirp::callExpr(mgr.getLangBasic().getRefDeref(), pattern::var("val", pirp::variable())) |
			pirp::callExpr(mgr.getLangBasic().getRefNew(), pattern::var("val", pirp::variable())) |
			pirp::callExpr(mgr.getLangBasic().getRefVar(), pattern::var("val", pirp::variable()));

	vector<NodeAddress> childAddresses = scope.getChildAddresses();
	for(auto I = childAddresses.rbegin(); I != childAddresses.rend(); ++I) {
		NodeAddress child = *I;

		if(child.getDepth() > 4) {
			if(LambdaAddress lambda = child.isa<LambdaAddress>()) {
//	std::cout << "Lambda: " << lambda << "\n var " << var << std::endl;
				// if var is a parameter, continue search for declaration of corresponding argument in outer scope

//	for(int i = 1; i <= 4; ++i)
//		std::cout << "\nlp: " << utils::whatIs(lambda.getParentNode(i)) << std::endl;

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

		if(DeclarationStmtAddress decl = child.isa<DeclarationStmtAddress>()) {
			if(*(decl->getVariable()) == *var) {
				// check if init expression is another variable
				if(pattern::AddressMatchOpt valueInit = valueCopy.matchAddress(decl->getInitialization())) {
					// if so, continue walk with other variable
					return getRootVariable(scope, valueInit->getVarBinding("val").getValue());
				}
//std::cout << "found decl of " << *var << std::endl;
				// if init is no other varable, the root is found
				return decl->getVariable();
			}
		}

		if(CallExprAddress call = var.isa<CallExprAddress>()) {
//std::cout << "\ncalling " << *call << std::endl;//"\narg0: " << *call->getArgument(0) << "\nvar: " << *extractVariable(call->getArgument(0)) << std::endl;
			if(call->getNodeManager().getLangBasic().isBuiltIn(call->getFunctionExpr())) {
//std::cout << "\tthing: " << *call->getFunctionExpr() << std::endl;
				return getRootVariable(scope, extractVariable(call->getArgument(0))); // crossing my fingers that that will work ;)
			}
		}

	}

	//check if we already reached the top
	if(scope.getDepth() <= 1)
		return NodeAddress();

	//compound expressions may not open a new scope, therefore declaration can be in the parent
	return getRootVariable(scope.getParentAddress(), var);
}

ExpressionAddress removeRefVar(ExpressionAddress refVar) {
//	std::cout << "remvoing var from " << refVar << std::endl;

	if(core::analysis::isCallOf(refVar, refVar->getNodeManager().getLangBasic().getRefVar())) {
//		dumpPretty(refVar.as<CallExprPtr>()[0]);
		return refVar.as<CallExprAddress>()[0];
	}

	return refVar;
}

TypePtr removeRef(TypePtr refTy) {
	if(RefTypePtr r = refTy.isa<RefTypePtr>())
		return r->getElementType();
	return refTy;
}

TypePtr removeRefArray(TypePtr refTy) {
	if(RefTypePtr r = refTy.isa<RefTypePtr>())
		if(ArrayTypePtr a = r->getElementType().isa<ArrayTypePtr>())
			return a->getElementType();
	return refTy;
}

TypePtr getBaseType(TypePtr type, StringValuePtr field) {
	if(RefTypePtr ref = type.isa<RefTypePtr>()) {
		return getBaseType(ref->getElementType(), field);
	}

	if(ArrayTypePtr arr = type.isa<ArrayTypePtr>()) {
		return getBaseType(arr->getElementType(), field);
	}

	StructTypePtr str = type.isa<StructTypePtr>();
	if(field && str) {
		return getBaseType(str->getTypeOfMember(field), field);
	}

	return type;
}

ExpressionPtr valueAccess(ExpressionPtr thing, ExpressionPtr index, StringValuePtr field, ExpressionPtr vecIndex) {
	NodeManager& mgr = thing->getNodeManager();
	IRBuilder builder(mgr);

	if(RefTypePtr ref = thing->getType().isa<RefTypePtr>()) {
		if(ref->getElementType().isa<ArrayTypePtr>()) {
			if(!index)
				return thing;

			return valueAccess(builder.deref(builder.arrayRefElem(thing, index)), index, field, vecIndex);
		}

		if(ref->getElementType().isa<VectorTypePtr>()) {
			if(!vecIndex)
				return thing;

			return valueAccess(builder.deref(builder.arrayRefElem(thing, vecIndex)), index, field, vecIndex);
		}

		return valueAccess(builder.deref(thing), index, field, vecIndex);
	}

	if(thing->getType().isa<ArrayTypePtr>()) {
		if(!index)
			return thing;

		return valueAccess(builder.arraySubscript(thing, index), index, field, vecIndex);
	}

	if(thing->getType().isa<VectorTypePtr>()) {
		if(!vecIndex)
			return thing;

		return valueAccess(builder.arraySubscript(thing, vecIndex), index, field, vecIndex);
	}

	if(thing->getType().isa<StructTypePtr>()) {
		if(!field)
			return thing;

		return valueAccess(builder.accessMember(thing, field), index, field, vecIndex);
	}

	return thing;
}

ExpressionPtr refAccess(ExpressionPtr thing, ExpressionPtr index, StringValuePtr field, ExpressionPtr vecIndex) {
	NodeManager& mgr = thing->getNodeManager();
	IRBuilder builder(mgr);

	if(RefTypePtr ref = thing->getType().isa<RefTypePtr>()) {

		if(builder.getLangBasic().isPrimitive(ref->getElementType()))
			return thing;

		if(ref->getElementType().isa<ArrayTypePtr>()) {
			if(!index)
				return thing;

			return refAccess(builder.arrayRefElem(thing, index), index, field, vecIndex);
		}

		if(ref->getElementType().isa<VectorTypePtr>()) {
			if(!vecIndex)
				return thing;

			return refAccess(builder.arrayRefElem(thing, vecIndex), index, field, vecIndex);
		}

		if(ref->getElementType().isa<StructTypePtr>()) {
			if(!field)
				return thing;

			return refAccess(builder.refMember(thing, field), index, field, vecIndex);
		}

		return refAccess(builder.deref(thing), index, field, vecIndex);
	}

	if(thing->getType().isa<StructTypePtr>()) {
		if(!field)
			return thing;

		return refAccess(builder.accessMember(thing, field), index, field, vecIndex);
	}

	return thing;

}


pattern::TreePattern declOrAssignment(pattern::TreePattern lhs, pattern::TreePattern rhs) {
	pattern::TreePattern assign = pattern::var("assignment", pirp::assignment(lhs, rhs));
	pattern::TreePattern decl = pattern::var("decl", pirp::declarationStmt(lhs, pirp::refVar(rhs)));

	return assign | decl;
}

StatementAddress getStatementReplacableParent(NodeAddress toBeReplacedByAStatement) {
	while(!toBeReplacedByAStatement.getParentAddress().isa<CompoundStmtAddress>())
		toBeReplacedByAStatement = toBeReplacedByAStatement.getParentAddress();

	return toBeReplacedByAStatement.as<StatementAddress>();
}

bool validVar(ExpressionPtr toTest) {
	return toTest && (toTest.isa<VariablePtr>() || (toTest.isa<LiteralPtr>() && toTest->getType().isa<RefTypePtr>()));
}

bool isInsideJob(NodeAddress toTest) {
	if(toTest.getNodeType() == NT_JobExpr)
		return true;

	if(toTest.isRoot())
		return false;

	return isInsideJob(toTest.getParentAddress());
}

StatementPtr allocTypeUpdate(const StatementPtr& stmt, pattern::TreePattern& oldStructTypePattern, pattern::TreePattern& newStructTypePattern) {
	TypePtr oldType, newType;
	NodeManager& mgr = stmt->getNodeManager();


	if(const DeclarationStmtPtr& decl = stmt.isa<DeclarationStmtPtr>()) {
		const VariablePtr& var = decl->getVariable();
		const CallExprPtr& init = decl->getInitialization().isa<CallExprPtr>();

		// check if init is a call and its type fits the variable
		if(!init | types::isSubTypeOf(var->getType(), init->getType()))
			return stmt;

		// if init is a call with wrong, try to fix the type
		pattern::MatchOpt initMatch = oldStructTypePattern.matchPointer(init[0]->getType());
		pattern::MatchOpt varMatch = newStructTypePattern.matchPointer(var->getType());
		ExpressionPtr nElem;
		if(initMatch && varMatch) {
			oldType = initMatch.get()["structType"].getValue().as<TypePtr>();
			newType = varMatch.get()["structType"].getValue().as<TypePtr>();
		}
	}

	if(const CallExprPtr& assign = stmt.isa<CallExprPtr>()) {
		if(!core::analysis::isCallOf(assign, mgr.getLangBasic().getRefAssign()))
			return stmt;

		const ExpressionPtr& lhs = assign[0];
		const ExpressionPtr& rhs = assign[1];

		const RefTypePtr& lhsTy = lhs->getType().as<RefTypePtr>();

		// check if type is consistent
		if(lhsTy->getElementType() == rhs->getType())
			return stmt;

		// if type is not consistent, try to update the type
		pattern::MatchOpt rhsMatch = oldStructTypePattern.matchPointer(rhs->getType());
		pattern::MatchOpt lhsMatch = newStructTypePattern.matchPointer(lhsTy->getElementType());
		ExpressionPtr nElem;
		if(rhsMatch && lhsMatch) {
			oldType = rhsMatch.get()["structType"].getValue().as<TypePtr>();
			newType = lhsMatch.get()["structType"].getValue().as<TypePtr>();
		}
	}

	if(oldType)
		return core::transform::replaceAllGen(mgr, stmt, oldType, newType, false);

	return stmt;
}

ExpressionAddress removeMemLocationCreators(const ExpressionAddress& expr) {
	if(const CallExprAddress call = expr.isa<CallExprAddress>()) {

		pattern::TreePattern memLocCreator = pirp::refVar(pattern::any) | pirp::refNew(pattern::any) | pirp::refLoc(pattern::any);
		if(memLocCreator.matchAddress(expr)) {
			return removeMemLocationCreators(call[0]);
		}
	}

	return expr;
}


bool isRefStruct(ExpressionPtr expr, RefTypePtr structType) {
	TypePtr type = expr->getType();

	if(!type.isa<RefTypePtr>())
		return false;

	pattern::TreePattern containsStructType = pattern::aT(pattern::atom(structType));

	if(containsStructType.match(type))
		return true;

	return false;
}

pattern::TreePattern optionalDeref(const pattern::TreePattern& mayToBeDerefed) {
	return mayToBeDerefed | pirp::refDeref(mayToBeDerefed) | pirp::scalarToArray(mayToBeDerefed);
}


} // datalayout
} // transform
} // insieme
