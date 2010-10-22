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

#include "checks/typechecks.h"

#include "type_utils.h"
#include "lang_basic.h"

namespace insieme {
namespace core {
namespace checks {



CheckPtr getFullCheck() {
	// TODO: extend list of checks
	return makeVisitOnce(
			combine(toVector<CheckPtr>(
					make_check<CallExprTypeCheck>(),
					make_check<DeclarationStmtTypeCheck>(),
					make_check<WhileConditionTypeCheck>(),
					make_check<IfConditionTypeCheck>(),
					make_check<SwitchExpressionTypeCheck>(),
					make_check<BuildInLiteralCheck>()
			)
	));
}

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)


OptionalMessageList CallExprTypeCheck::visitCallExpr(const CallExprAddress& address) {

	NodeManager manager;
	OptionalMessageList res;

	// obtain function type ...
	TypePtr funType = address->getFunctionExpr()->getType();
	assert( address->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Illegal function expression!");

	FunctionTypePtr functionType = CAST(FunctionType, funType);
	TupleTypePtr argumentType = functionType->getArgumentType();
	TypePtr returnType = functionType->getReturnType();

	// Obtain argument type
	vector<TypePtr> types;
	transform(address->getArguments(), back_inserter(types), [](const ExpressionPtr& cur) {
		return cur->getType();
	});
	TupleTypePtr parameterType = TupleType::get(manager, types);

	// 1) check number of arguments
	int numArguments = argumentType->getElementTypes().size();
	int numParameter = parameterType->getElementTypes().size();
	if (numArguments != numParameter) {
		add(res, Message(address,
						EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS,
						format("Wrong number of arguments - expected: %d, actual: %d", numArguments, numParameter),
						Message::ERROR));
		return res;
	}

	// 2) check types of arguments => by computing most general unifier
	auto mgu = unify(manager, argumentType, parameterType);
	if (!mgu) {
		add(res, Message(address,
						EC_TYPE_INVALID_ARGUMENT_TYPE,
						format("Invalid argument type(s) - expected: %s, actual: %s - %s",
								toString(*argumentType).c_str(),
								toString(*parameterType).c_str(),
								toString(*functionType).c_str()),
						Message::ERROR));
		return res;
	}

	// 3) check return type - which has to be unifyable with modified function return value.
	TypePtr retType = mgu->applyTo(manager, returnType);
	TypePtr resType = address->getType();

	if (*retType != *resType) {
		add(res, Message(address,
						EC_TYPE_INVALID_RETURN_TYPE,
						format("Invalid return type - expected: %s, actual: %s",
								toString(*retType).c_str(),
								toString(*resType).c_str()),
						Message::ERROR));
		return res;
	}
	return res;
}



OptionalMessageList DeclarationStmtTypeCheck::visitDeclarationStmt(const DeclarationStmtAddress& address) {

	NodeManager manager;
	OptionalMessageList res;

	DeclarationStmtPtr declaration = address.getAddressedNode();

	// just test whether same type is on both sides
	TypePtr variableType = declaration->getVariable()->getType();
	TypePtr initType = declaration->getInitialization()->getType();

	if (*variableType != *initType) {
		add(res, Message(address,
						EC_TYPE_INVALID_INITIALIZATION_EXPR,
						format("Invalid type of initial value - expected: %s, actual: %s",
								toString(*variableType).c_str(),
								toString(*initType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList IfConditionTypeCheck::visitIfStmt(const IfStmtAddress& address) {

	NodeManager manager;
	OptionalMessageList res;

	TypePtr conditionType = address->getCondition()->getType();

	if (*conditionType != lang::TYPE_BOOL_VAL) {
		add(res, Message(address,
						EC_TYPE_INVALID_CONDITION_EXPR,
						format("Invalid type of condition expression - expected: %s, actual: %s",
								toString(*lang::TYPE_BOOL_PTR).c_str(),
								toString(*conditionType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList WhileConditionTypeCheck::visitWhileStmt(const WhileStmtAddress& address) {

	OptionalMessageList res;
	TypePtr conditionType = address->getCondition()->getType();
	if (*conditionType != lang::TYPE_BOOL_VAL) {
		add(res, Message(address,
						EC_TYPE_INVALID_CONDITION_EXPR,
						format("Invalid type of condition expression - expected: %s, actual: %s",
								toString(*lang::TYPE_BOOL_PTR).c_str(),
								toString(*conditionType).c_str()),
						Message::ERROR));
	}
	return res;
}


OptionalMessageList SwitchExpressionTypeCheck::visitSwitchStmt(const SwitchStmtAddress& address) {

	OptionalMessageList res;
	TypePtr switchType = address->getSwitchExpr()->getType();
	if (!lang::isIntType(*switchType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_SWITCH_EXPR,
						format("Invalid type of switch expression - expected: integral type, actual: %s",
								toString(*switchType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList BuildInLiteralCheck::visitLiteral(const LiteralAddress& address) {

	OptionalMessageList res;

	// check whether it is a build-in literal
	LiteralPtr buildIn = lang::getBuildInForValue(address->getValue());
	if (!buildIn) {
		return res;
	}

	if (!isUnifyable(buildIn->getType(),address->getType())) {
		add(res, Message(address,
				EC_TYPE_INVALID_TYPE_OF_LITERAL,
				format("Deviating type of build in literal %s - expected: %s, actual: %s",
						address->getValue().c_str(),
						toString(*buildIn->getType()).c_str(),
						toString(*address->getType()).c_str()),
				Message::WARNING));
	}
	return res;
}

#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
