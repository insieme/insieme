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

#include "insieme/core/checks/typechecks.h"

#include "insieme/core/type_utils.h"
#include "insieme/core/lang_basic.h"

namespace insieme {
namespace core {
namespace checks {

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

	// TODO: only allow more specialized type - not unified!
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

namespace {

	unsigned getNumRefs(const TypePtr& type) {
		unsigned count = 0;
		TypePtr cur = type;
		while (cur->getNodeType() == NT_RefType) {
			count++;
			cur = static_pointer_cast<const RefType>(cur)->getElementType();
		}
		return count;
	}

}

OptionalMessageList RefCastCheck::visitCastExpr(const CastExprAddress& address) {

	OptionalMessageList res;

	// check whether it is a build-in literal
	TypePtr src = address->getSubExpression()->getType();
	TypePtr trg = address->getType();
	unsigned srcCount = getNumRefs(src);
	unsigned trgCount = getNumRefs(trg);

	if (srcCount > trgCount) {
		add(res, Message(address,
				EC_TYPE_REF_TO_NON_REF_CAST,
				format("Casting reference type %s to non-reference type %s",
						toString(*src).c_str(),
						toString(*trg).c_str()),
				Message::ERROR));
	}

	if (srcCount < trgCount) {
		add(res, Message(address,
				EC_TYPE_NON_REF_TO_REF_CAST,
				format("Casting non-reference type %s to reference type %s",
						toString(*src).c_str(),
						toString(*trg).c_str()),
				Message::ERROR));
	}

	return res;
}


OptionalMessageList CastCheck::visitCastExpr(const CastExprAddress& address) {

	OptionalMessageList res;

	TypePtr source = address->getSubExpression()->getType();
	TypePtr target = address->getType();
	TypePtr src = source;
	TypePtr trg = target;

	while (src->getNodeType() == trg->getNodeType()) {
		switch(src->getNodeType()) {
		case NT_ArrayType:
		case NT_ChannelType:
		case NT_RefType:
		case NT_VectorExpr:
			src = static_pointer_cast<const Type>(src->getChildList()[0]);
			trg = static_pointer_cast<const Type>(trg->getChildList()[0]);
			break;
		case NT_TupleType:
		case NT_FunctionType:
		case NT_RecType:
			if (*src != *trg) {
				add(res, Message(address,
						EC_TYPE_ILLEGAL_CAST,
						format("Casting between incompatible types %s and %s",
								toString(*source).c_str(),
								toString(*target).c_str()),
						Message::ERROR));
				return res;
			}
		case NT_GenericType:
			// this cast is allowed (for now)
			return res;
		default:
			break;
		}
	}

	// types are not of same kind => illegal cast
	add(res, Message(address,
		EC_TYPE_ILLEGAL_CAST,
		format("Casting between incompatible types %s and %s",
				toString(*source).c_str(),
				toString(*target).c_str()),
		Message::ERROR));

	return res;
}


#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
