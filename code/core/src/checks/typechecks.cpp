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
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/analysis/type_variable_deduction.h"

namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)


OptionalMessageList KeywordCheck::visitGenericType(const GenericTypeAddress& address) {

	OptionalMessageList res;

	if ((address->getFamilyName() == "vector" && address->getNodeType()!=NT_VectorType) ||
		(address->getFamilyName() == "array" && address->getNodeType()!=NT_ArrayType) ||
		(address->getFamilyName() == "ref" && address->getNodeType()!=NT_RefType) ||
		(address->getFamilyName() == "channel" && address->getNodeType()!=NT_ChannelType)) {

		add(res, Message(address,
				EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD,
				format("Name of generic type %s is a reserved keyword.", toString(*address).c_str()),
				Message::WARNING));
	}
	return res;
}

OptionalMessageList CallExprTypeCheck::visitCallExpr(const CallExprAddress& address) {

	NodeManager manager;
	OptionalMessageList res;

	// obtain function type ...
	TypePtr funType = address->getFunctionExpr()->getType();
	assert( address->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Illegal function expression!");

	const FunctionTypePtr& functionType = CAST(FunctionType, funType);
	const TypeList& parameterTypes = functionType->getParameterTypes();
	const TypePtr& returnType = functionType->getReturnType();

	// Obtain argument type
	TypeList argumentTypes;
	transform(address->getArguments(), back_inserter(argumentTypes), [](const ExpressionPtr& cur) {
		return cur->getType();
	});

	// 1) check number of arguments
	int numParameter = parameterTypes.size();
	int numArguments = argumentTypes.size();
	if (numArguments != numParameter) {
		add(res, Message(address,
						EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS,
						format("Wrong number of arguments - expected: %d, actual: %d - function type: %s",
								numParameter,
								numArguments,
								toString(*functionType).c_str()),
						Message::ERROR));
		return res;
	}

	// 2) check types of arguments => using variable deduction
	auto substitution = analysis::getTypeVariableInstantiation(manager, address);

	if (!substitution) {
		TupleTypePtr argumentTuple = TupleType::get(manager, argumentTypes);
		TupleTypePtr parameterTuple = TupleType::get(manager, parameterTypes);
		add(res, Message(address,
						EC_TYPE_INVALID_ARGUMENT_TYPE,
						format("Invalid argument type(s) - expected: %s, actual: %s - function type: %s",
								toString(*parameterTuple).c_str(),
								toString(*argumentTuple).c_str(),
								toString(*functionType).c_str()),
						Message::ERROR));
		return res;
	}

	// 3) check return type - which has to be matched with modified function return value.
	TypePtr retType = substitution->applyTo(returnType);
	TypePtr resType = address->getType();

	if (*retType != *resType) {
		add(res, Message(address,
						EC_TYPE_INVALID_RETURN_TYPE,
						format("Invalid return type - expected: %s, actual: %s - function type: %s",
								toString(*retType).c_str(),
								toString(*resType).c_str(),
                                toString(*functionType).c_str()),
						Message::ERROR));
		return res;
	}
	return res;
}

OptionalMessageList FunctionTypeCheck::visitLambdaExpr(const LambdaExprAddress& address) {

	NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	// recreate type
	auto extractType = [](const VariablePtr& var) {
		return var->getType();
	};

	TypeList param;
	transform(address->getParameterList(), back_inserter(param), extractType);

	FunctionTypePtr isType = address->getLambda()->getType();
	TypePtr result = address->getLambda()->getType()->getReturnType();

	FunctionTypePtr funType = FunctionType::get(manager, param, result);
	if (*funType != *isType) {
		add(res, Message(address,
						EC_TYPE_INVALID_FUNCTION_TYPE,
						format("Invalid type of lambda expression - expected: %s, actual: %s",
								toString(*funType).c_str(),
								toString(*isType).c_str()),
						Message::ERROR));
		return res;
	}
	return res;
}


OptionalMessageList ReturnTypeCheck::visitLambda(const LambdaAddress& address) {
	OptionalMessageList res;

	// obtain return type of lambda
	const TypePtr& returnType = address->getType()->getReturnType();

	// search for all return statements and check type
	visitAllPrunable(address, makeLambdaAddressVisitor([&](const NodeAddress& cur)->bool {

		// check whether it is a a return statement
		if (cur->getNodeType() != NT_ReturnStmt) {
			// abort if this node is a expression or type
			NodeCategory category = cur->getNodeCategory();
			return !(category == NC_Type || category == NC_Expression);
		}

		const ReturnStmtAddress& returnStmt = static_address_cast<const ReturnStmt>(cur);
		const TypePtr& actualType = returnStmt->getReturnExpr()->getType();
		if (returnType != actualType) {
			add(res, Message(cur,
				EC_TYPE_INVALID_RETURN_VALUE_TYPE,
				format("Invalid return type - expected: %s, actual: %s",
						toString(*returnType).c_str(),
						toString(*actualType).c_str()),
				Message::ERROR));
		}

		return false;
	}, false));


	// EC_TYPE_MISSING_RETURN_STMT,

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

	const NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	TypePtr conditionType = address->getCondition()->getType();

	if (!manager.basic.isBool(conditionType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_CONDITION_EXPR,
						format("Invalid type of condition expression - expected: %s, actual: %s",
								toString(*manager.basic.getBool()).c_str(),
								toString(*conditionType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList WhileConditionTypeCheck::visitWhileStmt(const WhileStmtAddress& address) {

	const NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	TypePtr conditionType = address->getCondition()->getType();
	if (!manager.basic.isBool(conditionType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_CONDITION_EXPR,
						format("Invalid type of condition expression - expected: %s, actual: %s",
								toString(*manager.basic.getBool()).c_str(),
								toString(*conditionType).c_str()),
						Message::ERROR));
	}
	return res;
}


OptionalMessageList SwitchExpressionTypeCheck::visitSwitchStmt(const SwitchStmtAddress& address) {
	const NodeManager& manager = address->getNodeManager();

	OptionalMessageList res;
	TypePtr switchType = address->getSwitchExpr()->getType();
	if (!manager.basic.isInt(switchType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_SWITCH_EXPR,
						format("Invalid type of switch expression - expected: integral type, actual: %s",
								toString(*switchType).c_str()),
						Message::ERROR));
	}
	return res;
}


namespace {


	OptionalMessageList checkMemberAccess(const NodeAddress& address, const ExpressionPtr& structExpr, const IdentifierPtr& identifier, const TypePtr& elementType, bool isRefVersion) {

		OptionalMessageList res;

		// check whether it is a struct at all
		TypePtr exprType = structExpr->getType();
		if (isRefVersion) {
			if (exprType->getNodeType() == NT_RefType) {
				// extract element type
				exprType = static_pointer_cast<const RefType>(exprType)->getElementType();
			} else {
				// invalid argument => handled by argument check
				return res;
			}
		}

		// unroll recursive types if necessary
		if (exprType->getNodeType() == NT_RecType) {
			exprType = static_pointer_cast<const RecType>(exprType)->unroll();
		}

		// check whether it is a composite type
		const NamedCompositeTypePtr compositeType = dynamic_pointer_cast<const NamedCompositeType>(exprType);
		if (!compositeType) {
			add(res, Message(address,
					EC_TYPE_ACCESSING_MEMBER_OF_NON_NAMED_COMPOSITE_TYPE,
					format("Cannot access member '%s' of non-named-composed type %s of type %s",
							toString(*identifier).c_str(),
							toString(*structExpr).c_str(),
							toString(*exprType).c_str()),
					Message::ERROR));
			return res;
		}

		// get member type
		const TypePtr& resultType = compositeType->getTypeOfMember(identifier);
		if (!resultType) {
			add(res, Message(address,
					EC_TYPE_NO_SUCH_MEMBER,
					format("No member %s within composed type %s",
							toString(*identifier).c_str(),
							toString(*compositeType).c_str()),
					Message::ERROR));
			return res;
		}

		// check for correct member type
		if (elementType != resultType) {
			add(res, Message(address,
					EC_TYPE_INVALID_TYPE_OF_MEMBER,
					format("Invalid type of extracted member %s - expected %s",
							toString(*resultType).c_str(),
							toString(*elementType).c_str()),
					Message::ERROR));
			return res;
		}

		// no problems found
		return res;
	}


}

OptionalMessageList MemberAccessElementTypeCheck::visitCallExpr(const CallExprAddress& address) {
	NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	// check whether it is a call to the member access expression
	bool isMemberAccess = analysis::isCallOf(address.getAddressedNode(), manager.basic.getCompositeMemberAccess());
	bool isMemberReferencing = analysis::isCallOf(address.getAddressedNode(), manager.basic.getCompositeRefElem());
	if (!isMemberAccess && !isMemberReferencing) {
		// no matching case
		return res;
	}

	if (address->getArguments().size() != 3) {
		// incorrect function usage => let function check provide errors
		return res;
	}

	// extract parameters
	const ExpressionPtr& structExpr = address->getArgument(0);
	const ExpressionPtr& identifierExpr = address->getArgument(1);
	const TypePtr& elementType = address->getArgument(2)->getType();

	// check identifier literal
	if (identifierExpr->getNodeType() != NT_Literal) {
		add(res, Message(address,
				EC_TYPE_INVALID_IDENTIFIER,
				format("Invalid identifier expression %s - not a constant.",
						toString(*identifierExpr).c_str()),
				Message::ERROR));
		return res;
	}

	// check type literal
	TypePtr resultType;
	if (GenericTypePtr genType = dynamic_pointer_cast<const GenericType>(elementType)) {
		if (genType->getFamilyName() != "type" || genType->getTypeParameter().size() != 1) {
			// invalid argument => leaf issues to argument type checker
			return res;
		}

		// retrieve type
		resultType = genType->getTypeParameter()[0];

	} else {
		// invalid arguments => argument type checker will handle it
		return res;
	}

	// extract the value of the literal
	const LiteralPtr& identifierLiteral = static_pointer_cast<const Literal>(identifierExpr);
	const IdentifierPtr memberName = Identifier::get(manager, identifierLiteral->getValue());

	// use common check routine
	return checkMemberAccess(address, structExpr, memberName, resultType, isMemberReferencing);

}


OptionalMessageList BuiltInLiteralCheck::visitLiteral(const LiteralAddress& address) {

	OptionalMessageList res;

	// check whether it is a build-in literal
	const NodeManager& manager = address->getNodeManager();

	// obtain literal
	try {
		LiteralPtr buildIn = manager.basic.getLiteral(address->getValue());

		// check whether used one is special case of build-in version
		if (*buildIn->getType() != *address->getType()) {
			add(res, Message(address,
					EC_TYPE_INVALID_TYPE_OF_LITERAL,
					format("Deviating type of build in literal %s - expected: %s, actual: %s",
							address->getValue().c_str(),
							toString(*buildIn->getType()).c_str(),
							toString(*address->getType()).c_str()),
					Message::WARNING));
		}

	} catch (const lang::LiteralNotFoundException& lnfe) {
		// no such literal => all fine
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
		case NT_VectorType:
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
			assert(false && "Sorry, missed some type!");
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
