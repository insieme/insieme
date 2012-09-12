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

#include "insieme/core/checks/type_checks.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/type_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/analysis/type_variable_deduction.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)

namespace{

TypePtr ResolveDataPath (const IRBuilder& builder, 
					  	 const TypePtr& source,
					  	 const ExpressionPtr& datapath) {

	TypePtr fail;

	// if ROOT, target and source must be equal
	if (builder.getLangBasic().isDataPathRoot(datapath)){

		if (auto ret= analysis::getReferencedType(source))
			return ret;									// narrow case
		else
			return source;								// expand case
	}
	else{
		// if not root, datapath is function shaped
		if (!datapath.isa<CallExprPtr>())
			return fail;

		const CallExprPtr& dpExp = datapath.as<CallExprPtr>();
		const ExpressionPtr& innerDP = dpExp->getArgument(0);
		const ExpressionPtr& member  = dpExp->getArgument(1);
		const TypePtr& inType = ResolveDataPath(builder, source, innerDP);

		// if Member, check that member exist inside of source
		if (builder.getLangBasic().isDataPathMember(dpExp->getFunctionExpr())){

			if (inType.isa<NamedCompositeTypePtr>()){
				const NamedCompositeTypePtr& structType = inType.as<NamedCompositeTypePtr>();
				return structType->getTypeOfMember (member.as<core::LiteralPtr>()->getValue());
			}
			else{
				// TODO: recursive types not supported
				return fail;
			}
		}
		else if (builder.getLangBasic().isDataPathElement(dpExp->getFunctionExpr())){

			if (inType.isa<ArrayTypePtr>()) {
				return inType.as<ArrayTypePtr>()->getElementType();
			}
			else if(inType.isa<VectorTypePtr>()){
				return inType.as<VectorTypePtr>()->getElementType();
			}
			else{
				return fail;
			}
		}
		else if (builder.getLangBasic().isDataPathComponent(dpExp->getFunctionExpr())){

			if (!inType.isa<TupleTypePtr>())
				return fail;

			auto f = arithmetic::toFormula(member);
			if (!f.isConstant())
				return fail;
			if (!f.isInteger())
				return  fail;

			int n = (int64_t)f.getConstantValue();
			if (((int)inType.as<TupleTypePtr>()->size() <= n) || (n <0))
				return  fail;
			return inType.as<TupleTypePtr>()->getElement(n);
		}
	}
	return fail;
}


} // end anonymous namespace

OptionalMessageList KeywordCheck::visitGenericType(const GenericTypeAddress& address) {

	OptionalMessageList res;

	if ((address->getName()->getValue() == "vector" && address->getNodeType()!=NT_VectorType) ||
		(address->getName()->getValue() == "array" && address->getNodeType()!=NT_ArrayType) ||
		(address->getName()->getValue() == "ref" && address->getNodeType()!=NT_RefType) ||
		(address->getName()->getValue() == "channel" && address->getNodeType()!=NT_ChannelType)) {

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
	const TypeList& parameterTypes = functionType->getParameterTypes()->getTypes();
	const TypePtr& returnType = functionType->getReturnType();

	// Obtain argument type
	TypeList argumentTypes;
	transform(address.as<CallExprPtr>()->getArguments(), back_inserter(argumentTypes), [](const ExpressionPtr& cur) {
		return cur->getType();
	});

	// 1) check number of arguments
	int numParameter = parameterTypes.size();
	int numArguments = argumentTypes.size();
	if (numArguments != numParameter) {
		add(res, Message(address,
						EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS,
						format("Wrong number of arguments - expected: %d\n", numParameter) +
						format("actual: %d\n ", numArguments) +
						format("- function type: \n%s", toString(*functionType).c_str()),
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
						format("Invalid argument type(s) - expected: \n%s\n",	toString(*parameterTuple).c_str()) +
						format("actual: \n%s\n", toString(*argumentTuple).c_str()) +
						format("- function type: \n%s", toString(*functionType).c_str()),
						Message::ERROR));
		return res;
	}

	// 3) check return type - which has to be matched with modified function return value.
	TypePtr retType = substitution->applyTo(returnType);
	TypePtr resType = address->getType();

	if (*retType != *resType) {
		add(res, Message(address,
						EC_TYPE_INVALID_RETURN_TYPE,
						format("Invalid result type of call expression - expected: %s, actual: %s - function type: %s",
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
	transform(address.getAddressedNode()->getParameterList()->getElements(), back_inserter(param), extractType);

	FunctionTypePtr isType = address->getLambda()->getType();
	TypePtr result = address->getLambda()->getType()->getReturnType();

	FunctionTypePtr funType = FunctionType::get(manager, param, result, true);
	if (*funType != *isType) {
		add(res, Message(address,
						EC_TYPE_INVALID_FUNCTION_TYPE,
						format("Invalid type of lambda expression - expected: \n%s, actual: \n%s",
								toString(*funType).c_str(),
								toString(*isType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList BindExprTypeCheck::visitBindExpr(const BindExprAddress& address) {

	NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	// recreate type
	auto extractType = [](const VariablePtr& var) {
		return var->getType();
	};

	TypeList param;
	transform(address.getAddressedNode()->getParameters()->getElements(), back_inserter(param), extractType);

	TypePtr isType = address->getType();
	TypePtr result = address->getCall()->getType();

	FunctionTypePtr funType = FunctionType::get(manager, param, result, false);
	if (*funType != *isType) {
		add(res, Message(address,
						EC_TYPE_INVALID_FUNCTION_TYPE,
						format("Invalid type of bind expression - expected: \n%s, actual: \n%s",
								toString(*funType).c_str(),
								toString(*isType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList ExternalFunctionTypeCheck::visitLiteral(const LiteralAddress& address) {

	OptionalMessageList res;

	// only important for function types
	core::TypePtr type = address->getType();
	if (type->getNodeType() != core::NT_FunctionType) {
		return res;
	}

	core::FunctionTypePtr funType = static_pointer_cast<const core::FunctionType>(type);
	if (!funType->isPlain()) {
		add(res, Message(address,
						EC_TYPE_INVALID_FUNCTION_TYPE,
						format("External literals have to have plain function types!"),
						Message::ERROR));
	}
	return res;
}


OptionalMessageList ReturnTypeCheck::visitLambda(const LambdaAddress& address) {
	OptionalMessageList res;

	// obtain return type of lambda
	const TypePtr& returnType = address->getType()->getReturnType();

	// search for all return statements and check type
	visitDepthFirstPrunable(address, [&](const NodeAddress& cur)->bool {

		// check whether it is a a return statement
		if (cur->getNodeType() != NT_ReturnStmt) {
			// abort if this node is a expression or type
			NodeCategory category = cur->getNodeCategory();
			return (category == NC_Type || category == NC_Expression);
		}

		const ReturnStmtAddress& returnStmt = static_address_cast<const ReturnStmt>(cur);
		const TypePtr& actualType = returnStmt->getReturnExpr()->getType();
		if (returnType != actualType) {
			add(res, Message(cur,
				EC_TYPE_INVALID_RETURN_VALUE_TYPE,
				format("Invalid return type - expected: \n%s, actual: \n%s",
						toString(*returnType).c_str(),
						toString(*actualType).c_str()),
				Message::ERROR));
		}

		return true;
	}, false);


	// EC_TYPE_MISSING_RETURN_STMT,

	return res;
}

OptionalMessageList LambdaTypeCheck::visitLambdaExpr(const LambdaExprAddress& address) {

	OptionalMessageList res;

	// get lambda expression
	LambdaExprPtr lambda = address.getAddressedNode();

	// check that rec-lambda variable does exist within definitions
	if(!lambda->getDefinition()->getDefinitionOf(lambda->getVariable())) {
		add(res, Message(address,
				EC_TYPE_INVALID_LAMBDA_EXPR_NO_SUCH_DEFINITION,
				format("No definition found for rec-lambda variable %s",
						toString(*lambda->getVariable())),
				Message::ERROR
		));

		// no further checks usefull
		return res;
	}


	// check type of lambda expression compared to rec-lambda variable type
	TypePtr is = lambda->getType();
	TypePtr should = lambda->getVariable()->getType();
	if (*is != *should) {
		add(res, Message(address,
				EC_TYPE_INVALID_LAMBDA_EXPR_TYPE,
				format("Lambda-Expression Type does not match rec-lambda Variable Type - is: %s, should: %s",
						toString(*is),
						toString(*should)),
				Message::ERROR
		));
	}

	// check type of recursive variable
	assert(lambda->getDefinition()->getDefinitionOf(lambda->getVariable()));
	is = lambda->getVariable()->getType();
	should = lambda->getDefinition()->getDefinitionOf(lambda->getVariable())->getType();
	if (*is != *should) {
		add(res, Message(address,
				EC_TYPE_INVALID_LAMBDA_REC_VAR_TYPE,
				format("Type of recursive lambda variable %s does not fit type of lambda - is: %s, should: %s",
						toString(*lambda->getVariable()),
						toString(*is),
						toString(*should)),
				Message::ERROR
		));
	}

	// check type of lambda
	IRBuilder builder(lambda->getNodeManager());
	FunctionTypePtr funTypeIs = lambda->getLambda()->getType();
	FunctionTypePtr funTypeShould = builder.functionType(::transform(lambda->getLambda()->getParameterList(), [](const VariablePtr& cur) { return cur->getType(); }), funTypeIs->getReturnType());
	if (*funTypeIs != *funTypeShould) {
		add(res, Message(address,
				EC_TYPE_INVALID_LAMBDA_TYPE,
				format("Invalid type of lambda definition for variable %s - is: %s, should %s",
						toString(*lambda->getVariable()),
						toString(*funTypeIs),
						toString(*funTypeShould)),
					Message::ERROR
		));
	}

	return res;
}

namespace {

	TypeAddress getParentType(const NodeAddress& cur) {
		// terminal case
		if (!cur || cur.isRoot()) return TypeAddress();

		NodeAddress parent = cur.getParentAddress();
		if (parent->getNodeCategory() == NC_Type) {
			return parent.as<TypeAddress>();
		}
		return getParentType(parent);
	}

	bool isArrayBuiltIn(const NodePtr& cur) {
		auto& basic = cur->getNodeManager().getLangBasic();
		return basic.isBuiltIn(cur);
	}

	bool isPartOfArrayBuiltIn(const NodeAddress& cur) {
		if (cur.isRoot()) return isArrayBuiltIn(cur);
		if (cur->getNodeCategory() != NC_Expression) return isPartOfArrayBuiltIn(cur.getParentAddress());
		return isArrayBuiltIn(cur);
	}

}

OptionalMessageList ArrayTypeCheck::visitArrayType(const ArrayTypeAddress& address) {
	NodeManager manager;
	OptionalMessageList res;

	// this test requires a context
	if (address.isRoot()) {
		return res;
	}

	// check the two main exceptions - the array create literals
	if (isPartOfArrayBuiltIn(address)) {
		return res;
	}

	NodeAddress parent = address.getParentAddress();

	// check that array is not used by value
	if (parent->getNodeCategory() == NC_Expression) {

		// if it is the result of a array-create call it is accepted
		auto& basic = address->getNodeManager().getLangBasic();
		if (core::analysis::isCallOf(parent, basic.getArrayCreate1D()) || core::analysis::isCallOf(parent, basic.getArrayCreateND())) {
			return res; // all fine for those two components
		}

		add(res, Message(parent,
				EC_TYPE_INVALID_ARRAY_VALUE,
				format("Invalid instantiation of array value of type %s! Arrays must not be accessed by value, only by reference.",
						toString(*address)),
					Message::ERROR
		));
		return res;
	}

	// get enclosing parent type
	parent = getParentType(address);
	if (!parent) return res;

	// check the context
	assert(parent->getNodeCategory() == NC_Type && "There should not be another context!");

	// if it is enclosed within a reference or another array, everything is fine
	if (parent->getNodeType() == NT_RefType || parent->getNodeType() == NT_ArrayType) {
		return res;
	}

	// it has to be a struct, union or tuple
	auto parentType = parent->getNodeType();

	// allow array to occur within a meta-type type
	if (core::analysis::isTypeLiteralType(parent.as<TypePtr>())) {
		return res; 	// arrays are allowed within type literals
	}

	// check valid context
	if (parentType != NT_StructType && parentType != NT_UnionType && parentType != NT_TupleType) {
		add(res, Message(parent,
				EC_TYPE_INVALID_ARRAY_CONTEXT,
				"Arrays must only be used within refs, structs, unions, tuples or other array types..",
				Message::ERROR
		));
		return res;
	}

	// check that an array is not used without a reference
	NodeAddress grandParent = getParentType(parent);

	// check valid parent context => element has to be embedded within a reference
	if (grandParent && grandParent->getNodeType() != NT_RefType && grandParent->getNodeType() != NT_ArrayType) {
		add(res, Message(grandParent,
				EC_TYPE_INVALID_ARRAY_CONTEXT,
				"Invalid array context. Variable sized struct / union / tuples must be enclosed by a ref or array.",
				Message::ERROR
		));
		return res;
	}

	// for unions it is fine anyway
	if (parent->getNodeType() == NT_UnionType) {
		return res;	// all fine
	}

	// for structs and tuples it has to be the last element
	if (address.getParentAddress(address.getDepth() - parent.getDepth() - 1).getIndex() != parent->getChildList().size() - 1u) {
		add(res, Message(parent,
				EC_TYPE_INVALID_ARRAY_CONTEXT,
				"Embedded array has to be the last component of enclosing struct or tuple type.",
				Message::ERROR
		));
		return res;
	}

	// no problem encountered
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
						format("Invalid type of initial value - expected: \n%s, actual: \n%s",
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

	if (!manager.getLangBasic().isBool(conditionType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_CONDITION_EXPR,
						format("Invalid type of condition expression - expected: \n%s, actual: \n%s",
								toString(*manager.getLangBasic().getBool()).c_str(),
								toString(*conditionType).c_str()),
						Message::ERROR));
	}
	return res;
}

OptionalMessageList ForStmtTypeCheck::visitForStmt(const ForStmtAddress& address) {

	OptionalMessageList res;

	core::ForStmtPtr node = address.getAddressedNode();
	auto& basic = node->getNodeManager().getLangBasic();

	// get type of iterator
	core::TypePtr iteratorType = node->getIterator()->getType();

	// check iterator type
	if (!basic.isInt(iteratorType)) {
		add(res, Message(address,
			EC_TYPE_INVALID_ITERATOR_TYPE,
			format("Invalid type of iterator variable - expected: some integral, actual: %s\n",
					toString(*iteratorType).c_str()),
			Message::ERROR));
		return res;
	}

	if (*iteratorType != *node->getEnd().getType()) {
		add(res, Message(address,
			EC_TYPE_INVALID_BOUNDARY_TYPE,
			format("Invalid type of upper loop boundary - expected: %s, actual: %s\n",
					toString(*iteratorType).c_str(),
					toString(*node->getEnd().getType()).c_str()),
			Message::ERROR));
	}

	if (*iteratorType != *node->getStep().getType()) {
		add(res, Message(address,
			EC_TYPE_INVALID_BOUNDARY_TYPE,
			format("Invalid type of step size - expected: %s, actual: %s\n",
					toString(*iteratorType).c_str(),
					toString(*node->getStep().getType()).c_str()),
			Message::ERROR));
	}

	return res;
}

OptionalMessageList WhileConditionTypeCheck::visitWhileStmt(const WhileStmtAddress& address) {

	const NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	TypePtr conditionType = address->getCondition()->getType();
	if (!manager.getLangBasic().isBool(conditionType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_CONDITION_EXPR,
						format("Invalid type of condition expression - expected: \n%s, actual: \n%s",
								toString(*manager.getLangBasic().getBool()).c_str(),
								toString(*conditionType).c_str()),
						Message::ERROR));
	}
	return res;
}


OptionalMessageList SwitchExpressionTypeCheck::visitSwitchStmt(const SwitchStmtAddress& address) {
	const NodeManager& manager = address->getNodeManager();

	OptionalMessageList res;
	TypePtr switchType = address->getSwitchExpr()->getType();
	if (!manager.getLangBasic().isInt(switchType)) {
		add(res, Message(address,
						EC_TYPE_INVALID_SWITCH_EXPR,
						format("Invalid type of switch expression - expected: integral type, actual: \n%s",
								toString(*switchType).c_str()),
						Message::ERROR));
	}
	return res;
}



OptionalMessageList StructExprTypeCheck::visitStructExpr(const StructExprAddress& address) {
	OptionalMessageList res;

	// extract type
	core::StructTypePtr structType = static_pointer_cast<const StructType>(address.getAddressedNode()->getType());

	// check type of values within struct expression
	for_each(address.getAddressedNode()->getMembers()->getNamedValues(), [&](const NamedValuePtr& cur) {
		core::TypePtr requiredType = structType->getTypeOfMember(cur->getName());
		core::TypePtr isType = cur->getValue()->getType();

		if (*requiredType != *isType) {
			add(res, Message(address,
				EC_TYPE_INVALID_INITIALIZATION_EXPR,
				format("Invalid type of struct-member initalization - expected type: \n%s, actual: \n%s",
						toString(*requiredType).c_str(),
						toString(*isType).c_str()),
				Message::ERROR));
		}
	});

	return res;
}


namespace {


	OptionalMessageList checkMemberAccess(const NodeAddress& address, const ExpressionPtr& structExpr, const StringValuePtr& identifier, const TypePtr& elementType, bool isRefVersion) {

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
					format("Cannot access member '%s' of non-named-composed type \n%s of type \n%s",
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
					format("No member '%s' within composed type '%s'",
							toString(*identifier).c_str(),
							toString(*compositeType).c_str()),
					Message::ERROR));
			return res;
		}

		// check for correct member type
		if (elementType != resultType) {
			add(res, Message(address,
					EC_TYPE_INVALID_TYPE_OF_MEMBER,
					format("Invalid type of extracted member '%s' - expected '%s'",
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
	bool isMemberAccess = analysis::isCallOf(address.getAddressedNode(), manager.getLangBasic().getCompositeMemberAccess());
	bool isMemberReferencing = analysis::isCallOf(address.getAddressedNode(), manager.getLangBasic().getCompositeRefElem());
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
				format("Invalid identifier expression \n%s - not a constant.",
						toString(*identifierExpr).c_str()),
				Message::ERROR));
		return res;
	}

	// check type literal
	TypePtr resultType;
	if (GenericTypePtr genType = dynamic_pointer_cast<const GenericType>(elementType)) {
		if (genType->getName()->getValue() != "type" || genType->getTypeParameter()->size() != 1) {
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
	const StringValuePtr memberName = identifierLiteral->getValue();

	// use common check routine
	return checkMemberAccess(address, structExpr, memberName, resultType, isMemberReferencing);

}


namespace {


	OptionalMessageList checkTupleAccess(const NodeAddress& address, const ExpressionPtr& tupleExpr, unsigned index, const TypePtr& elementType, bool isRefVersion) {

		OptionalMessageList res;

		// check whether it is a tuple at all
		TypePtr exprType = tupleExpr->getType();
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

		// check whether it is a tuple type
		const TupleTypePtr tupleType = dynamic_pointer_cast<const TupleType>(exprType);
		if (!tupleType) {
			add(res, Message(address,
					EC_TYPE_ACCESSING_MEMBER_OF_NON_TUPLE_TYPE,
					format("Cannot access element #%d of non-tuple type \n%s of type \n%s",
							index,
							toString(*tupleExpr).c_str(),
							toString(*exprType).c_str()),
					Message::ERROR));
			return res;
		}

		// get element type
		unsigned numElements = tupleType->getElements().size();
		if (numElements <= index) {
			add(res, Message(address,
					EC_TYPE_NO_SUCH_MEMBER,
					format("No element with index %d within tuple type \n%s",
							index,
							toString(*tupleType).c_str()),
					Message::ERROR));
			return res;
		}

		const TypePtr& resultType = tupleType->getElement(index);

		// check for correct element type
		if (elementType != resultType) {
			add(res, Message(address,
					EC_TYPE_INVALID_TYPE_OF_MEMBER,
					format("Invalid type of extracted member \n%s - expected \n%s",
							toString(*resultType).c_str(),
							toString(*elementType).c_str()),
					Message::ERROR));
			return res;
		}

		// no problems found
		return res;
	}


}

OptionalMessageList ComponentAccessTypeCheck::visitCallExpr(const CallExprAddress& address) {
	NodeManager& manager = address->getNodeManager();
	OptionalMessageList res;

	// check whether it is a call to the tuple access expression
	bool isMemberAccess = analysis::isCallOf(address.getAddressedNode(), manager.getLangBasic().getTupleMemberAccess());
	bool isMemberReferencing = analysis::isCallOf(address.getAddressedNode(), manager.getLangBasic().getTupleRefElem());
	if (!isMemberAccess && !isMemberReferencing) {
		// no matching case
		return res;
	}

	if (address->getArguments().size() != 3) {
		// incorrect function usage => let function check provide errors
		return res;
	}

	// extract parameters
	const ExpressionPtr& tupleExpr = address->getArgument(0);
	ExpressionPtr indexExpr = address->getArgument(1);
	const TypePtr& elementType = address->getArgument(2)->getType();

	// check index literal
	while(indexExpr->getNodeType() == NT_CastExpr) { // TODO: remove this when removing casts
		indexExpr = static_pointer_cast<core::CastExprPtr>(indexExpr)->getSubExpression();
	}
	if (indexExpr->getNodeType() != NT_Literal) {
		add(res, Message(address,
				EC_TYPE_INVALID_TUPLE_INDEX,
				format("Invalid index expression \n%s - not a constant.",
						toString(*indexExpr).c_str()),
				Message::ERROR));
		return res;
	}

	// check type literal
	TypePtr resultType;
	if (GenericTypePtr genType = dynamic_pointer_cast<const GenericType>(elementType)) {
		if (genType->getName()->getValue() != "type" || genType->getTypeParameter()->size() != 1) {
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
	const LiteralPtr& indexLiteral = static_pointer_cast<const Literal>(indexExpr);
	unsigned index = utils::numeric_cast<unsigned>(indexLiteral->getValue()->getValue());

	// use common check routine
	return checkTupleAccess(address, tupleExpr, index, resultType, isMemberReferencing);

}


OptionalMessageList BuiltInLiteralCheck::visitLiteral(const LiteralAddress& address) {

	OptionalMessageList res;

	// check whether it is a build-in literal
	const NodeManager& manager = address->getNodeManager();

	// obtain literal
	try {
		LiteralPtr buildIn = manager.getLangBasic().getLiteral(address->getValue()->getValue());

		// check whether used one is special case of build-in version
		if (*buildIn->getType() != *address->getType()) {
			add(res, Message(address,
					EC_TYPE_INVALID_TYPE_OF_LITERAL,
					format("Deviating type of build in literal \n%s - expected: \n%s, actual: \n%s",
							address->getValue()->getValue().c_str(),
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

	// allow cast to generic
	if (trg->getNodeType() == NT_TypeVariable) {
		return res;
	}

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
		case NT_StructType: // also necessary for c++ inheritance
			// this cast is allowed (for now)
			return res;
		case NT_TypeVariable:
			return res;
		default:
			assert(false && "Sorry, missed some type!"); break;
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

///~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
///      Narrow check
///	Narrow construction is a function call in which there are 3 parameters
//		- data structure/array/tuple
//		- datapath to the inner member/element to extract
//		- type of the expected inner object
///~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
OptionalMessageList NarrowCheck::visitCallExpr(const CallExprAddress& call) {
	
	NodeManager manager;
	OptionalMessageList res;

	// obtain function type ...
	TypePtr funType = call->getFunctionExpr()->getType();
	assert( call->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Illegal function expression!");

	// the function call must be narrow
	if (!analysis::isCallOf(call.getAddressedNode(), manager.getLangBasic().getRefNarrow()))
		return res;

	if (call.getArguments().size() != 3u)
		return res;

	// Obtain argument type
	ExpressionPtr srcArg = call.as<CallExprPtr>()->getArgument(0);
	ExpressionPtr dpArg  = call.as<CallExprPtr>()->getArgument(1);
	ExpressionPtr trgArg = call.as<CallExprPtr>()->getArgument(2);

	IRBuilder builder(call.getNodeManager());

	// check recursively the inner types of the expression
	TypePtr narrowType = ResolveDataPath(builder, srcArg->getType(), dpArg);
	if (!narrowType){
		add(res, Message(call,
						EC_TYPE_MALFORM_NARROW_CALL,
						format ("Malform dataPath in Narrow expresion %s, %s, %s",
								toString(*srcArg).c_str(),
								toString(*dpArg).c_str(),
								toString(*trgArg).c_str()),
						Message::ERROR));
	}
	else{
		if (*narrowType != *analysis::getRepresentedType(trgArg)){
		add(res, Message(call,
						EC_TYPE_MALFORM_NARROW_CALL,
						format (" Wrong return type in Narrow Expression %s, %s, %s",
								toString(*srcArg).c_str(),
								toString(*dpArg).c_str(),
								toString(*trgArg).c_str()),
						Message::ERROR));
		}
	}
	return res;
}

///~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
///      Expand check
///	Expand construction is a function call in which there are 3 parameters
//		- variable of any type which belongs as member to a structure/array/tuple
//		- datapath of this variable inside of the outhermost structure/array/tuple
//		- type of the expected object
///~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
OptionalMessageList ExpandCheck::visitCallExpr(const CallExprAddress& call) {

	NodeManager manager;
	OptionalMessageList res;

	// obtain function type ...
	TypePtr funType = call->getFunctionExpr()->getType();
	assert( call->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Illegal function expression!");

	// the function call must be Expand
	if (!analysis::isCallOf(call.getAddressedNode(), manager.getLangBasic().getRefExpand()))
		return res;

	if (call.getArguments().size() != 3u)
		return res;

	// Obtain argument type
	ExpressionPtr srcArg = call.as<CallExprPtr>()->getArgument(0);  // variable
	ExpressionPtr dpArg  = call.as<CallExprPtr>()->getArgument(1);  // datapath
	ExpressionPtr trgArg = call.as<CallExprPtr>()->getArgument(2);  // outher data structure type literal

	IRBuilder builder(call.getNodeManager());

	// check recursively the inner types of the expression
	TypePtr extractType = ResolveDataPath(builder, analysis::getRepresentedType(trgArg), dpArg);
	if (!extractType){
		add(res, Message(call,
						EC_TYPE_MALFORM_EXPAND_CALL,
						format ("Malform dataPath in Expand expresion %s, %s, %s",
								toString(*srcArg).c_str(),
								toString(*dpArg).c_str(),
								toString(*trgArg).c_str()),
						Message::ERROR));
	}
	else{
		auto srcType = analysis::getReferencedType(srcArg->getType());
		if (!srcType){
			add(res, Message(call,
						EC_TYPE_MALFORM_EXPAND_CALL,
						format (" First parameter must be a reference in Expand Expression %s, %s, %s",
								toString(*srcArg).c_str(),
								toString(*dpArg).c_str(),
								toString(*trgArg).c_str()),
						Message::ERROR));
		}


		if (*extractType != *srcType){
			add(res, Message(call,
						EC_TYPE_MALFORM_EXPAND_CALL,
						format (" Wrong return type in Expand Expression %s, %s, %s",
								toString(*srcArg).c_str(),
								toString(*dpArg).c_str(),
								toString(*trgArg).c_str()),
						Message::ERROR));
		}
	}
	return res;
}

#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
