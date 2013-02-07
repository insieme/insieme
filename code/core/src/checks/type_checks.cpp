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
#include "insieme/core/ir_class_info.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/types/variable_sized_struct_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)

namespace {

	TypePtr followDataPath(const TypePtr& source, const ExpressionPtr& datapath) {

		TypePtr fail;
		const auto& basic = source.getNodeManager().getLangBasic();

		// if data path is ROOT, the target type is equal to the source type
		if (basic.isDataPathRoot(datapath)){
			return source;		// no step to be taken
		}

		// if not root, it has to be a composed data path (based on call Expressions)
		auto call = datapath.isa<CallExprPtr>();
		if (!call || !basic.isDataPathPrimitive(call->getFunctionExpr())) {
			return fail; // not a valid data path
		}

		// resolve target type recursively
		TypePtr localSrc = followDataPath(source, call->getArgument(0));
		if (!localSrc) return fail; 	// unable to follow path recursively

		// unroll recursive types to make those transparent
		if (localSrc->getNodeType() == NT_RecType) {
			localSrc = localSrc.as<RecTypePtr>()->unroll();
		}

		// process local step
		auto step = call->getFunctionExpr();
		if (basic.isDataPathMember(step)) {

			// check input type
			NamedCompositeTypePtr compositeType = localSrc.isa<NamedCompositeTypePtr>();
			if (!compositeType) return fail;	// invalid source

			// check member argument
			auto member = call->getArgument(1);
			if (!member.isa<LiteralPtr>()) return fail; 	// invalid member name

			// return type of member
			return compositeType->getTypeOfMember(member.as<LiteralPtr>()->getValue());

		}

		// check whether it is accessing an element of an array / vector
		if (basic.isDataPathElement(step)) {

			// input needs to be an array or a vector
			if (localSrc->getNodeType() != NT_ArrayType && localSrc->getNodeType() != NT_VectorType) {
				return fail;		// no valid type
			}

			// the rest is fine, get the element type
			return localSrc.as<SingleElementTypePtr>()->getElementType();

		}

		// check whether it is accessing an element of a tuple
		if (basic.isDataPathComponent(step)) {

			// source has to be a tuple type
			if (localSrc->getNodeType() != NT_TupleType) return fail;
			auto tupleType = localSrc.as<TupleTypePtr>();

			// check whether the given index is an integer constant
			auto f = arithmetic::toFormula(call->getArgument(1));
			if (!f.isConstant() || !f.isInteger()) return fail;

			// extract index
			size_t n = (int64_t)f.getConstantValue();

			// return type of n-th component
			return (0<=n && n<tupleType.size())? tupleType[n] : fail;
		}

		// check whether it is navigating along the inheritance hierarchy
		if (basic.isDataPathParent(step)) {

			// local source needs to be a struct
			if (localSrc->getNodeType() != NT_StructType) return fail;
			StructTypePtr structType = localSrc.as<StructTypePtr>();

			// second argument needs to be a type literal
			auto parentTypeLiteral = call->getArgument(1);
			if (!analysis::isTypeLiteral(parentTypeLiteral)) return fail; // not a type literal
			TypePtr parent = analysis::getRepresentedType(parentTypeLiteral);

			// there has to be a parent of the requested type
			for(auto cur : structType->getParents()) {
				if (*cur->getType() == *parent) return parent;
			}

			return fail; 		// no matching parent found
		}

		std::cerr << "Invalid DataPath step encountered: " << *step << "\n";
		assert(false && "Unsupported DataPath step encountered!");

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

OptionalMessageList FunctionKindCheck::visitFunctionType(const FunctionTypeAddress& address) {

	OptionalMessageList res;

	// check value of kind-flag (must be within the enumeration)
	switch(address->getKind()) {
	case FK_PLAIN:
	case FK_CLOSURE:
	case FK_CONSTRUCTOR:
	case FK_DESTRUCTOR:
	case FK_MEMBER_FUNCTION:
		break;	// all valid values
	default:
		// this is an invalid value
		add(res, Message(address,
				EC_TYPE_ILLEGAL_FUNCTION_TYPE_KIND,
				format("Invalid value for function-type kind field: %d", toString(address->getKind())),
				Message::ERROR
		));
	}

	// check object type for ctors / dtors / member functions
	if (address->isConstructor() || address->isDestructor() || address->isMemberFunction()) {
		if (address->getParameterTypes().empty()) {
			add(res, Message(address,
					EC_TYPE_ILLEGAL_OBJECT_TYPE,
					format("Missing object type within ctor / dtor / member function."),
					Message::ERROR
			));
		} else if (!analysis::isObjectReferenceType(address->getParameterType(0))) {
			add(res, Message(address,
					EC_TYPE_ILLEGAL_OBJECT_TYPE,
					format("Invalid type for target object: %s", toString(address->getParameterType(0))),
					Message::ERROR
			));
		}
	}

	// check no-arguments for destructor
	if (address->isDestructor()) {
		if (address->getParameterTypes().size() > 1u) {
			add(res, Message(address,
					EC_TYPE_ILLEGAL_DESTRUCTOR_PARAMETERS,
					format("Destructor type must not exhibit parameters!"),
					Message::ERROR
			));
		}
	}

	// check return type of constructor
	if (address->isConstructor() && !address->getParameterTypes().empty()) {
		if (*address->getParameterType(0) != *address->getReturnType()) {
			add(res, Message(address,
					EC_TYPE_ILLEGAL_CONSTRUCTOR_RETURN_TYPE,
					format("Invalid return type of constructor - is: %s, should %s",
							toString(*address->getReturnType()),
							toString(*address->getParameterType(0))),
					Message::ERROR
			));
		}
	}

	// check return type of destructor
	if (address->isDestructor() && !address->getParameterTypes().empty()) {
		if (*address->getParameterType(0) != *address->getReturnType()) {
			add(res, Message(address,
					EC_TYPE_ILLEGAL_DESTRUCTOR_RETURN_TYPE,
					format("Invalid return type of destructor - is: %s, should %s",
							toString(*address->getReturnType()),
							toString(*address->getParameterType(0))),
					Message::ERROR
			));
		}
	}

	return res;

}

OptionalMessageList ParentCheck::visitParent(const ParentAddress& address) {

	OptionalMessageList res;

	// just check whether parent type is a potential object type
	auto type = address.as<ParentPtr>()->getType();
	if (!analysis::isObjectType(type)) {
		add(res, Message(address,
				EC_TYPE_ILLEGAL_OBJECT_TYPE,
				format("Invalid parent type - not an object: %s",
						toString(*type)),
				Message::ERROR
		));
	}

	return res;
}

OptionalMessageList ClassInfoCheck::visitType(const TypeAddress& address) {

	OptionalMessageList res;

	// check whether there is something to check
	if (!hasMetaInfo(address)) {
		return res;
	}

	// extract the class type
	TypePtr type = address.as<TypePtr>();

	// check whether address is referencing object type
	if (!analysis::isObjectType(type)) {
		add(res, Message(address,
				EC_TYPE_ILLEGAL_OBJECT_TYPE,
				format("Invalid type exhibiting class-meta-info: %s",
						toString(*type)),
				Message::ERROR
		));

		// skip rest of the checks
		return res;
	}

	// extract information
	const ClassMetaInfo& info = getMetaInfo(type);

	// check whether class info is covering target type
	TypePtr should = info.getClassType();
	if (should && should != type) {
		add(res, Message(address,
				EC_TYPE_MISMATCHING_OBJECT_TYPE,
				format("Class-Meta-Info attached to mismatching type - is: %s - should: %s",
						toString(*type), toString(*should)),
				Message::ERROR
		));
	}

	return res;
}

OptionalMessageList CallExprTypeCheck::visitCallExpr(const CallExprAddress& address) {

	NodeManager& manager = address->getNodeManager();
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
	auto substitution = types::getTypeVariableInstantiation(manager, address);

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

	// assume return type and function type to be correct
	auto result = isType->getReturnType();
	auto kind = isType->getKind();

	FunctionTypePtr funType = FunctionType::get(manager, param, result, kind);
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
	if (funType->isClosure()) {
		add(res, Message(address,
						EC_TYPE_INVALID_FUNCTION_TYPE,
						format("External literals must not be closure types!"),
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
	FunctionTypePtr funTypeShould = builder.functionType(::transform(lambda->getLambda()->getParameterList(), [](const VariablePtr& cur) { return cur->getType(); }), funTypeIs->getReturnType(), funTypeIs->getKind());
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


OptionalMessageList ArrayTypeCheck::visitNode(const NodeAddress& address) {

	OptionalMessageList res;

	// filter out everything which is not a type or expression
	NodeCategory cat = address->getNodeCategory();
	if (cat != NC_Expression && cat != NC_Type) {
		return res;		// this test is only covering expressions and types
	}

	// check expressions (must not be arrays except within very few cases)
	if (cat == NC_Expression) {
		ExpressionPtr expr = address.as<ExpressionPtr>();
		if (expr->getType()->getNodeType() == NT_ArrayType) {

			// if it is the result of a array-create call it is accepted
			auto& basic = address->getNodeManager().getLangBasic();
			if (core::analysis::isCallOf(expr, basic.getArrayCreate1D()) || core::analysis::isCallOf(expr, basic.getArrayCreateND())) {
				return res; // all fine for those two components
			}

			// no value instantiation allowed
			add(res, Message(address,
					EC_TYPE_INVALID_ARRAY_VALUE,
					format("Invalid instantiation of array value of type %s! Arrays must not be accessed by value, only by reference.",
							toString(*address)),
						Message::ERROR
			));
			return res;
		}
	}

	// the rest are just limitations on types
	if (cat != NC_Type) return res;

	// union, ref and array types are fine


	// check composition of struct types
	if (address->getNodeType() == NT_StructType) {
		StructTypePtr structType = address.as<StructTypePtr>();
		if (structType.empty()) return res;
		for(auto it = structType.begin(); it != structType.end() - 1; ++it) {
			if (types::isVariableSized(it->getType())) {
				add(res, Message(address,
						EC_TYPE_INVALID_ARRAY_CONTEXT,
						"Variable sized data structure has to be the last component of enclosing struct type.",
						Message::ERROR
				));
			}
		}
		return res;
	}

	// check union types
	if (address->getNodeType() == NT_TupleType) {
		TupleTypePtr tupleType = address.as<TupleTypePtr>();
		if (tupleType.empty()) return res;
		for(auto it = tupleType.begin(); it != tupleType.end() - 1; ++it) {
			if (types::isVariableSized(*it)) {
				add(res, Message(address,
						EC_TYPE_INVALID_ARRAY_CONTEXT,
						"Variable sized data structure has to be the last component of enclosing tuple type.",
						Message::ERROR
				));
			}
		}
		return res;
	}

	// no issues identified
	return res;
}


OptionalMessageList DeclarationStmtTypeCheck::visitDeclarationStmt(const DeclarationStmtAddress& address) {

	OptionalMessageList res;

	DeclarationStmtPtr declaration = address.getAddressedNode();

	// just test whether same type is on both sides
	TypePtr variableType = declaration->getVariable()->getType();
	TypePtr initType = declaration->getInitialization()->getType();

	if (!types::isSubTypeOf(initType, variableType)) {
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

	if (!types::isSubTypeOf(node->getEnd().getType(), iteratorType)) {
		add(res, Message(address,
			EC_TYPE_INVALID_BOUNDARY_TYPE,
			format("Invalid type of upper loop boundary - expected: %s, actual: %s\n",
					toString(*iteratorType).c_str(),
					toString(*node->getEnd().getType()).c_str()),
			Message::ERROR));
	}

	if (!types::isSubTypeOf(node->getStep().getType(), iteratorType)) {
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

namespace {

	bool isPrimitiveType(const TypePtr& type) {
		auto& basic = type->getNodeManager().getLangBasic();
		return basic.isChar(type) || basic.isBool(type) || basic.isScalarType(type);
	}

	bool isValidCast(const TypePtr& src, const TypePtr& trg) {

		// get basic definitions
		auto& basic = src->getNodeManager().getLangBasic();

		// casting a type to itself is always allowed
		if (*src == *trg) return true;

		// allow cast to generic
		if (trg->getNodeType() == NT_TypeVariable) {
			return true;
		}

		// casts between integer values or reals are allowed
		if (isPrimitiveType(src) && isPrimitiveType(trg)) {
			return true; // this is allowed
		}

		// allow casts between recursive version and unrolled version
		if (src->getNodeType() == NT_RecType && trg->getNodeType() != NT_RecType) {
			return isValidCast(src.as<RecTypePtr>()->unroll(), trg);
		}

		if (src->getNodeType() != NT_RecType && trg->getNodeType() == NT_RecType) {
			return isValidCast(src, trg.as<RecTypePtr>()->unroll());
		}

		// also allow references to be casted to boolean
		if (src->getNodeType() == NT_RefType && basic.isBool(trg)) return true;

		// we also allow casts between references
		if (src->getNodeType() == NT_RefType && trg->getNodeType() == NT_RefType) {
			// check whether cast between target types is valid
			auto srcType = src.as<RefTypePtr>()->getElementType();
			auto trgType = trg.as<RefTypePtr>()->getElementType();

			if (srcType->getNodeType() == NT_RefType || trg->getNodeType() == NT_RefType) {
				return isValidCast(srcType, trgType);
			}

			// this is a
			return true;
		}

		// everything else is invalid
		return false;
	}

}


OptionalMessageList CastCheck::visitCastExpr(const CastExprAddress& address) {

	OptionalMessageList res;

	TypePtr src = address->getSubExpression()->getType();
	TypePtr trg = address->getType();

	// check whether cast is safe
	if (isValidCast(src, trg)) {
		return res;
	}

	// report an error
	add(res, Message(address,
		EC_TYPE_ILLEGAL_CAST,
		format("Casting between incompatible types %s and %s", toString(src), toString(trg)),
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
	
	NodeManager& manager = call->getNodeManager();
	OptionalMessageList res;

	CallExprPtr callExpr = call.getAddressedNode();

	// the function call must be narrow
	if (!analysis::isCallOf(callExpr, manager.getLangBasic().getRefNarrow()))
		return res;

	if (call.getArguments().size() != 3u)
		return res;

	// Obtain argument type
	ExpressionPtr srcArg = callExpr->getArgument(0);
	ExpressionPtr dpArg  = callExpr->getArgument(1);
	ExpressionPtr trgArg = callExpr->getArgument(2);

	// check whether src argument is of a reference type
	auto srcType = analysis::getReferencedType(srcArg->getType());
	if (!srcType) return res; // => parameter error is handled by standard type checker

	// check recursively the inner types of the expression
	TypePtr narrowType = followDataPath(srcType, dpArg);

	// check whether data path was valid
	if (!narrowType){
		add(res, Message(call,
						EC_TYPE_MALFORM_NARROW_CALL,
						format ("Malformed dataPath in Narrow expression %s, %s, %s",
								toString(*srcArg),
								toString(core::printer::PrettyPrinter(dpArg, core::printer::PrettyPrinter::NO_LET_BINDINGS)),
								toString(*trgArg)),
						Message::ERROR));
		return res;
	}

	// check whether specified target type is correct
	if (*narrowType != *analysis::getRepresentedType(trgArg)){
		add(res, Message(call,
						EC_TYPE_MALFORM_NARROW_CALL,
						format (" Wrong return type in Narrow Expression %s, %s, %s",
								toString(*srcArg),
								toString(core::printer::PrettyPrinter(dpArg, core::printer::PrettyPrinter::NO_LET_BINDINGS)),
								toString(*trgArg)),
						Message::ERROR));
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

	NodeManager& manager = call->getNodeManager();
	OptionalMessageList res;

	CallExprPtr callExpr = call.getAddressedNode();

	// the function call must be Expand
	if (!analysis::isCallOf(callExpr, manager.getLangBasic().getRefExpand()))
		return res;

	if (callExpr->getArguments().size() != 3u)
		return res;

	// Obtain argument type
	ExpressionPtr srcArg = callExpr->getArgument(0);  // variable
	ExpressionPtr dpArg  = callExpr->getArgument(1);  // datapath
	ExpressionPtr trgArg = callExpr->getArgument(2);  // outher data structure type literal

	// test whether source is a reference
	auto srcType = analysis::getReferencedType(srcArg->getType());
	if (!srcType) return res; // => parameter error is handled by standard type checker

	// check recursively the inner types of the expression
	TypePtr extractType = followDataPath(analysis::getRepresentedType(trgArg), dpArg);

	// check whether data path was valid
	if (!extractType){
		add(res, Message(call,
						EC_TYPE_MALFORM_EXPAND_CALL,
						format ("Malformed dataPath in Expand expression %s, %s, %s",
								toString(*srcArg),
								toString(core::printer::PrettyPrinter(dpArg, core::printer::PrettyPrinter::NO_LET_BINDINGS)),
								toString(*trgArg)),
						Message::ERROR));
		return res;
	}

	// check whether src/dp/trg combination is valid
	if (*extractType != *srcType){
		add(res, Message(call,
					EC_TYPE_MALFORM_EXPAND_CALL,
					format (" Wrong return type in Expand Expression %s, %s, %s",
							toString(*srcArg),
							toString(core::printer::PrettyPrinter(dpArg, core::printer::PrettyPrinter::NO_LET_BINDINGS)),
							toString(*trgArg)),
					Message::ERROR));
	}

	return res;
}

#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme
