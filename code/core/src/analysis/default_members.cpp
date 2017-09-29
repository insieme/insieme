/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/core/analysis/default_members.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/type_utils.h"

#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace core {
namespace analysis {

	namespace {
		struct DefaultedTag {};

		LiteralPtr getDefaultedMarker(const IRBuilder& builder) {
			auto defaulted = builder.stringLit("INSIEME_DEFAULTED");
			defaulted.attachValue<DefaultedTag>();
			return defaulted;
		}

		LambdaExprPtr markAsDefaultMember(const LambdaExprPtr& lambda) {
			IRBuilder builder(lambda->getNodeManager());
			StatementList newBody{ getDefaultedMarker(builder) };
			std::copy(lambda->getBody()->begin(), lambda->getBody()->end(), std::back_inserter(newBody));
			return builder.lambdaExpr(lambda->getType(), lambda->getParameterList(), builder.compoundStmt(newBody), lambda->getReference()->getNameAsString());
		}

		LambdaExprPtr buildConstructorLambda(const FunctionTypePtr& ctorType, const VariableList& variables, const StatementPtr& body) {
			IRBuilder builder(ctorType->getNodeManager());
			auto name = builder.getLiteralForConstructor(ctorType)->getValue()->getValue();
			return markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, variables, body, name)));
		}
		LambdaExprPtr buildDestructorLambda(const FunctionTypePtr& dtorType, const VariableList& variables, const StatementPtr& body) {
			IRBuilder builder(dtorType->getNodeManager());
			auto name = builder.getLiteralForDestructor(dtorType)->getValue()->getValue();
			return markAsDefaultMember(normalize(builder.lambdaExpr(dtorType, variables, body, name)));
		}
		MemberFunctionPtr buildAssignmentLambda(const FunctionTypePtr& funType, const VariableList& variables, const StatementPtr& body) {
			IRBuilder builder(funType->getNodeManager());
			auto funName = utils::getMangledOperatorAssignName();
			auto name = builder.getLiteralForMemberFunction(funType, funName)->getValue()->getValue();
			auto fun = markAsDefaultMember(normalize(builder.lambdaExpr(funType, variables, body, name)));
			return builder.memberFunction(false, funName, fun);
		}
	}


	FunctionTypePtr getDefaultConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultConstructorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto body = builder.getNoOp();
		return buildConstructorLambda(ctorType, toVector(thisParam), body);
	}

	FunctionTypePtr getDefaultCopyConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultCopyConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultCopyConstructorType(thisType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);
		// TODO: build actual copy-body
//		auto body = compoundStmt(assign(deref(thisParam),deref(otherParam)));
		auto body = builder.getNoOp();
		return buildConstructorLambda(ctorType, toVector(thisParam, otherParam), body);
	}

	FunctionTypePtr getDefaultMoveConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
		return builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultMoveConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultMoveConstructorType(thisType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);
		// TODO: build actual move-body
//		auto body = compoundStmt(assign(deref(thisParam),deref(otherParam)));
		auto body = builder.getNoOp();
		return buildConstructorLambda(ctorType, toVector(thisParam, otherParam), body);
	}

	FunctionTypePtr getDefaultDestructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_DESTRUCTOR);
	}
	LambdaExprPtr getDefaultDestructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto dtorType = getDefaultDestructorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto body = builder.getNoOp();
		return buildDestructorLambda(dtorType, toVector(thisParam), body);
	}

	FunctionTypePtr getDefaultCopyAssignOperatorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
		TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
	}
	MemberFunctionPtr getDefaultCopyAssignOperator(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto funType = getDefaultCopyAssignOperatorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(funType->getParameterType(1));
		// TODO: build actual copy-assign-body
		auto body = builder.compoundStmt(builder.returnStmt(lang::buildRefCast(builder.deref(thisParam), funType->getReturnType())));
		return buildAssignmentLambda(funType, toVector(thisParam, otherParam), body);
	}

	FunctionTypePtr getDefaultMoveAssignOperatorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
		TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
	}
	MemberFunctionPtr getDefaultMoveAssignOperator(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto funType = getDefaultMoveAssignOperatorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(funType->getParameterType(1));
		// TODO: build actual move-assign-body
		auto body = builder.compoundStmt(builder.returnStmt(lang::buildRefCast(builder.deref(thisParam), funType->getReturnType())));
		return buildAssignmentLambda(funType, toVector(thisParam, otherParam), body);
	}


	bool hasDefaultConstructor(const TagTypePtr& type) {
		return analysis::hasConstructorOfType(type, getDefaultConstructorType(lang::buildRefType(type->getTag())));
	}

	bool hasCopyConstructor(const TagTypePtr& type) {
		return analysis::hasConstructorOfType(type, getDefaultCopyConstructorType(lang::buildRefType(type->getTag())));
	}

	bool hasMoveConstructor(const TagTypePtr& type) {
		return analysis::hasConstructorOfType(type, getDefaultMoveConstructorType(lang::buildRefType(type->getTag())));
	}

	bool hasDefaultDestructor(const TagTypePtr& type) {
		const auto& record = type->getRecord();
		return record->hasDestructor() && isDefaultDestructor(record->getDestructor());
	}

	bool hasCopyAssignment(const TagTypePtr& type) {
		return analysis::hasMemberOfType(type, utils::getMangledOperatorAssignName(), getDefaultCopyAssignOperatorType(lang::buildRefType(type->getTag())));
	}

	bool hasMoveAssignment(const TagTypePtr& type) {
		return analysis::hasMemberOfType(type, utils::getMangledOperatorAssignName(), getDefaultMoveAssignOperatorType(lang::buildRefType(type->getTag())));
	}

	bool isaDefaultConstructor(const ExpressionPtr& ctor) {
		if (auto lambda = ctor.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isConstructor() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool isDefaultDestructor(const ExpressionPtr& dtor) {
		if (auto lambda = dtor.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isDestructor() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool isDefaultAssignment(const MemberFunctionPtr& memberFunction) {
		//only assignment operators can be default member functions
		if (memberFunction->getNameAsString() != utils::getMangledOperatorAssignName()) return false;

		const auto& impl = memberFunction->getImplementation();
		if(auto lambda = impl.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isMemberFunction() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool isaDefaultMember(const NodePtr& node) {
		auto n = node;
		if(auto mem = n.isa<MemberFunctionPtr>()) {
			n = mem->getImplementation();
		}
		auto defaultCheck = [](const CompoundStmtPtr& body, const FunctionTypePtr& funType) {
			if(!funType->isMember()) return false;
			if(!body || body->size() < 1) return false;
			auto first = body[0];
			return first == getDefaultedMarker(IRBuilder(body->getNodeManager())) && first.hasAttachedValue<DefaultedTag>();
		};
		if (auto lambda = n.isa<LambdaExprPtr>()) {
			return defaultCheck(lambda->getBody(), lambda->getFunctionType());
		} else if (auto lambda = n.isa<LambdaPtr>()) {
			return defaultCheck(lambda->getBody(), lambda->getType());
		}
		return false;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
