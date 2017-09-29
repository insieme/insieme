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
	}


	FunctionTypePtr getDefaultConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultConstructorType(thisType);
		auto name = builder.getLiteralForConstructor(ctorType);
		return markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, toVector(builder.variable(builder.refType(thisType))),
		                                                                        builder.getNoOp(), name->getValue()->getValue())).as<LambdaExprPtr>());
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
		auto name = builder.getLiteralForConstructor(ctorType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);
		// TODO: build actual copy-body
//		auto body = compoundStmt(assign(deref(thisParam),deref(otherParam)));
		auto body = builder.getNoOp();
		return markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, toVector(thisParam, otherParam),
		                                                                        body, name->getValue()->getValue())).as<LambdaExprPtr>());
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
		auto name = builder.getLiteralForConstructor(ctorType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);
		// TODO: build actual move-body
//		auto body = compoundStmt(assign(deref(thisParam),deref(otherParam)));
		auto body = builder.getNoOp();
		return markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, toVector(thisParam, otherParam),
		                                                                        body, name->getValue()->getValue())).as<LambdaExprPtr>());
	}

	FunctionTypePtr getDefaultDestructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_DESTRUCTOR);
	}
	LambdaExprPtr getDefaultDestructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto dtorType = getDefaultDestructorType(thisType);
		auto name = builder.getLiteralForDestructor(dtorType);
		return markAsDefaultMember(normalize(builder.lambdaExpr(dtorType, toVector(builder.variable(builder.refType(thisType))),
		                                                                        builder.getNoOp(), name->getValue()->getValue())).as<LambdaExprPtr>());
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
		auto res = builder.returnStmt(lang::buildRefCast(builder.deref(thisParam), funType->getReturnType()));
		auto name = builder.getLiteralForMemberFunction(funType, utils::getMangledOperatorAssignName());
		const auto& otherType = funType->getParameterType(1);
		auto fun = markAsDefaultMember(normalize(builder.lambdaExpr(funType, toVector(thisParam, builder.variable(otherType)),
		                                                                            res, name->getValue()->getValue())).as<LambdaExprPtr>());
		return builder.memberFunction(false, utils::getMangledOperatorAssignName(), fun);
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
		auto res = builder.returnStmt(lang::buildRefCast(builder.deref(thisParam), funType->getReturnType()));
		auto name = builder.getLiteralForMemberFunction(funType, utils::getMangledOperatorAssignName());
		const auto& otherType = funType->getParameterType(1);
		auto fun = markAsDefaultMember(normalize(builder.lambdaExpr(funType, toVector(thisParam, builder.variable(otherType)),
		                                                                            res, name->getValue()->getValue())).as<LambdaExprPtr>());
		return builder.memberFunction(false, utils::getMangledOperatorAssignName(), fun);
	}


	bool hasDefaultConstructor(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto thisType = builder.refType(type->getTag());
		auto ctorType = builder.functionType(TypeList{ thisType }, thisType, FK_CONSTRUCTOR);
		return hasConstructorOfType(type, ctorType);
	}

	bool hasCopyConstructor(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto otherType = builder.refType(type->getTag(), true, false, lang::ReferenceType::Kind::CppReference);
		return hasConstructorAccepting(type, otherType);
	}

	bool hasMoveConstructor(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto otherType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppRValueReference);
		return hasConstructorAccepting(type, otherType);
	}

	bool hasDefaultDestructor(const TagTypePtr& type) {
		const auto& record = type->getRecord();
		return record->hasDestructor() && isDefaultDestructor(record->getDestructor());
	}

	bool hasCopyAssignment(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto thisType = builder.refType(type->getTag());
		auto otherType = builder.refType(type->getTag(), true, false, lang::ReferenceType::Kind::CppReference);
		auto resType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppReference);
		auto funType = builder.functionType(TypeList{ thisType, otherType }, resType, FK_MEMBER_FUNCTION);
		return hasMemberOfType(type, utils::getMangledOperatorAssignName(), funType);
	}

	bool hasMoveAssignment(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto thisType = builder.refType(type->getTag());
		auto otherType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppRValueReference);
		auto resType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppReference);
		auto funType = builder.functionType(TypeList{ thisType, otherType }, resType, FK_MEMBER_FUNCTION);
		return hasMemberOfType(type, utils::getMangledOperatorAssignName(), funType);
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
