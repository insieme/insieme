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

#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace core {
namespace analysis {

	FunctionTypePtr getDefaultConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultConstructorType(thisType);
		auto name = builder.getLiteralForConstructor(ctorType);
		return core::analysis::markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, toVector(builder.variable(builder.refType(thisType))),
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
		return core::analysis::markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, toVector(thisParam, otherParam),
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
		return core::analysis::markAsDefaultMember(normalize(builder.lambdaExpr(ctorType, toVector(thisParam, otherParam),
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
		return core::analysis::markAsDefaultMember(normalize(builder.lambdaExpr(dtorType, toVector(builder.variable(builder.refType(thisType))),
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
		auto fun = core::analysis::markAsDefaultMember(normalize(builder.lambdaExpr(funType, toVector(thisParam, builder.variable(otherType)),
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
		auto fun = core::analysis::markAsDefaultMember(normalize(builder.lambdaExpr(funType, toVector(thisParam, builder.variable(otherType)),
		                                                                            res, name->getValue()->getValue())).as<LambdaExprPtr>());
		return builder.memberFunction(false, utils::getMangledOperatorAssignName(), fun);
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
