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
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/map_utils.h"


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

		TypePtr checkForTagType(const TypePtr& type) {
			if(type.isa<core::TagTypePtr>()) {
				assert_not_implemented() << "Support for using TagTypes in default member initializations not implemented";
			}
			return type;
		}

		bool canCopyTrivially(const TypePtr& type) {
			// we can copy trivially, if it is a built-in type, a reference, an array or a tuple (and thus also pointer)
			return lang::isBuiltIn(type) || analysis::isRefType(type) || lang::isArray(type) || type.isa<TupleTypePtr>();
		}

		core::StatementPtr buildCopyOrMoveConstructorBody(const core::VariablePtr& thisParam, const core::VariablePtr& otherParam,
		                                                  const ParentList& parents, const FieldList& fields, bool isCopy) {
			core::IRBuilder builder(thisParam.getNodeManager());
			const auto& refExt = thisParam.getNodeManager().getLangExtension<lang::ReferenceExtension>();

			core::StatementList bodyStmts;
			// first call base class copy/move ctors
			for(const auto& parent : parents) {
				const auto& parentType = checkForTagType(parent->getType());
				auto parentFunType = isCopy ? getDefaultCopyConstructorType(builder.refType(parentType)) : getDefaultMoveConstructorType(builder.refType(parentType));
				auto parentCtorLit = builder.getLiteralForConstructor(parentFunType);
				auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
				auto otherCallArg = lang::buildRefParentCast(otherParam, parentType);
				bodyStmts.push_back(builder.callExpr(parentCtorLit, callArg, otherCallArg));
			}

			// after that, copy/move all fields
			for(const auto& field : fields) {
				// get the type and build the member accesses
				const auto& fieldType = checkForTagType(field->getType());
				auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
				                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
				auto otherFieldAccess = builder.callExpr(refExt.getRefMemberAccess(), otherParam,
				                                         builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));

				// if the field can be copied trivially
				if(canCopyTrivially(fieldType)) {
					// we initialize using an initExpr
					bodyStmts.push_back(builder.initExpr(fieldAccess, builder.deref(otherFieldAccess)));

					// otherwise we initialize by calling the copy/move constructor
				} else {
					auto refFieldType = builder.refType(fieldType);
					auto targetCtor = builder.getLiteralForConstructor(isCopy ? getDefaultCopyConstructorType(refFieldType) : getDefaultMoveConstructorType(refFieldType));
					bodyStmts.push_back(builder.callExpr(targetCtor, fieldAccess,
					                                     lang::buildRefKindCast(otherFieldAccess,
					                                                            isCopy ? lang::ReferenceType::Kind::CppReference : lang::ReferenceType::Kind::CppRValueReference)));
				}
			}

			return builder.compoundStmt(bodyStmts);
		}

		core::StatementPtr buildCopyOrMoveAssignmentBody(const core::VariablePtr& thisParam, const core::VariablePtr& otherParam,
		                                                 const ParentList& parents, const FieldList& fields, bool isCopy) {
			core::IRBuilder builder(thisParam.getNodeManager());
			const auto& refExt = thisParam.getNodeManager().getLangExtension<lang::ReferenceExtension>();

			core::StatementList bodyStmts;
			// first call base class copy/move assignments
			for(const auto& parent : parents) {
				const auto& parentType = checkForTagType(parent->getType());
				auto parentFunType = isCopy ? getDefaultCopyAssignOperatorType(builder.refType(parentType)) : getDefaultMoveAssignOperatorType(builder.refType(parentType));
				auto parentMethodLit = builder.getLiteralForMemberFunction(parentFunType, utils::getMangledOperatorAssignName());
				auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
				auto otherCallArg = lang::buildRefParentCast(otherParam, parentType);
				bodyStmts.push_back(builder.callExpr(parentMethodLit, callArg, otherCallArg));
			}

			// after that, copy/move assign all fields
			for(const auto& field : fields) {
				// get the type and build the member accesses
				const auto& fieldType = checkForTagType(field->getType());
				auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
				                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
				auto otherFieldAccess = builder.callExpr(refExt.getRefMemberAccess(), otherParam,
				                                         builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));

				// if the field can be copied trivially
				if(canCopyTrivially(fieldType)) {
					// we assign the field
					bodyStmts.push_back(builder.callExpr(refExt.getRefAssign(), fieldAccess, builder.deref(otherFieldAccess)));

					// otherwise we initialize by calling the copy/move assignment operators
				} else {
					auto refFieldType = builder.refType(fieldType);
					auto targetMethod = builder.getLiteralForMemberFunction(isCopy ? getDefaultCopyAssignOperatorType(refFieldType) : getDefaultMoveAssignOperatorType(refFieldType),
					                                                        utils::getMangledOperatorAssignName());
					bodyStmts.push_back(builder.callExpr(targetMethod, fieldAccess,
					                                     lang::buildRefKindCast(otherFieldAccess,
					                                                            isCopy ? lang::ReferenceType::Kind::CppReference : lang::ReferenceType::Kind::CppRValueReference)));
				}
			}

			// last, add a correct return statement
			bodyStmts.push_back(builder.returnStmt(lang::buildRefKindCast(builder.deref(thisParam), lang::ReferenceType::Kind::CppReference)));

			return builder.compoundStmt(bodyStmts);
		}
	}


	FunctionTypePtr getDefaultConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields, const FieldInitMap& fieldInits) {
		core::IRBuilder builder(thisType.getNodeManager());
		const auto& refExt = thisType.getNodeManager().getLangExtension<lang::ReferenceExtension>();
		auto ctorType = getDefaultConstructorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));

		// create the body
		core::StatementList bodyStmts;
		// first call base class ctors
		for(const auto& parent : parents) {
			const auto& parentType = checkForTagType(parent->getType());
			auto parentFunType = getDefaultConstructorType(builder.refType(parentType));
			auto parentCtorLit = builder.getLiteralForConstructor(parentFunType);
			auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
			bodyStmts.push_back(builder.callExpr(parentCtorLit, callArg));
		}

		// after that, init all fields
		for(const auto& field : fields) {
			// get the type and build the member access
			const auto& fieldType = checkForTagType(field->getType());
			auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
			                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));

			const auto& fieldInit = fieldInits.find(field);
			// if the field can be copied trivially
			if(canCopyTrivially(fieldType)) {
				// we only initialize it if it has an initialization
				if(fieldInit != fieldInits.end()) {
					bodyStmts.push_back(builder.initExpr(fieldAccess, fieldInit->second));
				}

				// otherwise we initialize it in either case
			} else {
				auto refFieldType = builder.refType(fieldType);
				// if it has no initialization, we call the default constructor
				if(fieldInit == fieldInits.end()) {
					auto targetCtor = builder.getLiteralForConstructor(getDefaultConstructorType(refFieldType));
					bodyStmts.push_back(builder.callExpr(targetCtor, fieldAccess));

					// otherwise we call the constructor accepting the type of the initialization expression
				} else {
					auto init = fieldInit->second;
					// strip ctor calls with red_temp as memory location
					if(core::analysis::isConstructorCall(init)) {
						auto initCall = init.as<core::CallExprPtr>();
						if(refExt.isCallOfRefTemp(initCall->getArgument(0))) {
							init = initCall->getArgument(1);
						}
					}
					auto targetDefaultCtorType = getDefaultConstructorType(refFieldType);
					auto targetCtorType = builder.functionType({ targetDefaultCtorType.getParameterType(0), init->getType() },
					                                           targetDefaultCtorType->getReturnType(), targetDefaultCtorType->getKind());
					auto targetCtor = builder.getLiteralForConstructor(targetCtorType);
					bodyStmts.push_back(builder.callExpr(targetCtor, fieldAccess, init));
				}
			}
		}

		auto body = builder.compoundStmt(bodyStmts);
		return buildConstructorLambda(ctorType, toVector(thisParam), body);
	}


	FunctionTypePtr getDefaultCopyConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultCopyConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultCopyConstructorType(thisType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);

		auto body = buildCopyOrMoveConstructorBody(thisParam, otherParam, parents, fields, true);
		return buildConstructorLambda(ctorType, toVector(thisParam, otherParam), body);
	}


	FunctionTypePtr getDefaultMoveConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
		return builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr getDefaultMoveConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = getDefaultMoveConstructorType(thisType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);

		auto body = buildCopyOrMoveConstructorBody(thisParam, otherParam, parents, fields, false);
		return buildConstructorLambda(ctorType, toVector(thisParam, otherParam), body);
	}


	FunctionTypePtr getDefaultDestructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_DESTRUCTOR);
	}
	LambdaExprPtr getDefaultDestructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		const auto& refExt = thisType.getNodeManager().getLangExtension<lang::ReferenceExtension>();
		auto dtorType = getDefaultDestructorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));

		// create the body
		core::StatementList bodyStmts;
		// destruct non-builtin fields in reverse order
		core::FieldList reverseFields(fields);
		::reverse(reverseFields);
		for(const auto& field : reverseFields) {
			const auto& fieldType = checkForTagType(field->getType());
			auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
			                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
			// if the field can not be copied trivially
			if(!canCopyTrivially(fieldType)) {
				auto refFieldType = builder.refType(fieldType);
				auto targetDtor = builder.getLiteralForDestructor(getDefaultDestructorType(refFieldType));
				bodyStmts.push_back(builder.callExpr(targetDtor, fieldAccess));
			}
		}

		// call super class dtors in reverse order
		core::ParentList reverseParents(parents);
		::reverse(reverseParents);
		for(const auto& parent : reverseParents) {
			const auto& parentType = checkForTagType(parent->getType());
			auto targetDtor = builder.getLiteralForDestructor(getDefaultDestructorType(builder.refType(parentType)));
			auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
			bodyStmts.push_back(builder.callExpr(targetDtor, callArg));
		}

		auto body = builder.compoundStmt(bodyStmts);
		return buildDestructorLambda(dtorType, toVector(thisParam), body);
	}


	FunctionTypePtr getDefaultCopyAssignOperatorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
		TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
	}
	MemberFunctionPtr getDefaultCopyAssignOperator(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto funType = getDefaultCopyAssignOperatorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(funType->getParameterType(1));

		auto body = buildCopyOrMoveAssignmentBody(thisParam, otherParam, parents, fields, true);
		return buildAssignmentLambda(funType, toVector(thisParam, otherParam), body);
	}


	FunctionTypePtr getDefaultMoveAssignOperatorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
		TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
	}
	MemberFunctionPtr getDefaultMoveAssignOperator(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto funType = getDefaultMoveAssignOperatorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(funType->getParameterType(1));

		auto body = buildCopyOrMoveAssignmentBody(thisParam, otherParam, parents, fields, false);
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
