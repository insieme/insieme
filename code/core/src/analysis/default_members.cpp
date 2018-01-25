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
#include "insieme/core/transform/materialize.h"

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

		bool canCopyTrivially(const TypePtr& type) {
			// we can copy trivially, if it is a built-in type, a reference, an array or a tuple (and thus also pointer)
			return lang::isBuiltIn(type) || analysis::isRefType(type) || lang::isArray(type) || type.isa<TupleTypePtr>() || type.isa<TypeVariablePtr>();
		}

		template<typename GenTypeLG, typename GenTypeCTC, typename TagTypeCE>
		core::ExpressionPtr getGenericCallee(const core::TypePtr& fieldType,
		                                     const GenTypeLG& genTypeLiteralGenerator, const GenTypeCTC& genTypeCalleeTypeCreator,
		                                     const TagTypeCE& tagTypeCalleeExtractor) {
			core::IRBuilder builder(fieldType.getNodeManager());

			// if we are dealing with a generic type here, we are working pre-TU
			if(fieldType.isa<core::GenericTypePtr>()) {
				// create a substitute callee literal
				return genTypeLiteralGenerator(genTypeCalleeTypeCreator(builder.refType(fieldType)));
			}

			// otherwise we are working with a tag type. In this case return the actual callee from a lookup
			auto tagType = fieldType.isa<core::TagTypePtr>();
			assert_true(tagType) << "Don't know how to handle node type " << fieldType->getNodeType() << " here. Fields/Parents may only be of type GenericType or TagType";
			return tagTypeCalleeExtractor(tagType);
		}

		enum OperationType { Default, Copy, Move };

		core::ExpressionPtr getConstructorCallee(const core::TypePtr& fieldType, OperationType operation) {
			core::IRBuilder builder(fieldType.getNodeManager());
			return getGenericCallee(fieldType, [&builder](const core::FunctionTypePtr& funType) { return builder.getLiteralForConstructor(funType); },
			                        operation == Default ? buildDefaultDefaultConstructorType : (operation == Copy ? buildDefaultCopyConstructorType : buildDefaultMoveConstructorType),  // GenericType
			                        operation == Default ? getDefaultConstructor : (operation == Copy ? getCopyConstructor : getMoveConstructor));                                        // TagType
		}

		core::ExpressionPtr getDestructorCallee(const core::TypePtr& fieldType) {
			core::IRBuilder builder(fieldType.getNodeManager());
			return getGenericCallee(fieldType, [&builder](const core::FunctionTypePtr& funType) { return builder.getLiteralForDestructor(funType); },
			                        buildDefaultDestructorType, // GenericType
			                        getDestructor);             // TagType
		}

		core::ExpressionPtr getAssignmentCallee(const core::TypePtr& fieldType, OperationType operation) {
			assert_true(operation != Default);
			core::IRBuilder builder(fieldType.getNodeManager());
			return getGenericCallee(fieldType, [&builder](const core::FunctionTypePtr& funType) { return builder.getLiteralForMemberFunction(funType, utils::getMangledOperatorAssignName()); },
			                        operation == Copy ? buildDefaultCopyAssignOperatorType : buildDefaultMoveAssignOperatorType,  // GenericType
			                        operation == Copy ? getCopyAssignment : getMoveAssignment);                                   // TagType
		}

		core::StatementPtr buildCopyOrMoveConstructorBody(const core::VariablePtr& thisParam, const core::VariablePtr& otherParam,
		                                                  const ParentList& parents, const FieldList& fields, OperationType operation) {
			assert_true(operation != Default);
			core::IRBuilder builder(thisParam.getNodeManager());
			const auto& refExt = thisParam.getNodeManager().getLangExtension<lang::ReferenceExtension>();

			core::StatementList bodyStmts;
			// first call base class copy/move ctors
			for(const auto& parent : parents) {
				const auto& parentType = parent->getType();
				auto parentCtor = getConstructorCallee(parentType, operation);
				auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
				auto otherCallArg = lang::buildRefParentCast(otherParam, parentType);
				bodyStmts.push_back(builder.callExpr(parentCtor, callArg, otherCallArg));
			}

			// after that, copy/move all fields
			for(const auto& field : fields) {
				// get the type and build the member accesses
				const auto& fieldType = field->getType();
				auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
				                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
				auto otherFieldAccess = builder.callExpr(refExt.getRefMemberAccess(), lang::toPlainReference(otherParam),
				                                         builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
				// if the field is a cpp_ref or cpp_rrref we need to add another deref here - but then the ctor won't be used anyways

				// if the field can be copied trivially
				if(canCopyTrivially(fieldType)) {
					// we initialize using an initExpr
					bodyStmts.push_back(builder.initExpr(fieldAccess, builder.deref(otherFieldAccess)));

					// otherwise we initialize by calling the copy/move constructor
				} else {
					auto fieldCtor = getConstructorCallee(fieldType, operation);
					bodyStmts.push_back(builder.callExpr(fieldCtor, fieldAccess,
					                                     lang::buildRefKindCast(otherFieldAccess,
					                                                            operation == Copy ? lang::ReferenceType::Kind::CppReference : lang::ReferenceType::Kind::CppRValueReference)));
				}
			}

			return builder.compoundStmt(bodyStmts);
		}

		core::StatementPtr buildCopyOrMoveAssignmentBody(const core::VariablePtr& thisParam, const core::VariablePtr& otherParam,
		                                                 const ParentList& parents, const FieldList& fields, OperationType operation) {
			assert_true(operation != Default);
			core::IRBuilder builder(thisParam.getNodeManager());
			const auto& refExt = thisParam.getNodeManager().getLangExtension<lang::ReferenceExtension>();

			core::StatementList bodyStmts;
			// first call base class copy/move assignments
			for(const auto& parent : parents) {
				const auto& parentType = parent->getType();
				auto parentMethodCallee = getAssignmentCallee(parentType, operation);
				auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
				auto otherCallArg = lang::buildRefParentCast(otherParam, parentType);
				bodyStmts.push_back(builder.callExpr(parentMethodCallee, callArg, otherCallArg));
			}

			// after that, copy/move assign all fields
			for(const auto& field : fields) {
				// get the type and build the member accesses
				const auto& fieldType = field->getType();
				auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
				                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
				auto otherFieldAccess = builder.callExpr(refExt.getRefMemberAccess(), lang::toPlainReference(otherParam),
				                                         builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
				// if the field is a cpp_ref or cpp_rrref we need to add another deref here - but then the assignment won't be used anyways

				// if the field can be copied trivially
				if(canCopyTrivially(fieldType)) {
					// we assign the field
					bodyStmts.push_back(builder.callExpr(refExt.getRefAssign(), fieldAccess, builder.deref(otherFieldAccess)));

					// otherwise we initialize by calling the copy/move assignment operators
				} else {
					auto targetMethod = getAssignmentCallee(fieldType, operation);
					bodyStmts.push_back(builder.callExpr(targetMethod, fieldAccess,
					                                     lang::buildRefKindCast(otherFieldAccess,
					                                                            operation == Copy ? lang::ReferenceType::Kind::CppReference : lang::ReferenceType::Kind::CppRValueReference)));
				}
			}

			// last, add a correct return statement
			bodyStmts.push_back(builder.returnStmt(lang::buildRefKindCast(builder.deref(thisParam), lang::ReferenceType::Kind::CppReference)));

			return builder.compoundStmt(bodyStmts);
		}
	}


	FunctionTypePtr buildDefaultDefaultConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr buildDefaultDefaultConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields, const FieldInitMap& fieldInits) {
		core::IRBuilder builder(thisType.getNodeManager());
		const auto& refExt = thisType.getNodeManager().getLangExtension<lang::ReferenceExtension>();
		auto ctorType = buildDefaultDefaultConstructorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));

		// create the body
		core::StatementList bodyStmts;
		// first call base class ctors
		for(const auto& parent : parents) {
			const auto& parentType = parent->getType();
			auto parentCtor = getConstructorCallee(parentType, OperationType::Default);
			auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
			bodyStmts.push_back(builder.callExpr(parentCtor, callArg));
		}

		// after that, init all fields
		for(const auto& field : fields) {
			// get the type and build the member access
			const auto& fieldType = field->getType();
			auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
			                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));

			const auto& fieldInit = fieldInits.find(field);
			// if the field can be copied trivially
			if(canCopyTrivially(fieldType)) {
				// we only initialize it if it has an initialization
				if(fieldInit != fieldInits.end()) {
					bodyStmts.push_back(builder.initExpr(fieldAccess, transform::castInitializationIfNotMaterializing(fieldType, fieldInit->second)));
				}

				// otherwise we initialize it in either case
			} else {
				// if it has no initialization, we call the default constructor
				if(fieldInit == fieldInits.end()) {
					auto targetCtor = getConstructorCallee(fieldType, OperationType::Default);
					bodyStmts.push_back(builder.callExpr(targetCtor, fieldAccess));

					// otherwise we call the constructor accepting the type of the initialization expression
				} else {
					if(fieldType.isa<core::TagTypePtr>()) {
						assert_not_implemented() << "Support for initialization of members of TagType type in default constructor not implemented yet";
					}
					auto init = transform::castInitializationIfNotMaterializing(fieldType, fieldInit->second);
					// strip ctor calls with red_temp as memory location
					if(core::analysis::isConstructorCall(init)) {
						auto initCall = init.as<core::CallExprPtr>();
						if(refExt.isCallOfRefTemp(initCall->getArgument(0))) {
							init = initCall->getArgument(1);
						}
					}
					auto targetDefaultCtorType = buildDefaultDefaultConstructorType(builder.refType(fieldType));
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


	FunctionTypePtr buildDefaultCopyConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr buildDefaultCopyConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = buildDefaultCopyConstructorType(thisType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);

		auto body = buildCopyOrMoveConstructorBody(thisParam, otherParam, parents, fields, OperationType::Copy);
		return buildConstructorLambda(ctorType, toVector(thisParam, otherParam), body);
	}


	FunctionTypePtr buildDefaultMoveConstructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
		return builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
	}
	LambdaExprPtr buildDefaultMoveConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto ctorType = buildDefaultMoveConstructorType(thisType);
		const auto& otherType = ctorType->getParameterType(1);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(otherType);

		auto body = buildCopyOrMoveConstructorBody(thisParam, otherParam, parents, fields, OperationType::Move);
		return buildConstructorLambda(ctorType, toVector(thisParam, otherParam), body);
	}


	FunctionTypePtr buildDefaultDestructorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		return builder.functionType(toVector(thisType), thisType, FK_DESTRUCTOR);
	}
	LambdaExprPtr buildDefaultDestructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		const auto& refExt = thisType.getNodeManager().getLangExtension<lang::ReferenceExtension>();
		auto dtorType = buildDefaultDestructorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));

		// create the body
		core::StatementList bodyStmts;
		// destruct non-builtin fields in reverse order
		core::FieldList reverseFields(fields);
		::reverse(reverseFields);
		for(const auto& field : reverseFields) {
			const auto& fieldType = field->getType();
			auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisParam),
			                                    builder.getIdentifierLiteral(field->getName()), builder.getTypeLiteral(fieldType));
			// if the field can not be copied trivially
			if(!canCopyTrivially(fieldType)) {
				auto targetDtor = getDestructorCallee(fieldType);
				bodyStmts.push_back(builder.callExpr(targetDtor, fieldAccess));
			}
		}

		// call super class dtors in reverse order
		core::ParentList reverseParents(parents);
		::reverse(reverseParents);
		for(const auto& parent : reverseParents) {
			const auto& parentType = parent->getType();
			auto targetDtor = getDestructorCallee(parentType);
			auto callArg = lang::buildRefParentCast(builder.deref(thisParam), parentType);
			bodyStmts.push_back(builder.callExpr(targetDtor, callArg));
		}

		auto body = builder.compoundStmt(bodyStmts);
		return buildDestructorLambda(dtorType, toVector(thisParam), body);
	}


	FunctionTypePtr buildDefaultCopyAssignOperatorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
		TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
	}
	MemberFunctionPtr buildDefaultCopyAssignOperator(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto funType = buildDefaultCopyAssignOperatorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(funType->getParameterType(1));

		auto body = buildCopyOrMoveAssignmentBody(thisParam, otherParam, parents, fields, OperationType::Copy);
		return buildAssignmentLambda(funType, toVector(thisParam, otherParam), body);
	}


	FunctionTypePtr buildDefaultMoveAssignOperatorType(const TypePtr& thisType) {
		core::IRBuilder builder(thisType.getNodeManager());
		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
		TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
		return builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
	}
	MemberFunctionPtr buildDefaultMoveAssignOperator(const TypePtr& thisType, const ParentList& parents, const FieldList& fields) {
		core::IRBuilder builder(thisType.getNodeManager());
		auto funType = buildDefaultMoveAssignOperatorType(thisType);
		auto thisParam = builder.variable(builder.refType(thisType));
		auto otherParam = builder.variable(funType->getParameterType(1));

		auto body = buildCopyOrMoveAssignmentBody(thisParam, otherParam, parents, fields, OperationType::Move);
		return buildAssignmentLambda(funType, toVector(thisParam, otherParam), body);
	}



	namespace {
		LambdaExprPtr getConstructorByType(const TagTypePtr& type, const FunctionTypePtr& ctorType) {
			auto res = analysis::hasConstructorOfType(type, ctorType);
			assert_true(res) << "Passed TagType " << *type << " doesn't have a destructor of type " << *ctorType;
			return res->as<LambdaExprPtr>();
		}

		LambdaExprPtr getAssignmentByType(const TagTypePtr& type, const FunctionTypePtr& assignmentType) {
			for(const auto& cur : type->getRecord()->getMemberFunctions()) {
				if(cur->getImplementation()->getType() == assignmentType && cur->getNameAsString() == utils::getMangledOperatorAssignName()) {
					return cur->getImplementation().as<LambdaExprPtr>();
				}
			}
			assert_fail() << "Passed TagType " << *type << " doesn't have an assignment operator of type " << *assignmentType;
			return {};
		}
	}

	bool hasDefaultConstructor(const TagTypePtr& type) {
		return analysis::hasConstructorOfType(type, buildDefaultDefaultConstructorType(lang::buildRefType(type->getTag())));
	}

	LambdaExprPtr getDefaultConstructor(const TagTypePtr& type) {
		return type->peel(getConstructorByType(type, buildDefaultDefaultConstructorType(lang::buildRefType(type->getTag()))));
	}

	bool hasCopyConstructor(const TagTypePtr& type) {
		return analysis::hasConstructorOfType(type, buildDefaultCopyConstructorType(lang::buildRefType(type->getTag())));
	}

	LambdaExprPtr getCopyConstructor(const TagTypePtr& type) {
		return type->peel(getConstructorByType(type, buildDefaultCopyConstructorType(lang::buildRefType(type->getTag()))));
	}

	bool hasMoveConstructor(const TagTypePtr& type) {
		return analysis::hasConstructorOfType(type, buildDefaultMoveConstructorType(lang::buildRefType(type->getTag())));
	}

	LambdaExprPtr getMoveConstructor(const TagTypePtr& type) {
		return type->peel(getConstructorByType(type, buildDefaultMoveConstructorType(lang::buildRefType(type->getTag()))));
	}

	bool hasDestructor(const TagTypePtr& type) {
		return type->getRecord()->hasDestructor();
	}

	LambdaExprPtr getDestructor(const TagTypePtr& type) {
		return type->peel(type->getRecord()->getDestructor().as<LambdaExprPtr>());
	}

	bool hasCopyAssignment(const TagTypePtr& type) {
		return analysis::hasMemberOfType(type, utils::getMangledOperatorAssignName(), buildDefaultCopyAssignOperatorType(lang::buildRefType(type->getTag())));
	}

	LambdaExprPtr getCopyAssignment(const TagTypePtr& type) {
		return type->peel(getAssignmentByType(type, buildDefaultCopyAssignOperatorType(lang::buildRefType(type->getTag()))));
	}

	bool hasMoveAssignment(const TagTypePtr& type) {
		return analysis::hasMemberOfType(type, utils::getMangledOperatorAssignName(), buildDefaultMoveAssignOperatorType(lang::buildRefType(type->getTag())));
	}

	LambdaExprPtr getMoveAssignment(const TagTypePtr& type) {
		return type->peel(getAssignmentByType(type, buildDefaultMoveAssignOperatorType(lang::buildRefType(type->getTag()))));
	}

	bool isaDefaultConstructor(const ExpressionPtr& ctor) {
		if (auto lambda = ctor.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isConstructor() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool isaDefaultDestructor(const ExpressionPtr& dtor) {
		if (auto lambda = dtor.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isDestructor() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool isaDefaultAssignment(const MemberFunctionPtr& memberFunction) {
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
