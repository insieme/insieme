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

#include "insieme/frontend/conversion/init_lists.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/materialize.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace frontend {
namespace conversion {

	namespace {
		void buildInitializerMemberFunction(Converter& converter, const core::LiteralPtr& literal, const core::TypePtr& ptrType,
		                                    const core::TypePtr& initListIRType, const std::string& bodyString) {
			auto& builder = converter.getIRBuilder();
			auto& refExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();

			auto buildMemberAccess = [&builder, &refExt](const core::ExpressionPtr& thisVar, const std::string memberName, const core::TypePtr& memberType) {
				return builder.callExpr(refExt.getRefMemberAccess(), core::lang::toPlainReference(builder.deref(thisVar)), builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(memberType));
			};

			// extract function type and param types
			auto funType = literal->getType().as<core::FunctionTypePtr>();
			core::VariableList params;
			for(const core::TypePtr& type : funType->getParameterTypes()) {
				params.push_back(builder.variable(core::transform::materialize(type)));
			}

			// build the symbol map to parse the body, as well as the member type
			std::map<string, core::NodePtr> symbols {
				{ "_m_array", buildMemberAccess(params[0], "_M_array", ptrType) },
				{ "_m_len", buildMemberAccess(params[0], "_M_len", builder.getLangBasic().getUInt8()) },
				{ "_m_original", buildMemberAccess(params[0], "_M_original", builder.getLangBasic().getBool()) },
				{ "_member_type", core::lang::PointerType(ptrType).getElementType() },
				{ "_deref_this", builder.deref(params[0]) },
				{ "_this_type", initListIRType },
			};
			// add additional access symbols for the remaining params
			unsigned index = 1;
			for(auto it = params.begin() + 1; it != params.end(); ++it, ++index) {
				symbols.insert({ format("_param_%d", index), *it });
			}

			// generate the body
			auto body = builder.parseStmt(bodyString, symbols);

			// add the ctor/dtor implementation to the IR TU
			auto lambda = builder.lambdaExpr(funType, params, body);
			converter.getIRTranslationUnit().addFunction(literal, lambda);
		}

		std::pair<core::LambdaExprPtr, core::ExpressionList> buildGenerationCtor(const core::GenericTypePtr& irThisType, const core::ExpressionPtr subEx,
		                                                                         const core::TypePtr& memberType, unsigned numElements,
		                                                                         const core::ExpressionPtr& callThisVar) {
			core::IRBuilder builder(irThisType.getNodeManager());
			auto& refExt = irThisType.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			auto buildMemberAccess = [&builder, &refExt](const core::ExpressionPtr& thisVar, const std::string memberName, const core::TypePtr& memberType) {
				return builder.callExpr(refExt.getRefMemberAccess(), core::lang::toPlainReference(builder.deref(thisVar)), builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(memberType));
			};

			// generate function param types, actual param variables and the argument expressions to call this function
			core::VariablePtr thisVar = builder.variable(builder.refType(irThisType));
			core::TypeList funParamTypes { irThisType };
			core::VariableList funParams { thisVar };
			core::ExpressionList callArguments { callThisVar };
			for(auto decl : subEx.as<core::InitExprPtr>()->getInitDecls()) {
				auto expr = decl->getInitialization();
				auto exprType = expr->getType();
				if(core::analysis::isRefType(exprType)) exprType = core::analysis::getReferencedType(exprType);
				auto paramType = builder.refType(exprType, true, false, core::lang::ReferenceType::Kind::CppReference);
				funParamTypes.push_back(paramType);
				funParams.push_back(builder.variable(paramType));

				// materialize parameter if necessary
				if (!core::lang::isReference(expr)) {
					expr = builder.refTemp(expr);
				}
				// pass value by reference
				callArguments.push_back(core::lang::toCppReference(expr));
			}
			core::ExpressionList initList;
			std::copy(funParams.cbegin()+1, funParams.cend(), std::back_inserter(initList));

			// build the body of the ctor, which will allocate a new array and copy the elements, assign the length and the original flag
			auto arrayType = core::lang::ArrayType::create(memberType, numElements);
			auto memloc = builder.undefinedNew(arrayType);
			auto arrayInitExpr = builder.initExpr(memloc, initList);

			auto arrayMemberAccess = buildMemberAccess(thisVar, "_M_array", core::lang::PointerType::create(memberType, true, false));
			auto ptrInit = core::lang::buildPtrCast(core::lang::buildPtrFromArray(arrayInitExpr), true, false);

			auto lenMemberAccess = buildMemberAccess(thisVar, "_M_len", builder.getLangBasic().getUInt8());
			auto originalMemberAccess = buildMemberAccess(thisVar, "_M_original", builder.getLangBasic().getBool());

			auto body = builder.compoundStmt({ builder.assign(arrayMemberAccess, ptrInit),
			                                   builder.assign(lenMemberAccess, builder.literal(format("%d", numElements), builder.getLangBasic().getUInt8())),
			                                   builder.assign(originalMemberAccess, builder.getLangBasic().getTrue()) });

			// finally create the new ctor lambda and return it along with the arguments needed to call it
			auto funType = builder.functionType(funParamTypes, irThisType, core::FunctionKind::FK_CONSTRUCTOR);
			return { builder.lambdaExpr(funType, funParams, body), callArguments };
		}
	}

	core::ExpressionPtr convertCxxStdInitializerListExpr(Converter& converter, const clang::CXXStdInitializerListExpr* stdInitListExpr) {
		core::ExpressionPtr retIr;

		auto& builder = converter.getIRBuilder();

		//convert sub expression and types
		auto subEx = converter.convertExpr(stdInitListExpr->getSubExpr());
		auto subExType = stdInitListExpr->getSubExpr()->getType().getTypePtr();
		auto initListIRType = converter.convertType(stdInitListExpr->getType());
		auto irThisType = builder.refType(initListIRType);
		auto recordType = llvm::dyn_cast<clang::RecordType>(stdInitListExpr->getType().getTypePtr()->getUnqualifiedDesugaredType());
		auto recordDecl = recordType->getAsCXXRecordDecl();
		frontend_assert(recordType && recordDecl) << "failed to get the std::initializer_list type declaration.";
		auto thisVar = core::lang::buildRefTemp(initListIRType);

		//extract size of sub expr
		frontend_assert(llvm::isa<clang::ConstantArrayType>(subExType)) << "std::initializer_list sub expression has no constant size array type.";
		auto numElements = llvm::cast<clang::ConstantArrayType>(subExType)->getSize().getSExtValue();

		// The member and pointer type of our member field
		core::TypePtr memberType = converter.convertType(llvm::cast<clang::ConstantArrayType>(subExType)->getElementType());
		core::TypePtr ptrType = core::lang::PointerType::create(memberType, true, false);

		core::LiteralPtr ctorToRemove;
		core::LiteralList ctorsToAdd;

		// iterate over the ctors generated by clang and modify their bodies
		for(auto ctorDecl : recordDecl->ctors()) {
			core::LiteralPtr ctorLiteral = converter.getFunMan()->lookup(ctorDecl);
			// this is the ctor used to build the init list. We will not make use of this ctor, so actually we can remove it from the record
			if(ctorDecl->getNumParams() == 2) {
				ctorToRemove = ctorLiteral;
				converter.getIRTranslationUnit().removeFunction(ctorLiteral);

				// The default ctor will just initialize the length to zero in it's body
			} else if(ctorDecl->isDefaultConstructor()) {
				buildInitializerMemberFunction(converter, ctorLiteral, ptrType, initListIRType, R"({
					_m_len = 0ul;
					_m_original = true;
				})");
				ctorsToAdd.push_back(ctorLiteral);
			}
		}

		// the body of all the move and assign ctors/operators
		std::string moveAssignBody = R"(
			_m_array = *ref_member_access(ref_kind_cast(_param_1, type_lit(plain)), lit("_M_array"), type_lit(ptr<_member_type, t, f>));
			_m_len = *ref_member_access(ref_kind_cast(_param_1, type_lit(plain)), lit("_M_len"), type_lit(uint<8>));
			_m_original = false;
		)";

		// generate copy and move ctor
		auto addCtor = [&](const core::TypePtr& otherType) {
			auto funType = builder.functionType(toVector<core::TypePtr>(irThisType, otherType), irThisType, core::FunctionKind::FK_CONSTRUCTOR);
			core::LiteralPtr ctorLiteral = builder.getLiteralForConstructor(funType);
			buildInitializerMemberFunction(converter, ctorLiteral, ptrType,initListIRType, std::string("{") + moveAssignBody + "}");
			ctorsToAdd.push_back(ctorLiteral);
		};
		addCtor(builder.refType(initListIRType, true, false, core::lang::ReferenceType::Kind::CppReference));
		addCtor(builder.refType(initListIRType, false, false, core::lang::ReferenceType::Kind::CppRValueReference));

		// generate dtor
		auto funType = builder.functionType(toVector<core::TypePtr>(irThisType), irThisType, core::FunctionKind::FK_DESTRUCTOR);
		core::LiteralPtr dtorLiteral = builder.getLiteralForDestructor(funType);
		buildInitializerMemberFunction(converter, dtorLiteral, ptrType, initListIRType, R"({
			if(*_m_original) {
				if(*_m_len != 0ul) {
					ref_delete(ptr_to_array(ptr_const_cast(*_m_array, type_lit(f))));
				}
			}
		})");

		// generate copy and move assignment operators
		auto generateMFun = [&](const core::TypePtr& otherType) {
			core::TypePtr resType = builder.refType(core::analysis::getReferencedType(irThisType), false, false, core::lang::ReferenceType::Kind::CppReference);
			auto funType = builder.functionType(toVector<core::TypePtr>(irThisType, otherType), resType, core::FunctionKind::FK_MEMBER_FUNCTION);
			auto name = insieme::utils::getMangledOperatorAssignName();
			auto mFunLit = builder.getLiteralForMemberFunction(funType, name);
			buildInitializerMemberFunction(converter, mFunLit, ptrType, initListIRType,
			                               std::string("{") + moveAssignBody + R"( return ref_kind_cast(_deref_this,type_lit(cpp_ref)); })");
			return builder.memberFunction(false, name, mFunLit);
		};
		auto copyAssignment = generateMFun(builder.refType(core::analysis::getReferencedType(irThisType), true, false, core::lang::ReferenceType::Kind::CppReference));
		auto moveAssignment = generateMFun(builder.refType(core::analysis::getReferencedType(irThisType), false, false, core::lang::ReferenceType::Kind::CppRValueReference));

		// generate special ctor with the correct number of arguments to call directly in order to preserve special cpp copy semantics
		auto generationCtor = buildGenerationCtor(irThisType, subEx, memberType, numElements, thisVar);
		auto generationCtorLit = builder.getLiteralForConstructor(generationCtor.first->getType());
		converter.getIRTranslationUnit().addFunction(generationCtorLit, generationCtor.first);
		ctorsToAdd.push_back(generationCtorLit);
		// a call to this ctor will is the result of this function here
		retIr = builder.callExpr(generationCtorLit->getType().as<core::FunctionTypePtr>()->getReturnType(), generationCtorLit, generationCtor.second);

		// now we have to replace the whole type in the IrTU, in order to set a custom dtor and add another member field
		auto key = initListIRType.as<core::GenericTypePtr>();
		const auto& oldRecord = converter.getIRTranslationUnit().getTypes().at(key)->getStruct();
		frontend_assert(oldRecord) << "Record has not been stored in TU previously";

		// add the new field
		core::FieldList fields(oldRecord->getFields()->getFields());
		if(!::any(fields.cbegin(), fields.cend(), [](const core::FieldPtr& field) { return field->getName()->getValue() == "_M_original"; })) {
			fields.push_back(builder.field("_M_original", builder.getLangBasic().getBool()));
		}
		// as well as the new ctors
		core::ExpressionList ctors(oldRecord->getConstructors()->getExpressions());
		for(auto ctor : ctorsToAdd) {
			if(!::contains(ctors, ctor)) ctors.push_back(ctor);
		}
		// and remove the ctor which we don't need
		if(ctorToRemove && ::contains(ctors, ctorToRemove)) {
			ctors.erase(std::remove(ctors.begin(), ctors.end(), ctorToRemove), ctors.end());
			converter.getIRTranslationUnit().removeFunction(ctorToRemove);
		}
		// finally, add both assignment operators
		core::MemberFunctionList mFuns(oldRecord->getMemberFunctions()->getMembers());
		if(!::contains(mFuns, copyAssignment)) mFuns.push_back(copyAssignment);
		if(!::contains(mFuns, moveAssignment)) mFuns.push_back(moveAssignment);

		// build the new record and replace the old one in the irTU
		auto newRecord = builder.structType(oldRecord->getName(), oldRecord->getParents(), builder.fields(fields), builder.expressions(ctors),
		                                    dtorLiteral, builder.boolValue(false), builder.memberFunctions(mFuns),
		                                    oldRecord->getPureVirtualMemberFunctions(), oldRecord->getStaticMemberFunctions());
		converter.getIRTranslationUnit().replaceType(key, newRecord);

		if(stdInitListExpr->isRValue()) retIr = builder.deref(retIr);
		return retIr;
	}

} // End namespace utils
} // End namespace frontend
} // End namespace insieme
