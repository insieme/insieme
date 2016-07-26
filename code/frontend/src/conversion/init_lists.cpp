/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/conversion/init_lists.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/materialize.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/utils/macros.h"


namespace insieme {
namespace frontend {
namespace conversion {

	namespace {
		void buildInitializerMemberFunction(Converter& converter, const core::LiteralPtr& literal, const core::TypePtr& ptrType,
		                                    const std::string& bodyString) {
			auto& builder = converter.getIRBuilder();
			auto& refExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();

			auto buildMemberAccess = [&builder, &refExt](const core::ExpressionPtr& thisVar, const std::string memberName, const core::TypePtr& memberType) {
				return builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisVar), builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(memberType));
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

		// iterate over the ctors generated by clang and modify their bodies
		for(auto ctorDecl : recordDecl->ctors()) {
			core::LiteralPtr ctorLiteral = converter.getFunMan()->lookup(ctorDecl);

			// this is the ctor used to build the init list. we need to store the length, allocate the memory and copy/assign the elements in here.
			// this will also be the ctor which get's called on creation and thus we return a complete call to it
			if(ctorDecl->getNumParams() == 2) {
				// ctor decl: constexpr initializer_list<T>(T* x, size_t s)

				// check whether we need to copy the elements or can simply assign them
				bool copyElements = false;
				auto tuIt = converter.getIRTranslationUnit().getTypes().find(memberType.as<core::GenericTypePtr>());
				if(tuIt != converter.getIRTranslationUnit().getTypes().cend() && !core::analysis::isTrivial(tuIt->second)) copyElements = true;

				buildInitializerMemberFunction(converter, ctorLiteral, ptrType, std::string("") + R"({
					var uint<inf> array_len = num_cast(*_param_2, type_lit(uint<inf>));
					_m_array = ptr_const_cast(ptr_from_array(ref_new(type_lit(array<_member_type, #array_len>))), type_lit(t));
					_m_len = *_param_2;
					_m_original = true;
					for(int<8> it = 0l .. num_cast(*_param_2, type_lit(int<8>))) { )" +
					(copyElements ?
							R"( ref_assign(ptr_subscript(ptr_const_cast(*_m_array, type_lit(f)), it), ref_cast(ptr_subscript(*_param_1, it), type_lit(t), type_lit(f), type_lit(cpp_ref))); )" :
							R"( ptr_subscript(ptr_const_cast(*_m_array, type_lit(f)), it) = *ptr_subscript(*_param_1, it); )") + R"(
					}
				})");

				// build call to the specific constructor
				core::ExpressionList args { thisVar, core::lang::buildPtrFromArray(subEx),
				                            builder.numericCast(builder.uintLit(numElements), builder.getLangBasic().getUInt8()) };
				retIr = builder.callExpr(ctorLiteral->getType().as<core::FunctionTypePtr>()->getReturnType(), ctorLiteral, args);

				// The default ctor will just initialize the length to zero in it's body
			} else if(ctorDecl->isDefaultConstructor()) {
				// ctor decl: constexpr initializer_list<T>()
				buildInitializerMemberFunction(converter, ctorLiteral, ptrType,R"({
					_m_len = 0ul;
					_m_original = true;
				})");
			}
		}

		// generate copy ctor
		auto otherType = builder.refType(initListIRType, true, false, core::lang::ReferenceType::Kind::CppReference);
		auto funType = builder.functionType(toVector<core::TypePtr>(irThisType, otherType), irThisType, core::FunctionKind::FK_CONSTRUCTOR);
		core::LiteralPtr copyCtorLiteral = builder.getLiteralForConstructor(funType);
		buildInitializerMemberFunction(converter, copyCtorLiteral, ptrType, R"({
			_m_array = *ref_member_access(_param_1, lit("_M_array"), type_lit(ptr<_member_type, t, f>));
			_m_len = *ref_member_access(_param_1, lit("_M_len"), type_lit(uint<8>));
			_m_original = false;
		})");

		// generate dtor
		funType = builder.functionType(toVector<core::TypePtr>(irThisType), irThisType, core::FunctionKind::FK_DESTRUCTOR);
		core::LiteralPtr dtorLiteral = builder.getLiteralForDestructor(funType);
		buildInitializerMemberFunction(converter, dtorLiteral, ptrType, R"({
			if(*_m_original && *_m_len != 0ul) {
				ref_delete(ptr_to_ref(ptr_const_cast(*_m_array, type_lit(f))));
			}
		})");

		// now we have to replace the whole type in the IrTU, in order to set a custom dtor and add another member field
		auto key = initListIRType.as<core::GenericTypePtr>();
		const auto& oldRecord = converter.getIRTranslationUnit().getTypes().at(key)->getStruct();
		frontend_assert(oldRecord) << "Record has not been stored in TU previously";

		core::FieldList fields(oldRecord->getFields()->getFields());
		if(!::any(fields.cbegin(), fields.cend(), [](const core::FieldPtr& field) { return field->getName()->getValue() == "_M_original"; })) {
			fields.push_back(builder.field("_M_original", builder.getLangBasic().getBool()));
		}

		auto newRecord = builder.structType(oldRecord->getName(), oldRecord->getParents(), builder.fields(fields), oldRecord->getConstructors(),
		                                    dtorLiteral, builder.boolValue(false), oldRecord->getMemberFunctions(), oldRecord->getPureVirtualMemberFunctions());
		converter.getIRTranslationUnit().replaceType(key, newRecord);

		//return call to special constructor
		frontend_assert(retIr) << "failed to convert std::initializer_list expression.";
		return retIr;
	}

} // End namespace utils
} // End namespace frontend
} // End namespace insieme
