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
 *
 */
#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/variable_manager.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/default_delete_member_semantics.h"
#include "insieme/core/annotations/naming.h"

#include <boost/algorithm/string/predicate.hpp>

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {

	namespace {

		core::ExpressionPtr generateLambdaCaptureAccess(const Converter& converter, const clang::FieldDecl* fieldDecl, const core::ExpressionPtr& thisExpr) {
			auto& builder = converter.getIRBuilder();
			string memName = frontend::utils::getNameForField(fieldDecl, converter.getSourceManager());
			auto access = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>().getRefMemberAccess();
			auto retType = converter.convertType(fieldDecl->getType());
			core::ExpressionPtr mem = builder.callExpr(access, thisExpr, builder.getIdentifierLiteral(memName), builder.getTypeLiteral(retType));
			if(core::lang::isCppReference(retType) || core::lang::isCppRValueReference(retType)) mem = builder.deref(mem);
			return mem;
		}

		/// build a mapping of captured lambda parameters to functors which generate an IR access for them within the lambda context
		/// -> used in variable manager when resolving decl refs and this expressions
		state::VariableManager::LambdaScope generateLambdaMapping(const Converter& converter, const clang::CXXRecordDecl* classDecl) {
			state::VariableManager::LambdaScope lScope;
			if(!classDecl->isLambda()) return lScope;
			lScope.setIsaLambda();
			llvm::DenseMap<const clang::VarDecl*, clang::FieldDecl*> clangCaptures;
			clang::FieldDecl *thisField;
			classDecl->getCaptureFields(clangCaptures, thisField);
			for(auto capture: clangCaptures) {
				// build a lambda which builds the field access when provided with the this expression
				lScope[capture.first] = [capture, &converter](const core::ExpressionPtr& thisExpr) {
					return generateLambdaCaptureAccess(converter, capture.second, thisExpr);
				};
			}
			if(thisField) {
				lScope.setThisGenerator([thisField, &converter](const core::ExpressionPtr& thisExpr) {
					return generateLambdaCaptureAccess(converter, thisField, thisExpr);
				});
			}
			return lScope;
		}

		/// for C++ Lambdas which support implicit conversion to function pointers, clang generates an "__invoke" static method with an empty body
		/// here we generate this body by copying the operator() body (static variables need to be multiversioning-safe in INSPIRE)
		void generateLambdaInvokeOperator(const std::vector<clang::CXXMethodDecl*>& methodDecls, const clang::CXXRecordDecl* classDecl,
			                              const core::MemberFunctionList& members, Converter& converter) {
			auto& builder = converter.getIRBuilder();
			// deal with the invoke operator on lambdas
			for(auto mem : methodDecls) {
				mem = mem->getCanonicalDecl();
				// if we have an automatically generated static "__invoke" function in a lambda, copy its body from operator()
				if(classDecl->isLambda() && mem->getNameAsString() == "__invoke" && mem->hasBody()) {
					auto opIt = std::find_if(members.cbegin(), members.cend(), [&](const core::MemberFunctionPtr& mf) {
						return boost::starts_with(mf->getNameAsString(), insieme::utils::getMangledOperatorCallName());
					});
					frontend_assert(opIt != members.cend());
					auto translatedLiteral = converter.getFunMan()->lookup(mem);
					auto translatedOperator = converter.getIRTranslationUnit().getFunctions()[(*opIt)->getImplementation().as<core::LiteralPtr>()];
					core::VariableList params;
					std::copy(translatedOperator->getParameterList().begin() + 1, translatedOperator->getParameterList().end(), std::back_inserter(params));
					core::LambdaExprPtr replacement = builder.lambdaExpr(translatedLiteral->getType().as<core::FunctionTypePtr>(), params,
						                                                 translatedOperator->getBody(), translatedLiteral->getStringValue());
					converter.getIRTranslationUnit().replaceFunction(translatedLiteral, replacement);
				}
			}
		}
	}

	//---------------------------------------------------------------------------------------------------------------------
	//										CXX CLANG TYPE CONVERTER
	//---------------------------------------------------------------------------------------------------------------------

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								BUILTIN TYPES
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitPointerType(const PointerType* ptrTy) {
		return TypeConverter::VisitPointerType(ptrTy);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitTagType(const TagType* tagType) {
		VLOG(2) << "CXXTypeConverter::VisitTagType " << tagType << std::endl;

		// first, try lookup
		auto clangRecTy = llvm::dyn_cast<RecordType>(tagType);
		if(clangRecTy) {
			auto recordDecl = clangRecTy->getDecl();
			// if type is complete, simply return it.
			// also return if we are lower on the type conversion stack and already generated an entry for this type
			if(converter.getRecordMan()->contains(recordDecl)
					&& (converter.getRecordMan()->isFinishing(recordDecl) || converter.getRecordMan()->isDeclOnlyConversion())) {
				return converter.getRecordMan()->lookup(recordDecl);
			}
		}

		// base conversion
		core::TypePtr retTy = TypeConverter::VisitTagType(tagType);
		LOG_TYPE_CONVERSION(tagType, retTy);

		// if not a GenericType mapping to a TagType (generated for struct type) we don't need to do anything more
		auto genTy = retTy.isa<core::GenericTypePtr>();
		if(!genTy) return retTy;
		auto& tuTypes = converter.getIRTranslationUnit().getTypes();
		auto irTuIt = tuTypes.find(genTy);
		if(irTuIt == tuTypes.end()) return retTy;
		retTy = irTuIt->second;

		// for C++ classes/structs, we need to add members and parents
		const clang::CXXRecordDecl* classDecl = llvm::dyn_cast_or_null<clang::CXXRecordDecl>(tagType->getDecl());
		if(!classDecl || !classDecl->getDefinition()) { return genTy; }

		// get struct type for easier manipulation
		auto tagTy = retTy.as<core::TagTypePtr>();
		auto recordTy = tagTy->getRecord();

		// decl only conversion start
		converter.getRecordMan()->incrementConversionStackDepth();

		// get parents if any
		core::ParentList parents;
		for(auto base : classDecl->bases()) {
			// visit the parent to build its type
			auto parentIrType = convert(base.getType());
			parents.push_back(builder.parent(base.isVirtual(), parentIrType));
		}
		auto irParents = builder.parents(parents);

		// get methods and instantiated template methods
		std::vector<clang::CXXMethodDecl*> methodDecls;
		std::copy(classDecl->method_begin(), classDecl->method_end(), std::back_inserter(methodDecls));
		for(auto d : classDecl->decls()) {
			if(clang::FunctionTemplateDecl* ftd = llvm::dyn_cast<clang::FunctionTemplateDecl>(d)) {
				for(auto specialization : ftd->specializations()) {
					if(!specialization->hasBody()) continue;
					methodDecls.push_back(llvm::dyn_cast<clang::CXXMethodDecl>(specialization));
				}
			}
		}

		// add static vars as globals - if not already done
		if(!converter.getRecordMan()->hasConvertedGlobals(clangRecTy->getDecl())) {
			for(auto decl : classDecl->decls()) {
				if(auto varDecl = llvm::dyn_cast<clang::VarDecl>(decl)) {
					if(varDecl->isStaticDataMember()) {
						converter.getDeclConverter()->VisitVarDecl(varDecl);
					}
				}
			}
			converter.getRecordMan()->markConvertedGlobals(clangRecTy->getDecl());
		}

		// add symbols for all methods to function manager before conversion
		for(auto mem : methodDecls) {
			if(mem->isStatic()) continue;
			converter.getDeclConverter()->convertMethodDecl(mem->getCanonicalDecl(), irParents, recordTy->getFields(), true);
		}

		// deal with static methods as functions (afterwards!)
		for(auto mem : methodDecls) {
			if(mem->isStatic()) {
				converter.getDeclConverter()->VisitFunctionDecl(mem->getCanonicalDecl());
			}
		}

		// decl only conversion end
		converter.getRecordMan()->decrementConversionStackDepth();

		// if we are converting only the decl, we are done at this point
		if(converter.getRecordMan()->isDeclOnlyConversion()) {
			return genTy;
		}

		// if we reached this point, the type will be finished for sure
		// mark finishing immediately to prevent infinite conversion recursion
		converter.getRecordMan()->markFinishing(clangRecTy->getDecl());

		// before method translation: push lambda mapping in case of lambda
		converter.getVarMan()->pushLambda(generateLambdaMapping(converter, classDecl));

		// get methods, constructors and destructor
		core::analysis::CppDefaultDeleteMembers recordMembers;
		core::PureVirtualMemberFunctionList pvMembers;
		bool destructorVirtual = false;
		for(auto mem : methodDecls) {
			mem = mem->getCanonicalDecl();
			if(mem->isStatic()) continue;
			auto ctorDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(mem);
			auto dtorDecl = llvm::dyn_cast<clang::CXXDestructorDecl>(mem);
			auto methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(mem);
			// if the method is implicit and one of the default constructs we skip it, because the C++ semantics too will add these again correctly
			if(mem->isImplicit()
					&& ((ctorDecl && (ctorDecl->isDefaultConstructor() || ctorDecl->isCopyOrMoveConstructor()))
							|| dtorDecl
							|| (methDecl && (methDecl->isCopyAssignmentOperator() || methDecl->isMoveAssignmentOperator())))) {
				continue;
			}
			// actual methods
			auto methodConversionResult = converter.getDeclConverter()->convertMethodDecl(mem, irParents, recordTy->getFields());
			if(mem->isVirtual() && mem->isPure()) {
				pvMembers.push_back(builder.pureVirtualMemberFunction(methodConversionResult.memberFunction->getName(), methodConversionResult.literal->getType().as<core::FunctionTypePtr>()));
			} else if(ctorDecl) {
				recordMembers.constructors.push_back(methodConversionResult);
			} else if(dtorDecl) {
				recordMembers.destructor = methodConversionResult;
				destructorVirtual = mem->isVirtual();
			} else {
				recordMembers.memberFunctions.push_back(methodConversionResult);
			}
		}

		// after method translation: generate invoke and pop lambda scope in case it was a lambda class
		generateLambdaInvokeOperator(methodDecls, classDecl, recordMembers.getMemberFunctionList(), converter);
		converter.getVarMan()->popLambda();

		// handle defaulted and deleted members accordingly
		recordMembers = core::analysis::applyCppDefaultDeleteSemantics(builder.refType(genTy), irParents, recordTy->getFields(), recordMembers);

		// register all lambdas in the TU
		auto registerInTu = [this](const core::analysis::MemberProperties& member) {
			VLOG(2) << "adding method lambda literal " << *member.literal << " of type " << dumpColor(member.literal->getType()) << " to IRTU";
			converter.getIRTranslationUnit().addFunction(member.literal, member.lambda);
		};
		::for_each(recordMembers.constructors, [&](const auto& ctor) { registerInTu(ctor); });
		if(recordMembers.destructor) registerInTu(*recordMembers.destructor);
		::for_each(recordMembers.memberFunctions, [&](const auto& mfun) { registerInTu(mfun); });

		// create new structTy/unionTy
		if(tagTy->isStruct()) {
			retTy = builder.structType(genTy->getName()->getValue(), parents, recordTy->getFields()->getFields(), recordMembers.getConstructorLiteralList(),
			                           recordMembers.getDestructorLiteral(), destructorVirtual,
			                           recordMembers.getMemberFunctionList(), pvMembers);
		} else {
			retTy = builder.unionType(genTy->getName()->getValue(), recordTy->getFields()->getFields(), recordMembers.getConstructorLiteralList(),
			                          recordMembers.getDestructorLiteral(), destructorVirtual,
			                          recordMembers.getMemberFunctionList(), pvMembers);
		}

		// add the type to the irTU
		converter.getIRTranslationUnit().replaceType(genTy, retTy.as<core::TagTypePtr>());

		// finally, we can mark the record conversion as done
		converter.getRecordMan()->markDone(clangRecTy->getDecl());

		return genTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 						DEPENDENT SIZED ARRAY TYPE
	// This type represents an array type in C++ whose size is a value-dependent
	// expression. For example:
	//
	//  template<typename T, int Size>
	//  class array {
	//     T data[Size];
	//  };
	//
	// For these types, we won't actually know what the array bound is until
	// template instantiation occurs, at which point this will become either
	// a ConstantArrayType or a VariableArrayType.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitDependentSizedArrayType(const DependentSizedArrayType* arrTy) {
		assert_fail() << "DependentSizedArrayType not yet handled!";
		return core::TypePtr();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						REFERENCE TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitReferenceType(const ReferenceType* refTy) {
		assert_fail() << "CPP Reference type should have been handled upstream already!";
		return core::TypePtr();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					TEMPLATE SPECIALIZATION TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitTemplateSpecializationType(const TemplateSpecializationType* templTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(templTy, retTy);

		VLOG(2) << "TemplateName: " << templTy->getTemplateName().getAsTemplateDecl()->getNameAsString();
		VLOG(2) << "numTemplateArg: " << templTy->getNumArgs();
		core::TypeList templateTypes;
		for(size_t argId = 0, end = templTy->getNumArgs(); argId < end; argId++) {
			switch(templTy->getArg(argId).getKind()) {
			case clang::TemplateArgument::Expression: {
				VLOG(2) << "arg: expression";
				templateTypes.push_back(converter.convertType(templTy->getArg(argId).getAsExpr()->getType()));
				break;
			}
			case clang::TemplateArgument::Type: {
				VLOG(2) << "arg: TYPE";
				templateTypes.push_back(converter.convertType(templTy->getArg(argId).getAsType()));
				break;
			}
			// -------------------   NON IMPLEMENTED ONES ------------------------
			case clang::TemplateArgument::Integral: {
				VLOG(2) << "arg: integral";
				assert_fail();
				break;
			}
			case clang::TemplateArgument::Null: {
				VLOG(2) << "arg: NULL";
				assert_fail();
				break;
			}
			case clang::TemplateArgument::Declaration: {
				VLOG(2) << "arg: DECL";
				assert_fail();
				break;
			}
			case clang::TemplateArgument::NullPtr: {
				VLOG(2) << "arg: nullptr";
				assert_fail();
				break;
			}
			case clang::TemplateArgument::Template: {
				VLOG(2) << "arg: template";
				assert_fail();
				break;
			}
			case clang::TemplateArgument::TemplateExpansion: {
				VLOG(2) << "arg: template expansion";
				assert_fail();
				break;
			}
			case clang::TemplateArgument::Pack: {
				VLOG(2) << "arg: pack";
				assert_fail();
				break;
			}
			}
		}

		if(templTy->isSugared()) {
			retTy = converter.convertType(templTy->desugar());
		} else {
			auto name = templTy->getTemplateName().getAsTemplateDecl()->getQualifiedNameAsString();
			if(boost::starts_with(name, "std::initializer_list")) {
				retTy = builder.typeVariable("_INSIEME_init_list_type_var");
				return retTy;
			}
			// intercepted template
			retTy = builder.genericType(insieme::utils::mangle(name), templateTypes);
		}
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					DEPENDENT TEMPLATE SPECIALIZATION TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitDependentTemplateSpecializationType(const DependentTemplateSpecializationType* tempTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(tempTy, retTy);

		assert_fail() << "DependentTemplateSpecializationType should not be translated, only a complete specialization can be turned into IR";
		return retTy;
	}

	core::TypePtr Converter::CXXTypeConverter::VisitDependentNameType(const clang::DependentNameType* depNameType) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(depNameType, retTy);

		assert_fail() << "DependentNameType should not be translated, only a complete specialization can be turned into IR";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					INJECTED CLASS NAME TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitInjectedClassNameType(const InjectedClassNameType* tempTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(tempTy, retTy);

		assert_fail() << "InjectedClassNameType not yet handled!";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					SUBSTITUTE TEMPLATE TYPE PARAMETER TYPE (TODO)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType* substTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(substTy, retTy);

		//		VLOG(2) << "resultType: " << funcTy->getReturnType().getTypePtr()->getTypeClassName();
		//		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
		//			[ this ] (const QualType& currArgType) {
		//				VLOG(2) << "argType: " << currArgType.getTypePtr()->getTypeClassName();
		//			}
		//		);

		//		VLOG(2) << "CLANG Type Classname: " << substTy->getReplacedParameter()->getTypeClassName();
		// VLOG(2) << "Replaced Template Name: " << substTy->getReplacedParameter()->getDecl()->getNameAsString();
		// VLOG(2) << "Replacement Type: " << substTy->getReplacementType().getTypePtr();

		// START_LOG_TYPE_CONVERSION(substTy);
		// assert_fail() << "SubstTemplateTypeParmType not yet handled!";
		retTy = converter.convertType(substTy->getReplacementType());
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					DEC
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitTemplateTypeParmType(const clang::TemplateTypeParmType* tempTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(tempTy, retTy);

		assert_fail() << "TemplateTypeParmType should not show off, can you explain to me how are you planing to handle this in IR?";
		return retTy;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                 MEMBER POINTER TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitMemberPointerType(const clang::MemberPointerType* memPointerTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(memPointerTy, retTy);
		retTy = convert(memPointerTy->getPointeeType());

		if(memPointerTy->isMemberFunctionPointer()) {
			auto memFunTy = retTy.as<core::FunctionTypePtr>();
			auto funProto = llvm::dyn_cast<clang::FunctionProtoType>(memPointerTy->getPointeeType());

			// prepend this obj to the param list
			auto thisTy = frontend::utils::getThisType(funProto, converter.convertType(clang::QualType(memPointerTy->getClass(), 0)));
			core::TypeList paramTypes = memFunTy->getParameterTypes();
			paramTypes.insert(paramTypes.begin(), thisTy);
			core::TypePtr returnTy = memFunTy->getReturnType();

			// generate new member function type (fun ptr/ref types are const by convention in INSPIRE)
			retTy = builder.ptrType(builder.functionType(paramTypes, returnTy, core::FK_MEMBER_FUNCTION), true);
		}

		return retTy;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                 DECLTYPE AND AUTO TYPE
	// For both of these, we depend on clang to correctly resolve them.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitDecltypeType(const clang::DecltypeType* declTy) {
		frontend_assert(!declTy->isUndeducedType()) << "Non-deduced decltype type unsupported.";
		return convert(declTy->getUnderlyingType());
	}
	core::TypePtr Converter::CXXTypeConverter::VisitAutoType(const clang::AutoType* autoTy) {
		if(!autoTy->isDeduced()) {
			return converter.getIRBuilder().genericType(utils::getDummyAutoDeducedTypeName());
		}
		return convert(autoTy->getDeducedType());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			The visitor itself
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::convertInternal(const clang::QualType& type) {

		if(type->isReferenceType()) {
			auto rType = type->getAs<clang::ReferenceType>();
			auto pointeeType = rType->getPointeeType();
			auto irInnerType = converter.convertType(pointeeType);
			auto refKind = llvm::isa<clang::RValueReferenceType>(rType) ? core::lang::ReferenceType::Kind::CppRValueReference
				                                                        : core::lang::ReferenceType::Kind::CppReference;
			return core::lang::buildRefType(irInnerType, pointeeType.isConstQualified(), pointeeType.isVolatileQualified(), refKind);
		}


		return TypeVisitor<CXXTypeConverter, core::TypePtr>::Visit(type.getTypePtr());
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
