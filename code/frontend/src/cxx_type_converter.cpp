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

#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/function_manager.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/annotations/naming.h"

#include <boost/algorithm/string/predicate.hpp>

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {
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
		if(auto clangRecTy = llvm::dyn_cast<RecordType>(tagType)) {
			if(converter.getRecordMan()->contains(clangRecTy->getDecl())) {
				return converter.getRecordMan()->lookup(clangRecTy->getDecl());
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

		// add static vars as globals
		for(auto decl : classDecl->decls()) {
			if(auto varDecl = llvm::dyn_cast<clang::VarDecl>(decl)) {
				if(varDecl->isStaticDataMember()) {
					converter.getDeclConverter()->VisitVarDecl(varDecl);
				}
			}
		}

		// get struct type for easier manipulation
		auto tagTy = retTy.as<core::TagTypePtr>();
		auto recordTy = tagTy->getRecord();

		// get parents if any
		std::vector<core::ParentPtr> parents;
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

		// add symbols for all methods to function manager before conversion
		for(auto mem : methodDecls) {
			mem = mem->getCanonicalDecl();
			if(mem->isStatic()) continue;
			auto convDecl = converter.getDeclConverter()->convertMethodDecl(mem, irParents, recordTy->getFields(), true);
		}

		// deal with static methods as functions (afterwards!)
		for(auto mem : methodDecls) {
			if(mem->isStatic()) {
				converter.getDeclConverter()->VisitFunctionDecl(mem);
				continue;
			}
		}

		// get methods, constructors and destructor
		std::vector<core::MemberFunctionPtr> members;
		std::vector<core::PureVirtualMemberFunctionPtr> pvMembers;
		std::vector<core::ExpressionPtr> constructors;
		core::LiteralPtr destructor = nullptr;
		bool destructorVirtual = false;
		for(auto mem : methodDecls) {
			mem = mem->getCanonicalDecl();
			// deal with static methods as functions
			if(mem->isStatic()) {
				converter.getDeclConverter()->VisitFunctionDecl(mem);
				continue;
			}
			// actual methods
			auto convDecl = converter.getDeclConverter()->convertMethodDecl(mem, irParents, recordTy->getFields());
			if(convDecl.lambda) {
				VLOG(2) << "adding method lambda literal " << *convDecl.lit << " of type " << dumpColor(convDecl.lit->getType()) << "to IRTU";
				converter.getIRTranslationUnit().addFunction(convDecl.lit, convDecl.lambda);
			}
			if(mem->isVirtual() && mem->isPure()) {
				pvMembers.push_back(builder.pureVirtualMemberFunction(convDecl.memFun->getName(), convDecl.lit->getType().as<core::FunctionTypePtr>()));
			} else if(llvm::dyn_cast<clang::CXXConstructorDecl>(mem)) {
				constructors.push_back(convDecl.lit.as<core::ExpressionPtr>());
			} else if(llvm::dyn_cast<clang::CXXDestructorDecl>(mem)) {
				destructor = convDecl.lit;
				destructorVirtual = mem->isVirtual();
			} else {
				members.push_back(convDecl.memFun);
			}
		}

		// create new structTy/unionTy
		if(tagTy->isStruct()) {
			retTy = builder.structTypeWithDefaults(builder.refType(genTy), irParents, recordTy->getFields(), builder.expressions(constructors), destructor,
				                                   destructorVirtual, builder.memberFunctions(members), builder.pureVirtualMemberFunctions(pvMembers));
		} else {
			retTy = builder.unionTypeWithDefaults(builder.refType(genTy), recordTy->getFields(), builder.expressions(constructors), destructor,
				                                  destructorVirtual, builder.memberFunctions(members), builder.pureVirtualMemberFunctions(pvMembers));
		}

		// update associated type in irTu
		converter.getIRTranslationUnit().insertRecordTypeWithDefaults(genTy, retTy.as<core::TagTypePtr>());

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
			// intercepted template
			retTy = builder.genericType(insieme::utils::mangle(templTy->getTemplateName().getAsTemplateDecl()->getNameAsString()), templateTypes);
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
		//core::TypePtr memTy = converter.lookupTypeDetails(retTy);
		//core::TypePtr classTy = convert(memPointerTy->getClass()->getCanonicalTypeInternal());

		//if(memPointerTy->isMemberFunctionPointer()) {
		//	frontend_assert(memTy.isa<core::FunctionTypePtr>()) << " no function type could be retrieved for pointed type\n";

		//	// prepend this obj to the param list
		//	core::TypeList paramTypes = memTy.as<core::FunctionTypePtr>()->getParameterTypes();
		//	paramTypes.insert(paramTypes.begin(), builder.refType(classTy));
		//	core::TypePtr returnTy = memTy.as<core::FunctionTypePtr>()->getReturnType();

		//	// generate new member function type
		//	return retTy = builder.functionType(paramTypes, returnTy, core::FK_MEMBER_FUNCTION);
		//} else {
		//	frontend_assert(memPointerTy->isMemberDataPointer());
		//	return retTy = core::analysis::getMemberPointer(classTy, memTy);
		//}

		assert_not_implemented();
		return retTy;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                 DECLTYPE AND AUTO TYPE
	// For both of these, we depend on clang to correctly resolve them.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::CXXTypeConverter::VisitDecltypeType(const clang::DecltypeType* declTy) {
		frontend_assert(!declTy->isUndeducedType()) << "Non-deduced decltype type unsupported.";
		return convertInternal(declTy->getUnderlyingType());
	}
	core::TypePtr Converter::CXXTypeConverter::VisitAutoType(const clang::AutoType* autoTy) {
		frontend_assert(autoTy->isDeduced()) << "Non-deduced auto type unsupported.";
		return convertInternal(autoTy->getDeducedType());
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
