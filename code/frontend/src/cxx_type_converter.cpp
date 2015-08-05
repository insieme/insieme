/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/lang/ir++_extension.h"

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

	// writte warnning on const pointers
	return TypeConverter::VisitPointerType(ptrTy);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitTagType(const TagType* tagType) {
	VLOG(2) << "VisitTagType " << tagType  <<  std::endl;
	
	core::TypePtr ty = TypeConverter::VisitTagType(tagType);
	LOG_TYPE_CONVERSION(tagType, ty) ;
	
	//if not a struct type we don't need to do anything more
	if(!ty.isa<core::StructTypePtr>()) {
		return ty;
	}
	
	core::StructTypePtr classType = ty.as<core::StructTypePtr>();
	
	// if is a c++ class, we need to annotate some stuff
	if(llvm::isa<clang::RecordType>(tagType)) {
		if(!llvm::isa<clang::CXXRecordDecl>(llvm::cast<clang::RecordType>(tagType)->getDecl())) {
			return classType;
		}
		
		const clang::CXXRecordDecl* classDecl = llvm::cast<clang::CXXRecordDecl>(tagType->getDecl());
		
		if(!classDecl->getDefinition()) {
			// check if the classDeclaration has a definition, if not we just use the the type
			// returned by the TypeConverter
			return classType;
		}
		
		//~~~~~ base classes if any ~~~~~
		if(classDecl->getNumBases() > 0) {
			std::vector<core::ParentPtr> parents;
			
			clang::CXXRecordDecl::base_class_const_iterator it = classDecl->bases_begin();
			for(; it != classDecl->bases_end(); it++) {
				// visit the parent to build its type
				auto parentIrType = convert((it)->getType());
				parents.push_back(builder.parent(it->isVirtual(), parentIrType));
			}
			
			// if we have base classes, update the classType
			assert(classType.isa<core::StructTypePtr>());
			
			// implant new parents list
			classType = core::transform::replaceNode(mgr, core::StructTypeAddress(classType)->getParents(), builder.parents(parents)).as<core::StructTypePtr>();
		}
		
//		//update name of class type
//		classType = core::transform::replaceNode(mgr,
//												 core::StructTypeAddress(classType)->getName(),
//												 builder.stringValue(classDecl->getNameAsString())).as<core::StructTypePtr>();

		//if classDecl has a name add it
		if(!classDecl->getNameAsString().empty()) {
			core::annotations::attachName(classType, classDecl->getNameAsString());
		}
	}
	return classType;
}

// Returns all bases of a c++ record declaration
vector<RecordDecl*> Converter::CXXTypeConverter::getAllBases(const clang::CXXRecordDecl* recDeclCXX) {
	vector<RecordDecl*> bases;
	
	for(CXXRecordDecl::base_class_const_iterator bit=recDeclCXX->bases_begin(),
	        bend=recDeclCXX->bases_end(); bit != bend; ++bit) {
		const CXXBaseSpecifier * base = bit;
		RecordDecl *baseRecord = base->getType()->getAs<RecordType>()->getDecl();
		bases.push_back(baseRecord);
		vector<RecordDecl*> subBases = getAllBases(dyn_cast<clang::CXXRecordDecl>(baseRecord));
		bases.insert(bases.end(), subBases.begin(), subBases.end());
	}
	return bases;
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
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION(refTy, retTy);
	
	// get inner type
	core::TypePtr inTy = convFact.convertType(refTy->getPointeeType()->getCanonicalTypeInternal());
	
	// find out if is a const ref or not
	QualType  qual;
	bool isConst;
	if(llvm::isa<clang::RValueReferenceType>(refTy)) {
		qual = llvm::cast<clang::RValueReferenceType>(refTy)->desugar();
		isConst = llvm::cast<clang::RValueReferenceType>(refTy)->getPointeeType().isConstQualified();
	}
	else {
		qual = llvm::cast<clang::LValueReferenceType>(refTy)->desugar();
		isConst = llvm::cast<clang::LValueReferenceType>(refTy)->getPointeeType().isConstQualified();
	}
	
	if(isConst) {
		retTy =  core::analysis::getConstCppRef(inTy);
	}
	else {
		retTy =  core::analysis::getCppRef(inTy);
	}
	
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TEMPLATE SPECIALIZATION TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitTemplateSpecializationType(const TemplateSpecializationType* templTy) {

	core::TypePtr retTy;
	LOG_TYPE_CONVERSION(templTy, retTy);
	
	VLOG(2) << "TemplateName: " << templTy->getTemplateName().getAsTemplateDecl()->getNameAsString();
	VLOG(2) << "numTemplateArg: " << templTy->getNumArgs();
	for(size_t argId=0, end=templTy->getNumArgs(); argId < end; argId++) {
		//we trigger the conversion of the inner type,
		//so we don't use the converted type/expr directly
		switch(templTy->getArg(argId).getKind()) {
		case clang::TemplateArgument::Expression: {
			VLOG(2) << "arg: expression";
			convFact.convertType(templTy->getArg(argId).getAsExpr()->getType());
			break;
		}
		case clang::TemplateArgument::Type: {
			VLOG(2) << "arg: TYPE";
			convFact.convertType(templTy->getArg(argId).getAsType());
			break;
		}
		// -------------------   NON IMPLEMENTED ONES ------------------------
		case clang::TemplateArgument::Integral:  {
			// templated parameters are values wich spetialize the template, because of their value nature,
			// they should be encapsulated as types to fit in the typing of the parent type
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
			// no need to do anything
			//templTy->getArg(argId).getAsTemplate().dump();
			
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
	
	assert_true(templTy->isSugared()) << "no idea what to do with non sugar";
	return	retTy = convFact.convertType(templTy->desugar());
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitDependentTemplateSpecializationType(const DependentTemplateSpecializationType* tempTy) {
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION(tempTy, retTy);
	
	assert_fail() << "DependentTemplateSpecializationType should not be translated, only a complete spetialization can be turn into IR";
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
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
	//VLOG(2) << "Replaced Template Name: " << substTy->getReplacedParameter()->getDecl()->getNameAsString();
	//VLOG(2) << "Replacement Type: " << substTy->getReplacementType().getTypePtr();
	
	//START_LOG_TYPE_CONVERSION(substTy);
	//assert_fail() << "SubstTemplateTypeParmType not yet handled!";
	retTy = convFact.convertType(substTy->getReplacementType());
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
	core::TypePtr memTy=  convFact.lookupTypeDetails(retTy);
	core::TypePtr classTy = convert(memPointerTy->getClass()->getCanonicalTypeInternal());
	
	if(memPointerTy->isMemberFunctionPointer()) {
		frontend_assert(memTy.isa<core::FunctionTypePtr>()) << " no function type could be retrieved for pointed type\n";
		
		// prepend this obj to the param list
		core::TypeList paramTypes = memTy.as<core::FunctionTypePtr>()->getParameterTypes();
		paramTypes.insert(paramTypes.begin(),builder.refType(classTy));
		core::TypePtr  returnTy      = memTy.as<core::FunctionTypePtr>()->getReturnType();
		
		// generate new member function type
		return retTy =  builder.functionType(paramTypes, returnTy, core::FK_MEMBER_FUNCTION);
	}
	else {
		frontend_assert(memPointerTy->isMemberDataPointer());
		return retTy = core::analysis::getMemberPointer(classTy, memTy);
	}
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//			The visitor itself
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::convertInternal(const clang::QualType& type) {
	return TypeVisitor<CXXTypeConverter, core::TypePtr>::Visit(type.getTypePtr());
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
