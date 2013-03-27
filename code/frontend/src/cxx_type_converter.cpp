/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_class_info.h"

#include "insieme/annotations/c/naming.h"

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/DeclTemplate.h>

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
core::TypePtr ConversionFactory::CXXTypeConverter::VisitBuiltinType(const BuiltinType* buldInTy) {
	return TypeConverter::VisitBuiltinType(buldInTy);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter::VisitTagType(const TagType* tagType) {
	VLOG(2) << "VisitTagType " << tagType  <<  std::endl;

	// check if this type has being already translated.
	// this boost conversion but also avoids infinite recursion while resolving class member function
	auto match = convFact.ctx.typeCache.find(tagType);
	if(match != convFact.ctx.typeCache.end()){
		return match->second;
	}

	auto classType = TypeConverter::VisitTagType(tagType);

	convFact.ctx.typeCache[tagType] = classType;
	// if is a c++ class, we need to annotate some stuff
	if (llvm::isa<clang::RecordType>(tagType)){
		if (!llvm::isa<clang::CXXRecordDecl>(llvm::cast<clang::RecordType>(tagType)->getDecl()))
			return classType;

		core::ClassMetaInfo classInfo;
		const clang::CXXRecordDecl* classDecl = llvm::cast<clang::CXXRecordDecl>(llvm::cast<clang::RecordType>(tagType)->getDecl());

		// base clases if any:
		if (classDecl->getNumBases() > 0){
			std::vector<core::ParentPtr> parents;

			clang::CXXRecordDecl::base_class_const_iterator it = classDecl->bases_begin();
			for (; it != classDecl->bases_end(); it++){
				// visit the parent to build its type
				auto parentIrType = Visit((it)->getType().getTypePtr());
				parents.push_back(builder.parent(it->isVirtual(), parentIrType));
			}

			// if we have base classes, we need to create again the IR type, with the 
			// parent list this time
			//FIXME: typename
			classType = builder.structType(parents, classType.as<core::StructTypePtr>()->getElements());
		}

		// update cache with base classes, for upcomming uses
		convFact.ctx.typeCache.erase(tagType);
		convFact.ctx.typeCache[tagType] = classType;

		// copy ctor, move ctor, default ctor
		clang::CXXRecordDecl::ctor_iterator ctorIt = classDecl->ctor_begin();
		clang::CXXRecordDecl::ctor_iterator ctorEnd= classDecl->ctor_end();
		for (; ctorIt != ctorEnd; ctorIt ++){
			const CXXConstructorDecl* ctorDecl = *ctorIt;

			if (ctorDecl->isDefaultConstructor() ||
				ctorDecl->isCopyConstructor() ||
				ctorDecl->isMoveConstructor() ){
				
				if (ctorDecl->isUserProvided ()){
					/*
					core::ExpressionPtr&& ctorLambda = convFact.convertCtor(ctorDecl, classType).as<core::ExpressionPtr>();
					if (ctorLambda && ctorLambda.isa<core::LambdaExprPtr>()){
						ctorLambda = convFact.memberize  (ctorDecl, ctorLambda, builder.refType(classType), core::FK_CONSTRUCTOR);
						classInfo.addConstructor(ctorLambda.as<core::LambdaExprPtr>());
					}
					*/

					core::LambdaExprPtr&& ctorLambda = convFact.convertFunctionDecl(ctorDecl).as<core::LambdaExprPtr>();
					if (ctorLambda ){
						ctorLambda = convFact.memberize  (ctorDecl, ctorLambda, builder.refType(classType), core::FK_CONSTRUCTOR);
						classInfo.addConstructor(ctorLambda);
					}
				}
			}
		} 

		// convert destructor
		if(classDecl->hasUserDeclaredDestructor()){
			const clang::FunctionDecl* dtorDecl = llvm::cast<clang::FunctionDecl>(classDecl->getDestructor () );
			core::LambdaExprPtr&& dtorLambda = convFact.convertFunctionDecl(dtorDecl).as<core::LambdaExprPtr>();
			dtorLambda = convFact.memberize  (dtorDecl, dtorLambda, builder.refType(classType), core::FK_DESTRUCTOR);
			classInfo.setDestructor(dtorLambda);
			if (llvm::cast<clang::CXXMethodDecl>(dtorDecl)->isVirtual())
				classInfo.setDestructorVirtual();
		}

		// member functions
		clang::CXXRecordDecl::method_iterator methodIt = classDecl->method_begin();
		clang::CXXRecordDecl::method_iterator methodEnd= classDecl->method_end();
		for (; methodIt != methodEnd; methodIt ++){
			if (llvm::isa<clang::CXXConstructorDecl>(*methodIt) ||
				llvm::isa<clang::CXXDestructorDecl>(*methodIt)){
				//FIXME: here might be a problem
				continue;
			}

			const clang::FunctionDecl* method = llvm::cast<clang::FunctionDecl>(*methodIt);

			// FIXME: we should not have to look for the F$%ing TU everyplace, this should be
			// responsability of the convert func function
			convFact.getTranslationUnitForDefinition(method);  // FIXME:: remove this crap
			
			core::LambdaExprPtr&& methodLambda = convFact.convertFunctionDecl(method).as<core::LambdaExprPtr>();
			methodLambda = convFact.memberize  (method, methodLambda, builder.refType(classType), core::FK_MEMBER_FUNCTION);
			/*
			core::ExpressionPtr&& methodLambda = convFact.convertFunctionDecl(method).as<core::ExpressionPtr>();

			if(methodLambda.isa<core::LambdaExprPtr>()) {
				methodLambda = convFact.memberize  (method, methodLambda, builder.refType(classType), core::FK_MEMBER_FUNCTION);
			}
			*/

			if (VLOG_IS_ON(2)){
				VLOG(2) << " ############ member! #############";
				VLOG(2)<< llvm::cast<clang::NamedDecl>(method)->getNameAsString();
				dumpDetail(methodLambda);
				VLOG(2) << "###";
				method->dump();
				VLOG(2) << ( (*methodIt)->isVirtual()? "virtual!":" ");
				VLOG(2) << ((*methodIt)->isConst()? "const!":" ");
				VLOG(2) << "           ############";
			}

			classInfo.addMemberFunction(llvm::cast<clang::NamedDecl>(method)->getNameAsString(), 
										methodLambda,
										(*methodIt)->isVirtual(), 
										(*methodIt)->isConst());
			/*
			if(methodLambda.isa<core::LambdaExprPtr>()) {
				classInfo.addMemberFunction(llvm::cast<clang::NamedDecl>(method)->getNameAsString(), 
										methodLambda.as<core::LambdaExprPtr>(),
										(*methodIt)->isVirtual(), 
										(*methodIt)->isConst());
			}
			*/
		}
		
		// append metha information to the class definition
		core::setMetaInfo(classType, classInfo);

	}
	
	// cache the new implementation
	convFact.ctx.typeCache.erase(tagType);
	convFact.ctx.typeCache[tagType] = classType;

	END_LOG_TYPE_CONVERSION(classType) ;
	return classType;
}

// Returns all bases of a c++ record declaration
vector<RecordDecl*> ConversionFactory::CXXTypeConverter::getAllBases(const clang::CXXRecordDecl* recDeclCXX ){
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


core::TypePtr ConversionFactory::CXXTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return convFact.builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return convFact.builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
	return core::TypePtr();
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
core::TypePtr ConversionFactory::CXXTypeConverter ::VisitDependentSizedArrayType(const DependentSizedArrayType* arrTy) {
	assert(false && "DependentSizedArrayType not yet handled!");
	return core::TypePtr();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						REFERENCE TYPE 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter::VisitReferenceType(const ReferenceType* refTy) {
	START_LOG_TYPE_CONVERSION(refTy);
	core::TypePtr retTy;

// this is a cpp reference not pointer 
	core::TypePtr inTy = convFact.convertType( refTy->getPointeeType().getTypePtr());

// we need to check where is a const ref or not	
	QualType  qual;
	if(llvm::isa<clang::RValueReferenceType>(refTy))
		//assert(false && "right side value ref not supported");
		qual = llvm::cast<clang::RValueReferenceType>(refTy)->desugar();
	else{
		qual = llvm::cast<clang::LValueReferenceType>(refTy)->desugar();
	}
	// FIXME: find a better way... i got annoyed
	if (boost::starts_with (qual.getAsString (), "const"))
		retTy =  core::analysis::getConstCppRef(inTy);
	else
		retTy =  core::analysis::getCppRef(inTy);
	
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter ::VisitTemplateSpecializationType(const TemplateSpecializationType* templTy) {
	VLOG(2) << "TemplateName: " << templTy->getTemplateName().getAsTemplateDecl()->getNameAsString();
	VLOG(2) << "numTemplateArg: " << templTy->getNumArgs();
	for(size_t argId=0, end=templTy->getNumArgs(); argId < end; argId++) {
		assert(templTy->getArg(argId).getAsType().getTypePtr());
		VLOG(2) << "TemplateArguments: " << templTy->getArg(argId).getAsType().getTypePtr()->getTypeClassName();
	}
	VLOG(2) << "isSugared: " << templTy->isSugared();

	START_LOG_TYPE_CONVERSION(templTy);
	core::TypePtr retTy;
	if(templTy->isSugared()) {
		//convert Template arguments (template < ActualClass >) -> ActualClass has to be converted
		for(TemplateSpecializationType::iterator ait=templTy->begin(), ait_end=templTy->end(); ait!=ait_end; ait++) {
			VLOG(2) << "Converting TemplateArg";
			convFact.convertType(ait->getAsType().getTypePtr());
		}

		retTy = convFact.convertType(templTy->desugar().getTypePtr());
	}
	//assert(false && "TemplateSpecializationType not yet handled!");
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter ::VisitDependentTemplateSpecializationType(const DependentTemplateSpecializationType* tempTy) {
	core::TypePtr retTy;

	START_LOG_TYPE_CONVERSION(tempTy);
	assert(false && "DependentTemplateSpecializationType not yet handled!");
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter ::VisitInjectedClassNameType(const InjectedClassNameType* tempTy) {
	core::TypePtr retTy;

	START_LOG_TYPE_CONVERSION(tempTy);
	assert(false && "InjectedClassNameType not yet handled!");
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					SUBSTITUTE TEMPLATE TYPE PARAMETER TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter ::VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType* substTy) {
	core::TypePtr retTy;

//		VLOG(2) << "resultType: " << funcTy->getResultType().getTypePtr()->getTypeClassName();
//		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
//			[ this ] (const QualType& currArgType) {
//				VLOG(2) << "argType: " << currArgType.getTypePtr()->getTypeClassName();
//			}
//		);

//		VLOG(2) << "CLANG Type Classname: " << substTy->getReplacedParameter()->getTypeClassName();
	//TODO SHOULD WORK IN NEWER CLANG VERSION???
	//VLOG(2) << "Replaced Template Name: " << substTy->getReplacedParameter()->getDecl()->getNameAsString();
	//VLOG(2) << "Replacement Type: " << substTy->getReplacementType().getTypePtr();

	START_LOG_TYPE_CONVERSION(substTy);
	//assert(false && "SubstTemplateTypeParmType not yet handled!");
	retTy = convFact.convertType( substTy->getReplacementType().getTypePtr() );

	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

core::TypePtr ConversionFactory::CXXTypeConverter::Visit(const clang::Type* type) {
	assert(type && "Calling CXXTypeConverter::Visit with a NULL pointer");
	//check cache for type
	auto fit = convFact.ctx.typeCache.find(type);
	if(fit != convFact.ctx.typeCache.end()) {
		return fit->second;
	}
	return TypeVisitor<CXXTypeConverter, core::TypePtr>::Visit(type);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
