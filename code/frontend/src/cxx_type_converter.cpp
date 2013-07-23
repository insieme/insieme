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


// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/DeclTemplate.h>


#include "insieme/frontend/utils/source_locations.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/c/naming.h"

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
	if (ptrTy->getPointeeType().isConstQualified() &&
		llvm::isa<clang::RecordType>(ptrTy->getPointeeType().getTypePtr())){
		convFact.warnings.insert("Constancy is lost in INSPIRE, pointers to a const object wont make use of const methods and operators");
	}
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
	if( !ty.isa<core::StructTypePtr>() ) { return ty; }

	core::StructTypePtr classType = ty.as<core::StructTypePtr>();

	// if is a c++ class, we need to annotate some stuff
	if (llvm::isa<clang::RecordType>(tagType)) {
		if (!llvm::isa<clang::CXXRecordDecl>(llvm::cast<clang::RecordType>(tagType)->getDecl()))
			return classType;

		const clang::CXXRecordDecl* classDecl = llvm::cast<clang::CXXRecordDecl>(tagType->getDecl());

		//~~~~~ base classes if any ~~~~~
		if (classDecl->getNumBases() > 0){
			std::vector<core::ParentPtr> parents;

			clang::CXXRecordDecl::base_class_const_iterator it = classDecl->bases_begin();
			for (; it != classDecl->bases_end(); it++){
				// visit the parent to build its type
				auto parentIrType = convert((it)->getType().getTypePtr());
				parents.push_back(builder.parent(it->isVirtual(), parentIrType));
			}

			// if we have base classes, update the classType
			assert(classType.isa<core::StructTypePtr>());

			// implant new parents list
			classType = core::transform::replaceNode(mgr, core::StructTypeAddress(classType)->getParents(), builder.parents(parents)).as<core::StructTypePtr>();
		}

		//update name of class type
		classType = core::transform::replaceNode(mgr, core::StructTypeAddress(classType)->getName(), builder.stringValue(classDecl->getNameAsString())).as<core::StructTypePtr>();

		//if classDecl has a name add it
		if( !classDecl->getNameAsString().empty() ) {
			annotations::c::attachCName(classType, classDecl->getNameAsString());
		}
	}

	return classType;
}

// Returns all bases of a c++ record declaration
vector<RecordDecl*> Converter::CXXTypeConverter::getAllBases(const clang::CXXRecordDecl* recDeclCXX ){
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


core::TypePtr Converter::CXXTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
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
core::TypePtr Converter::CXXTypeConverter::VisitDependentSizedArrayType(const DependentSizedArrayType* arrTy) {
	assert(false && "DependentSizedArrayType not yet handled!");
	return core::TypePtr();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						REFERENCE TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitReferenceType(const ReferenceType* refTy) {
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION(refTy, retTy);

	// get inner type
	core::TypePtr inTy = convFact.convertType( refTy->getPointeeType().getTypePtr());

	// find out if is a const ref or not
	QualType  qual;
	if(llvm::isa<clang::RValueReferenceType>(refTy))
		qual = llvm::cast<clang::RValueReferenceType>(refTy)->desugar();
	else{
		qual = llvm::cast<clang::LValueReferenceType>(refTy)->desugar();
	}

	// FIXME: find a better way... i got annoyed
	if (boost::starts_with (qual.getAsString (), "const"))
		retTy =  core::analysis::getConstCppRef(inTy);
	else
		retTy =  core::analysis::getCppRef(inTy);

	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitTemplateSpecializationType(const TemplateSpecializationType* templTy) {
	VLOG(2) << "TemplateName: " << templTy->getTemplateName().getAsTemplateDecl()->getNameAsString();
	VLOG(2) << "numTemplateArg: " << templTy->getNumArgs();
	for(size_t argId=0, end=templTy->getNumArgs(); argId < end; argId++) {
		assert(templTy->getArg(argId).getAsType().getTypePtr());
		VLOG(2) << "TemplateArguments: " << templTy->getArg(argId).getAsType().getTypePtr()->getTypeClassName();
	}
	VLOG(2) << "isSugared: " << templTy->isSugared();

	//START_LOG_TYPE_CONVERSION(templTy);
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( templTy, retTy );
	if(templTy->isSugared()) {
		//convert Template arguments (template < ActualClass >) -> ActualClass has to be converted
		for(TemplateSpecializationType::iterator ait=templTy->begin(), ait_end=templTy->end(); ait!=ait_end; ait++) {
			VLOG(2) << "Converting TemplateArg";
			convFact.convertType(ait->getAsType().getTypePtr());
		}

		retTy = convFact.convertType(templTy->desugar().getTypePtr());
	}
	//assert(false && "TemplateSpecializationType not yet handled!");
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitDependentTemplateSpecializationType(const DependentTemplateSpecializationType* tempTy) {
	core::TypePtr retTy;
    LOG_TYPE_CONVERSION( tempTy, retTy );

	assert(false && "DependentTemplateSpecializationType not yet handled!");
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitInjectedClassNameType(const InjectedClassNameType* tempTy) {
	core::TypePtr retTy;
    LOG_TYPE_CONVERSION( tempTy, retTy );

	assert(false && "InjectedClassNameType not yet handled!");
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					SUBSTITUTE TEMPLATE TYPE PARAMETER TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType* substTy) {
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( substTy, retTy );

//		VLOG(2) << "resultType: " << funcTy->getResultType().getTypePtr()->getTypeClassName();
//		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
//			[ this ] (const QualType& currArgType) {
//				VLOG(2) << "argType: " << currArgType.getTypePtr()->getTypeClassName();
//			}
//		);

//		VLOG(2) << "CLANG Type Classname: " << substTy->getReplacedParameter()->getTypeClassName();
	//VLOG(2) << "Replaced Template Name: " << substTy->getReplacedParameter()->getDecl()->getNameAsString();
	//VLOG(2) << "Replacement Type: " << substTy->getReplacementType().getTypePtr();

	//START_LOG_TYPE_CONVERSION(substTy);
	//assert(false && "SubstTemplateTypeParmType not yet handled!");
	retTy = convFact.convertType( substTy->getReplacementType().getTypePtr() );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitTemplateTypeParmType(const clang::TemplateTypeParmType* tempTy){
	core::TypePtr retTy;
    LOG_TYPE_CONVERSION( tempTy, retTy );

	assert(false && "TemplateTypeParmType not yet handled!");
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DECLTYPE TYPE (TODO) -- a CXX0x feature
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitDecltypeType(const clang::DecltypeType* declTy) {
	assert(false && "DeclType not supported");
	return core::TypePtr();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                 AUTO TYPE -- a CXX0x feature
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::CXXTypeConverter::VisitAutoType(const clang::AutoType* autoTy) {
    return convert(autoTy->getDeducedType().getTypePtr());
}

void Converter::CXXTypeConverter::postConvertionAction(const clang::Type* clangType, const core::TypePtr& irType) {

	// now attach meta-info (only for record type definitios)
	const clang::RecordType* recType = dyn_cast<const clang::RecordType>(clangType);
	if (!recType) return;	// nothing to do

	// skip if there is not declaration available
	if (!llvm::isa<clang::CXXRecordDecl>(recType->getDecl())) return;

	if( !irType.isa<core::StructTypePtr>() ) { return; }

	// get the generic type to substitute "this" parameter by the complete implementation
	// and make sure that the type we want to complete is "closed"
	// NOTE: irAlias might not exist for annonimous types
	core::TypePtr irAliasType    =	convFact.convertType(clangType);
	core::TypePtr irCompleteType =  convFact.lookupTypeDetails(irType);

	assert (!irCompleteType.isa<core::GenericTypePtr>());

	// assemble class info
	core::ClassMetaInfo classInfo;

	//~~~~~ look for the full decl ~~~~
	const clang::CXXRecordDecl* classDecl = llvm::cast<clang::CXXRecordDecl>(recType->getDecl());

	//~~~~~ copy ctor, move ctor, default ctor ~~~~~
	clang::CXXRecordDecl::ctor_iterator ctorIt = classDecl->ctor_begin();
	clang::CXXRecordDecl::ctor_iterator ctorEnd= classDecl->ctor_end();
	for (; ctorIt != ctorEnd; ctorIt ++){
		const CXXConstructorDecl* ctorDecl = *ctorIt;
		if (ctorDecl->isDefaultConstructor() ||
			ctorDecl->isCopyConstructor() ||
			ctorDecl->isMoveConstructor() ){

			if (ctorDecl->isUserProvided ()){

				// the function is a template spetialization, but if it has no body, we wont
				// convert it, it was never instanciated
				if (ctorDecl->getMemberSpecializationInfo () && !ctorDecl->hasBody()){
						continue;
				}

				core::ExpressionPtr&& ctorLambda = convFact.convertFunctionDecl(ctorDecl).as<core::ExpressionPtr>();
				if (ctorLambda ){
					assert(ctorLambda);
                    assert(!ctorLambda.isa<core::LiteralPtr>());
					if (irAliasType) ctorLambda = core::transform::replaceAllGen(mgr, ctorLambda, irAliasType, irCompleteType, true);
					classInfo.addConstructor(ctorLambda.as<core::LambdaExprPtr>());
				}
			}
		}
	}

	//~~~~~ convert destructor ~~~~~
	if(classDecl->hasUserDeclaredDestructor()){
		const clang::FunctionDecl* dtorDecl = llvm::cast<clang::FunctionDecl>(classDecl->getDestructor () );
		core::ExpressionPtr dtorLambda = convFact.convertFunctionDecl(dtorDecl).as<core::ExpressionPtr>();
		if(irAliasType) dtorLambda = core::transform::replaceAllGen(mgr, dtorLambda, irAliasType, irCompleteType, true);
		classInfo.setDestructor(dtorLambda.as<core::LambdaExprPtr>());
		if (llvm::cast<clang::CXXMethodDecl>(dtorDecl)->isVirtual())
			classInfo.setDestructorVirtual();
	}

	//~~~~~ member functions ~~~~~
	clang::CXXRecordDecl::method_iterator methodIt = classDecl->method_begin();
	clang::CXXRecordDecl::method_iterator methodEnd= classDecl->method_end();
	for (; methodIt != methodEnd; methodIt ++){
		if (llvm::isa<clang::CXXConstructorDecl>(*methodIt) ||
			llvm::isa<clang::CXXDestructorDecl>(*methodIt)){
			// ctor are handled in a previous loop
			continue;
		}

		if( (*methodIt)->isCopyAssignmentOperator() && !(*methodIt)->isUserProvided() ) {
			//FIXME: for now ignore CopyAssignmentOperator
			// -- backendCompiler should take care of it
			continue;
		}

		if( (*methodIt)->isMoveAssignmentOperator() && !(*methodIt)->isUserProvided() ) {
			//FIXME for non-userProvided moveAssign ops find solution,
			//maybe leave them to be handled by the backendCompiler or something else
			//currently are left over for be-compiler
			assert(!(*methodIt)->isMoveAssignmentOperator() && " move assigment operator is a CXX11 feature, not supported");
		}

		const clang::FunctionDecl* method = llvm::cast<clang::FunctionDecl>(*methodIt);

		// the function is a template espetialization, but if it has no body, we wont
		// convert it, it was never instanciated
		if (method->getMemberSpecializationInfo () && !method->hasBody()){
				continue;
		}

		auto methodLambda = convFact.convertFunctionDecl(method).as<core::ExpressionPtr>();
		if (irAliasType) methodLambda = core::transform::replaceAllGen(mgr, methodLambda, irAliasType, irCompleteType, true);

		if( method->isPure() ) {
			//pure virtual functions are handled bit different in metainfo
			VLOG(2) << "pure virtual function " << method;
			auto funcTy = methodLambda->getType().as<core::FunctionTypePtr>();
			VLOG(2) << funcTy;
			methodLambda = builder.getPureVirtual(funcTy);
		}

		if (VLOG_IS_ON(2)){
			VLOG(2) << " ############ member! #############";
			VLOG(2) << method->getNameAsString();
			VLOG(2) << methodLambda->getType();
			dumpDetail(methodLambda);
			VLOG(2) << " ###";
			method->dump();
			std::cout << std::endl;
			VLOG(2) << ((*methodIt)->isVirtual()? "virtual!":" ");
			VLOG(2) << ((*methodIt)->isConst()? "const!":" ");
			VLOG(2) << "           ############";
		}

		classInfo.addMemberFunction(method->getNameAsString(),
									methodLambda,
									(*methodIt)->isVirtual(),
									(*methodIt)->isConst());
	}

	// append meta information to the class definition
	core::setMetaInfo(irCompleteType, classInfo);
}

core::TypePtr Converter::CXXTypeConverter::convertInternal(const clang::Type* type) {
	assert(type && "Calling CXXTypeConverter::Visit with a NULL pointer");
	return TypeVisitor<CXXTypeConverter, core::TypePtr>::Visit(type);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
