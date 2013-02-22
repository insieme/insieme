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
	// this boost conversion but also avoids infinite recursion while resolving class member
	// function
	std::map<const TagType*, core::TypePtr>::iterator match = mClassTypeMap.find(tagType);
	if(match != mClassTypeMap.end()){
		return match->second;
	}

	auto classType = TypeConverter::VisitTagType(tagType);
	mClassTypeMap[tagType] = classType;

	// if is a c++ class, we need to annotate some stuff
	if (llvm::isa<clang::RecordType>(tagType)){
		if (!llvm::isa<clang::CXXRecordDecl>(llvm::cast<clang::RecordType>(tagType)->getDecl()))
			return classType;

		std::cout << "is a class" << std::endl;
		core::ClassMetaInfo classInfo;

		const clang::CXXRecordDecl* classDecl = llvm::cast<clang::CXXRecordDecl>(llvm::cast<clang::RecordType>(tagType)->getDecl());

		
		// copy ctor, move ctor, default ctor
		clang::CXXRecordDecl::ctor_iterator ctorIt = classDecl->ctor_begin();
		clang::CXXRecordDecl::ctor_iterator ctorEnd= classDecl->ctor_end();
		for (; ctorIt != ctorEnd; ctorIt ++){
			const CXXConstructorDecl* ctorDecl = *ctorIt;
			if (ctorDecl->isDefaultConstructor() ||
				ctorDecl->isCopyConstructor() ||
				ctorDecl->isMoveConstructor() ){

				core::LambdaExprPtr&& ctorLambda = convFact.convertCtor(ctorDecl, classType).as<core::LambdaExprPtr>();
				ctorLambda = convFact.memberize  (ctorDecl, ctorLambda, builder.refType(classType), core::FK_CONSTRUCTOR);
				classInfo.addConstructor(ctorLambda);
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

		// operator overloads
		//

		core::setMetaInfo(classType, classInfo);
	}

	mClassTypeMap.erase(tagType);
	mClassTypeMap[tagType] = classType;

	END_LOG_TYPE_CONVERSION(classType) ;
	return classType;

	//assert(false && "REWRITE, REMOVE THE C PART");

//	START_LOG_TYPE_CONVERSION( tagType );
//
//	if(!convFact.ctx.recVarMap.empty()) {
//		// check if this type has a typevar already associated, in such case return it
//		ConversionContext::TypeRecVarMap::const_iterator fit = convFact.ctx.recVarMap.find(tagType);
//		if( fit != convFact.ctx.recVarMap.end() ) {
//			// we are resolving a parent recursive type, so we shouldn't
//			return fit->second;
//		}
//	}
//
//	// check if the type is in the cache of already solved recursive types
//	// this is done only if we are not resolving a recursive sub type
//	if(!convFact.ctx.isRecSubType) {
//		ConversionContext::RecTypeMap::const_iterator rit = convFact.ctx.recTypeCache.find(tagType);
//		if(rit != convFact.ctx.recTypeCache.end())
//			return rit->second;
//	}
//
//	// will store the converted type
//	core::TypePtr retTy;
//	VLOG(1) << "~ Converting TagType: " << tagType->getDecl()->getName().str();
//
//	const TagDecl* tagDecl = tagType->getDecl()->getCanonicalDecl();
//	ConversionContext::ClassDeclMap::const_iterator cit = convFact.ctx.classDeclMap.find(tagDecl);
//	if(cit != convFact.ctx.classDeclMap.end()){
//		return cit->second;
//	}
//
//	// iterate through all the re-declarations to see if one of them provides a definition
//	TagDecl::redecl_iterator i,e = tagDecl->redecls_end();
//	for(i = tagDecl->redecls_begin(); i != e && !i->isCompleteDefinition(); ++i) ;
//	if(i != e) {
//		tagDecl = i->getDefinition();
//		// we found a definition for this declaration, use it
//		assert(tagDecl->isCompleteDefinition() && "TagType is not a definition");
//
//		if(tagDecl->getTagKind() == clang::TTK_Enum) {
//			// Enums are converted into integers
//			return convFact.builder.getLangBasic().getInt4();
//		} else {
//			// handle struct/union/class
//			const RecordDecl* recDecl = dyn_cast<const RecordDecl>(tagDecl);
//			assert(recDecl && "TagType decl is not of a RecordDecl type!");
//
//			if(!convFact.ctx.isRecSubType) {
//				// add this type to the type graph (if not present)
//				typeGraph.addNode(tagDecl->getTypeForDecl());
//			}
//
//			// retrieve the strongly connected componenets for this type
//			std::set<const Type*>&& components =
//				typeGraph.getStronglyConnectedComponents(tagDecl->getTypeForDecl());
//
//			if( !components.empty() ) {
//				if(VLOG_IS_ON(2)) {
//					// we are dealing with a recursive type
//					VLOG(2) << "Analyzing RecordDecl: " << recDecl->getNameAsString() << std::endl
//							<< "Number of components in the cycle: " << components.size();
//					std::for_each(components.begin(), components.end(),
//						[] (std::set<const Type*>::value_type c) {
//							assert(isa<const TagType>(c));
//							VLOG(2) << "\t" << dyn_cast<const TagType>(c)->getDecl()->getNameAsString();
//						}
//					);
//					typeGraph.print(std::cerr);
//				}
//
//				// we create a TypeVar for each type in the mutual dependence
//				convFact.ctx.recVarMap.insert(
//						std::make_pair(tagType, convFact.builder.typeVariable(recDecl->getName()))
//					);
//
//				// when a subtype is resolved we aspect to already have these variables in the map
//				if(!convFact.ctx.isRecSubType) {
//					std::for_each(components.begin(), components.end(),
//						[ this ] (std::set<const Type*>::value_type ty) {
//							const TagType* tagTy = dyn_cast<const TagType>(ty);
//							assert(tagTy && "Type is not of TagType type");
//
//							this->convFact.ctx.recVarMap.insert(
//									std::make_pair(ty, convFact.builder.typeVariable(tagTy->getDecl()->getName()))
//								);
//						}
//					);
//				}
//			}
//
//			// Visit the type of the fields recursively
//			// Note: if a field is referring one of the type in the cyclic dependency, a reference
//			//       to the TypeVar will be returned.
//			core::NamedCompositeType::Entries structElements;
//
//			// TODO
//			// c++ constructors
//			const CXXRecordDecl* recDeclCXX = dyn_cast<const CXXRecordDecl>(recDecl);
//			VLOG(2)<<recDeclCXX;
//
//			if(recDeclCXX){
//				bool hasPolymorphicBaseClass = false;
//				// add only direct baseclasses as member
//				for(CXXRecordDecl::base_class_const_iterator bit=recDeclCXX->bases_begin(),
//								bend=recDeclCXX->bases_end(); bit != bend; ++bit) {
//					const CXXBaseSpecifier * base = bit;
//					RecordDecl *baseRecord = base->getType()->getAs<RecordType>()->getDecl();
//
//					// put for every direct base-class a member to the derived class
//					core::TypePtr&& fieldType = Visit( const_cast<Type*>(baseRecord->getTypeForDecl()) );
//					VLOG(2) << "BaseClass is: " << baseRecord->getNameAsString() << " type: " << fieldType;
//					core::StringValuePtr id = convFact.builder.stringValue(baseRecord->getNameAsString());
//					structElements.push_back(convFact.builder.namedType(id, fieldType ));
//
//					hasPolymorphicBaseClass |= base->getType()->getAsCXXRecordDecl()->isPolymorphic();
//				}
//
////					for(CXXRecordDecl::ctor_iterator xit=recDeclCXX->ctor_begin(),
////							xend=recDeclCXX->ctor_end(); xit != xend; ++xit) {
////						CXXConstructorDecl * ctorDecl = *xit;
////						VLOG(1) << "~ Converting constructor: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;
////
////						core::TypePtr convertedType = convFact.convertType( GET_TYPE_PTR(ctorDecl) );
////						assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
////						core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);
////
////						//TODO funcType = addGlobalsToFunctionType(convFact.builder, convFact.ctx.globalStruct.first, funcType);
////
////						convFact.convertFunctionDecl(ctorDecl);
////						//std::cerr<<"dumpconstr: "<< curr->getNameAsString() << " ";
////						//curr->dumpDeclContext(); // on cerr
////						//std::cerr<<"enddumpconstr\n";
////						//core::StatementPtr&& body = convFact.convertStmt(curr->getBody());
////						//core::IdentifierPtr id = convFact.builder.identifier(curr->getNameAsString());
////					}
////
////					for(CXXRecordDecl::method_iterator mit=recDeclCXX->method_begin(),
////							mend=recDeclCXX->method_end(); mit != mend; ++mit) {
////						CXXMethodDecl * curr = *mit;
////						//convFact.convertFunctionDecl(curr, false);
////
////						//std::cerr<<"dumpconstr: "<< curr->getNameAsString() << " ";
////						//curr->dumpDeclContext(); // on cerr
////						//std::cerr<<"enddumpconstr\n";
////						//core::StatementPtr&& body = convFact.convertStmt(curr->getBody());
////						//core::IdentifierPtr id = convFact.builder.identifier(curr->getNameAsString());
////					}
//
//				// add __class member to support virtual functions at highest polymorphic baseclass
//				if( recDeclCXX->isPolymorphic() && !hasPolymorphicBaseClass) {
//					VLOG(2) << recDeclCXX->getName().data() << " polymorphic class";
//
//					core::StringValuePtr id = convFact.builder.stringValue("__class");
//					structElements.push_back(convFact.builder.namedType(id, convFact.builder.getLangBasic().getUInt4()));
//				}
//
//			}  // end if recDeclCXX
//
//			unsigned mid = 0;
//			for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
//				RecordDecl::field_iterator::value_type curr = *it;
//				core::TypePtr&& fieldType = Visit( const_cast<Type*>(GET_TYPE_PTR(curr)) );
//				// if the type is not const we have to add a ref because the value could be accessed and changed
//				//if(!(curr->getType().isConstQualified() || core::dynamic_pointer_cast<const core::VectorType>(fieldType)))
//				//	fieldType = convFact.builder.refType(fieldType);
//
//				core::StringValuePtr id = convFact.builder.stringValue(
//						curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));
//
//				structElements.push_back(convFact.builder.namedType(id, fieldType));
//				mid++;
//			}
//
//			// build a struct or union IR type
//			retTy = handleTagType(tagDecl, structElements);
//
//			if( !components.empty() ) {
//				// if we are visiting a nested recursive type it means someone else will take care
//				// of building the rectype node, we just return an intermediate type
//				if(convFact.ctx.isRecSubType)
//					return retTy;
//
//				// we have to create a recursive type
//				ConversionContext::TypeRecVarMap::const_iterator tit = convFact.ctx.recVarMap.find(tagType);
//				assert(tit != convFact.ctx.recVarMap.end() &&
//						"Recursive type has not TypeVar associated to himself");
//				core::TypeVariablePtr recTypeVar = tit->second;
//
//				vector<core::RecTypeBindingPtr> definitions;
//				definitions.push_back( convFact.builder.recTypeBinding(recTypeVar, handleTagType(tagDecl, structElements) ) );
//
//				// We start building the recursive type. In order to avoid loop the visitor
//				// we have to change its behaviour and let him returns temporarely types
//				// when a sub recursive type is visited.
//				convFact.ctx.isRecSubType = true;
//
//				std::for_each(components.begin(), components.end(),
//					[ this, &definitions, &recTypeVar ] (std::set<const Type*>::value_type ty) {
//						const TagType* tagTy = dyn_cast<const TagType>(ty);
//						assert(tagTy && "Type is not of TagType type");
//
//						//Visual Studio 2010 fix: full namespace
//						insieme::frontend::conversion::CXXConversionFactory::ConversionContext::TypeRecVarMap::const_iterator tit =
//								this->convFact.ctx.recVarMap.find(ty);
//
//						assert(tit != this->convFact.ctx.recVarMap.end() && "Recursive type has no TypeVar associated");
//						core::TypeVariablePtr var = tit->second;
//
//						// test whether this variable has already been handled
//						if (*var == *recTypeVar) { return; }
//
//						// we remove the variable from the list in order to fool the solver,
//						// in this way it will create a descriptor for this type (and he will not return the TypeVar
//						// associated with this recursive type). This behaviour is enabled only when the isRecSubType
//						// flag is true
//						this->convFact.ctx.recVarMap.erase(ty);
//
//						definitions.push_back( this->convFact.builder.recTypeBinding(var, this->Visit(const_cast<Type*>(ty))) );
//						var->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(tagTy->getDecl()->getNameAsString()) );
//
//						// reinsert the TypeVar in the map in order to solve the other recursive types
//						this->convFact.ctx.recVarMap.insert( std::make_pair(tagTy, var) );
//					}
//				);
//
//				// sort definitions - this will produce the same list of definitions for each of the related types => shared structure
//				if (definitions.size() > 1) {
//					std::sort(definitions.begin(), definitions.end(), [](const core::RecTypeBindingPtr& a, const core::RecTypeBindingPtr& b){
//						return a->getVariable()->getVarName()->getValue() < b->getVariable()->getVarName()->getValue();
//					});
//				}
//
//				// we reset the behavior of the solver
//				convFact.ctx.isRecSubType = false;
//				// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
//				convFact.ctx.recVarMap.clear();
//
//				core::RecTypeDefinitionPtr&& definition = convFact.builder.recTypeDefinition(definitions);
//				retTy = convFact.builder.recType(recTypeVar, definition);
//
//				// Once we solved this recursive type, we add to a cache of recursive types
//				// so next time we encounter it, we don't need to compute the graph
//				convFact.ctx.recTypeCache.insert(std::make_pair(tagType, retTy));
//			}
//
//			// Adding the name of the C struct as annotation
//			if (!recDecl->getName().empty())
//				retTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(recDecl->getName()) );
//			convFact.ctx.classDeclMap.insert(std::make_pair(tagDecl, retTy));
//		}
//	} else {
//		// We didn't find any definition for this type, so we use a name and define it as a generic type
//		retTy = convFact.builder.genericType( tagDecl->getNameAsString() );
//	}
//	END_LOG_TYPE_CONVERSION( retTy );
//	return retTy;
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

//TODO
core::FunctionTypePtr ConversionFactory::CXXTypeConverter::addCXXThisToFunctionType(const core::IRBuilder& builder,
											   const core::TypePtr& globals,
											   const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size()+1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin()+1);
	// function is receiving a reference to the global struct as the first argument
	argTypes[0] = builder.refType(globals);
	return builder.functionType( argTypes, funcType->getReturnType() );

}

core::TypePtr ConversionFactory::CXXTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return convFact.builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return convFact.builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
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
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						REFERENCE TYPE (FIXME)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::CXXTypeConverter ::VisitReferenceType(const ReferenceType* refTy) {
	return convFact.builder.refType( convFact.convertType( refTy->getPointeeType().getTypePtr()) );
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
	VLOG(2) << "CXX";
	return TypeVisitor<CXXTypeConverter, core::TypePtr>::Visit(type);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
