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

#include "insieme/frontend/utils/interceptor.h"

#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>
#include <boost/regex.hpp>
#include <boost/algorithm/string/replace.hpp>


#include "insieme/annotations/c/include.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/core/lang/enum_extension.h"
		
namespace insieme {
namespace frontend { 
namespace utils {

namespace {

namespace fs = boost::filesystem;


std::string fixQualifiedName(std::string name) {
	// get rid of inline namespace utilized by the clang headers
	boost::replace_all(name, "std::__1::", "std::");
	return name;
}

} //end anonymous namespace

/// Empties the toIntercept-set and fills it with the given set tI
void Interceptor::loadConfigSet(std::set<std::string> tI) {
	// clear the toIntercept-set of its default values
	// TODO: how to do it: we can use ONLY config file, or apend this to the base intercepted set (std, __gcc_cxx)
	toIntercept.clear();
	
	toIntercept.insert(tI.begin(), tI.end());

	// update regular expression:
	//use one big regex for all strings to intercept
	rx = boost::regex("("+toString(join(")|(", toIntercept))+")");
}

namespace {

	struct InterceptTypeVisitor : public clang::TypeVisitor<InterceptTypeVisitor, core::TypePtr> {

		insieme::frontend::conversion::Converter& convFact;
		const insieme::core::IRBuilder& builder;
		const Interceptor& interceptor;

		InterceptTypeVisitor(insieme::frontend::conversion::Converter& convFact, const Interceptor& interceptor);

		//core::TypePtr VisitTypedefType(const clang::TypedefType* typedefType);
		core::TypePtr VisitTagType(const clang::TagType* tagType);
		core::TypePtr VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy);
		core::TypePtr VisitTemplateTypeParmType(const clang::TemplateTypeParmType* templParmType);
		core::TypePtr Visit(const clang::Type* type);
	};

	InterceptTypeVisitor::InterceptTypeVisitor(insieme::frontend::conversion::Converter& convFact, const Interceptor& interceptor)
		: convFact(convFact), builder(convFact.getIRBuilder()), interceptor(interceptor) {}

	/*
	core::TypePtr InterceptTypeVisitor::VisitTypedefType(const clang::TypedefType* typedefType) {
		std::string typeName = fixQualifiedName(typedefType->getDecl()->getQualifiedNameAsString());
		VLOG(2) << typeName;
		return builder.genericType(typeName, insieme::core::TypeList(), insieme::core::IntParamList());
	}
	*/
	core::TypePtr InterceptTypeVisitor::VisitTagType(const clang::TagType* tagType) {
		const clang::TagDecl* tagDecl = tagType->getDecl();

		insieme::core::TypeList typeList; //empty typelist  = insieme::core::TypeList();
		if(llvm::isa<clang::ClassTemplateSpecializationDecl>(tagDecl)) {
			const clang::TemplateArgumentList& args= llvm::cast<clang::ClassTemplateSpecializationDecl>(tagDecl)->getTemplateArgs();
			for(size_t i = 0; i<args.size();i++) {
				switch(args[i].getKind()) {
					case clang::TemplateArgument::ArgKind::Null: VLOG(2) << "ArgKind::Null not supported"; break;
					case clang::TemplateArgument::ArgKind::Type:
						{
							const clang::Type* argType = args[i].getAsType().getTypePtr();
							auto ty =  convFact.convertType(argType);
							typeList.insert( typeList.end(), ty );
							//typeList.insert( typeList.end(), convFact.convertType(argType) );
						}
						break;
					case clang::TemplateArgument::ArgKind::Declaration: VLOG(2) << "ArgKind::Declaration not supported"; break;
					case clang::TemplateArgument::ArgKind::NullPtr: 	VLOG(2) << "ArgKind::NullPtr not supported"; break;
					case clang::TemplateArgument::ArgKind::Integral: 	
						{
							// the idea is to generate a generic type with a intParamList where we store
							// the value of the init expression
							//
							auto Ilist = insieme::core::IntParamList();
							Ilist.push_back( builder.concreteIntTypeParam(args[i].getAsIntegral().getLimitedValue()));
							typeList.insert(typeList.end(),builder.genericType("__insieme_IntTempParam", insieme::core::TypeList(), Ilist ));
							break;
						}
					case clang::TemplateArgument::ArgKind::Template: 	VLOG(2) << "ArgKind::Template not supported"; break;
					case clang::TemplateArgument::ArgKind::TemplateExpansion: VLOG(2) << "ArgKind::TemplateExpansion not supported"; break;
					case clang::TemplateArgument::ArgKind::Expression: 	VLOG(2) << "ArgKind::Expression not supported"; break;
					case clang::TemplateArgument::ArgKind::Pack: 		VLOG(2) << "ArgKind::Pack not supported"; break;
				}
			}
		}
		
		// obtain type name
		std::string typeName = fixQualifiedName(tagDecl->getQualifiedNameAsString());

		core::TypePtr retTy;
		if(tagDecl->getTagKind() == clang::TTK_Enum) {
			// for intercepted 3rdparty stuff we need to use the actual enum
			retTy = builder.getNodeManager().getLangExtension<core::lang::EnumExtension>().getEnumType(typeName);
		}
		else{
			retTy = builder.genericType(typeName, typeList, insieme::core::IntParamList());
		}

		addHeaderForDecl(retTy, tagDecl, interceptor.getHeaderTagger() , true);
		return retTy;
	}

	core::TypePtr InterceptTypeVisitor::VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy) {
		insieme::core::TypeList typeList;
		for(size_t argId=0, end=templTy->getNumArgs(); argId < end; argId++) {
			const clang::TemplateArgument arg = templTy->getArg(argId);

			switch(arg.getKind()) {
				case clang::TemplateArgument::ArgKind::Null: VLOG(2) << "ArgKind::Null not supported"; break;
				case clang::TemplateArgument::ArgKind::Type:
					{
						const clang::Type* argType = arg.getAsType().getTypePtr();
						core::TypePtr irArgType = convFact.convertType(argType);
						if(irArgType) {
							typeList.insert( typeList.end(), irArgType );
						} else {
							VLOG(2) << argType->getTypeClassName() << " type not supported";
							assert(false && "TemplateSpecializationType intercepted");
							typeList.insert( typeList.end(),
									builder.genericType("asdf", insieme::core::TypeList(), insieme::core::IntParamList()));
						}
					}
					break;
				case clang::TemplateArgument::ArgKind::Declaration: VLOG(2) << "ArgKind::Declaration not supported"; break;
				case clang::TemplateArgument::ArgKind::NullPtr:  	VLOG(2) << "ArgKind::NullPtr not supported"; break;
				case clang::TemplateArgument::ArgKind::Integral: 	VLOG(2) << "ArgKind::Integral not supported"; break;
				case clang::TemplateArgument::ArgKind::Template: 	VLOG(2) << "ArgKind::Template not supported"; break;
				case clang::TemplateArgument::ArgKind::TemplateExpansion: VLOG(2) << "ArgKind::TemplateExpansion not supported"; break;
				case clang::TemplateArgument::ArgKind::Expression: 	VLOG(2) << "ArgKind::Expression not supported"; break;
				case clang::TemplateArgument::ArgKind::Pack: 		VLOG(2) << "ArgKind::Pack not supported"; break;
			}
		}

		const clang::TemplateDecl* templDecl = templTy->getTemplateName().getAsTemplateDecl();
		// compute resulting type name
		string typeName = fixQualifiedName(templDecl->getQualifiedNameAsString());

		// build resulting type
		core::TypePtr retTy = builder.genericType(typeName, typeList, insieme::core::IntParamList());
		addHeaderForDecl(retTy, templDecl, interceptor.getHeaderTagger() , true);
		return retTy;
	}

	core::TypePtr InterceptTypeVisitor::VisitTemplateTypeParmType(const clang::TemplateTypeParmType* templParmType) {
		if( const clang::TemplateTypeParmDecl* tempTypeParamDecl = templParmType->getDecl() ) {
			string typeName = fixQualifiedName(tempTypeParamDecl->getNameAsString());
			VLOG(2) << typeName;
			core::TypePtr retTy = builder.genericType(typeName, insieme::core::TypeList(), insieme::core::IntParamList());
			addHeaderForDecl(retTy, tempTypeParamDecl, interceptor.getHeaderTagger() , true);
			return retTy;
		}
		assert(false && "TemplateTypeParmType intercepted");
		return builder.genericType("asdf", insieme::core::TypeList(), insieme::core::IntParamList());
	}

	core::TypePtr InterceptTypeVisitor::Visit(const clang::Type* type) {
	//	return TypeVisitor<InterceptTypeVisitor, core::TypePtr>::Visit(type);
		auto res = TypeVisitor<InterceptTypeVisitor, core::TypePtr>::Visit(type);

		// ensure include files are annotated
		if (res && res->getNodeType() == core::NT_GenericType) {
			const string& name = res.as<core::GenericTypePtr>()->getFamilyName();
			if (interceptor.isIntercepted(name)) {
				addHeaderForDecl(res, type->getAsCXXRecordDecl(), interceptor.getHeaderTagger() , true);
			}
		}

		return res;
	}
}


insieme::core::TypePtr Interceptor::intercept(const clang::Type* type, insieme::frontend::conversion::Converter& convFact) const{

	InterceptTypeVisitor iTV(convFact, *this);
	// resolve type and save in cache
	core::TypePtr irType = iTV.Visit(type);

	clang::TypeDecl* typeDecl = NULL;
	if( const clang::TagType* tagType = llvm::dyn_cast<clang::TagType>(type) ) {
		typeDecl = tagType->getDecl();
	} else if( const clang::TemplateTypeParmType* tempType = llvm::dyn_cast<clang::TemplateTypeParmType>(type) ){
		typeDecl = tempType->getDecl();
	} else if( const clang::TypedefType* typeDefType = llvm::dyn_cast<clang::TypedefType>(type) ) {
		typeDecl = typeDefType->getDecl();
		// typedef is sugar
		assert(false && "not used");
	}
	//we should only call intercept if type has a typeDecl
	assert(typeDecl && "Type has no TypeDecl");
	assert(irType && "irType");

	// add header file
	addHeaderForDecl(irType, typeDecl, getHeaderTagger() , true);
	VLOG(1) << "build interceptedType " << type << " ## " << irType;
	
	if(insieme::annotations::c::hasIncludeAttached(irType)) {
		VLOG(2) << "\t attached header: " << insieme::annotations::c::getAttachedInclude(irType);
	}

	return irType;
}

bool Interceptor::isIntercepted(const string& name) const {
	return regex_match(name, rx);
}

bool Interceptor::isIntercepted(const clang::Type* type) const {
	if(toIntercept.empty()) { return false; }

	//not every clang type has a Decl
	//cast Type and get decl
	clang::TypeDecl* typeDecl = NULL;
	if( const clang::TagType* tagType = llvm::dyn_cast<clang::TagType>(type) ) {
		typeDecl = tagType->getDecl();
	} else if( const clang::TemplateTypeParmType* tempType = llvm::dyn_cast<clang::TemplateTypeParmType>(type) ){
		typeDecl = tempType->getDecl();
	} else if( llvm::isa<clang::TypedefType>(type) ) {
		//we don't intercept typedef -> only sugar, we can use underlying type
		return false;
	}
	/*} else if( const clang::TypedefType* typeDefType = llvm::dyn_cast<clang::TypedefType>(type) ) {
		//typeDecl = typeDefType->getDecl();
	}
	*/

	if(typeDecl) {
		return isIntercepted(typeDecl->getQualifiedNameAsString());
	}

	return false;
}

bool Interceptor::isIntercepted(const clang::FunctionDecl* decl) const {
	if(toIntercept.empty()) {return false; }
	return regex_match(decl->getQualifiedNameAsString(), rx);
}

insieme::core::ExpressionPtr Interceptor::intercept(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& convFact) const{
	//FIXME create generic type for templates
	/* get template decl and convert its type -> add to converttype template handling...
		* if not specialized -> use typeVariable
		* */
	switch( decl->getTemplatedKind() ) {
		case clang::FunctionDecl::TemplatedKind::TK_NonTemplate:
			VLOG(2) << "TK_NonTemplate";
			break;
		case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplate:
			VLOG(2) << "TK_FunctionTemplate";
			break;
		case clang::FunctionDecl::TemplatedKind::TK_MemberSpecialization:
			VLOG(2) << "TK_MemberSpecialization";
			break;
		case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplateSpecialization:
			VLOG(2) << "TK_FunctionTemplateSpecialization";
			break;
		case clang::FunctionDecl::TemplatedKind::TK_DependentFunctionTemplateSpecialization:
			VLOG(2) << "TK_DependentFunctionTemplateSpecialization";
			break;
	}

	core::FunctionTypePtr type = convFact.convertType( decl->getType().getTypePtr() ).as<core::FunctionTypePtr>();

	//fix types for ctor, mfunc, ...
	std::string literalName = decl->getQualifiedNameAsString();

	// fix the type and literal name for Cxx members / ctors / dtors
	if(const clang::CXXMethodDecl* methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(decl) ) {
		if( const clang::CXXConstructorDecl* ctorDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(decl)) {
			literalName = ctorDecl->getParent()->getQualifiedNameAsString();
		} else {
			if (!methodDecl->isStatic())
				literalName = methodDecl->getNameAsString();
		}
		type = convFact.convertFunctionType(methodDecl);
	}

	literalName = fixQualifiedName(literalName);
	core::ExpressionPtr interceptExpr = builder.literal(literalName, type);
	addHeaderForDecl(interceptExpr, decl, getHeaderTagger(), true);

	VLOG(2) << interceptExpr << " " << interceptExpr->getType();

	if(insieme::annotations::c::hasIncludeAttached(interceptExpr)) {
		VLOG(2) << "\t attached header: " << insieme::annotations::c::getAttachedInclude(interceptExpr);
	}
	return interceptExpr;
}



} // end utils namespace
} // end frontend namespace
} // end insieme namespace
