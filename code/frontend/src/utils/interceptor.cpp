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
#include <sstream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>
#include <boost/regex.hpp>
#include <boost/algorithm/string/replace.hpp>


#include "insieme/annotations/c/include.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/core/lang/enum_extension.h"
		
namespace insieme {
namespace frontend { 
namespace utils {

namespace {

std::string fixQualifiedName(std::string name) {
	// get rid of inline namespace utilized by the clang headers
	boost::replace_all(name, "std::__1::", "std::");
	return name;
}

} //end anonymous namespace

insieme::core::TypePtr Interceptor::intercept(const clang::Type* type, insieme::frontend::conversion::Converter& convFact) const{
	auto builder = convFact.getIRBuilder();
	core::TypePtr irType;

	if( const clang::TagType* tagType = llvm::dyn_cast<clang::TagType>(type) ) {
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
						}
						break;
					case clang::TemplateArgument::ArgKind::Template: 	VLOG(2) << "ArgKind::Template not supported"; break;
					case clang::TemplateArgument::ArgKind::TemplateExpansion: VLOG(2) << "ArgKind::TemplateExpansion not supported"; break;
					case clang::TemplateArgument::ArgKind::Expression: 	VLOG(2) << "ArgKind::Expression not supported"; break;
					case clang::TemplateArgument::ArgKind::Pack:
						{
							VLOG(2) << "template pack ";
							for(clang::TemplateArgument::pack_iterator it = args[i].pack_begin(), end = args[i].pack_end();it!=end;it++) {
								const clang::Type* argType = (*it).getAsType().getTypePtr();
								auto ty =  convFact.convertType(argType);
								VLOG(2) << ty;
								typeList.insert( typeList.end(), ty );
							}
							//VLOG(2) << "ArgKind::Pack not supported";
						}
						break;
				}
			}
		}
		
		// obtain type name
		std::string typeName = fixQualifiedName(tagDecl->getQualifiedNameAsString());

		if(tagDecl->getTagKind() == clang::TTK_Enum) {
			core::GenericTypePtr gt = builder.genericType(typeName);
		
			//tag the genericType used inside the enum to pass this to the backend
			convFact.getHeaderTagger().addHeaderForDecl(gt, tagDecl, true);

			// for intercepted 3rdparty stuff we need to use the actual enum
			irType = builder.getNodeManager().getLangExtension<core::lang::EnumExtension>().getEnumType(gt);
			//return irType;
		}
		else{
			irType = builder.genericType(typeName, typeList, insieme::core::IntParamList());
		}

		// add header file
		convFact.getHeaderTagger().addHeaderForDecl(irType, tagDecl, true);

	} else if( llvm::isa<clang::TypedefType>(type) ) {
		// don't intercept typedefs -> only sugar, we can use underlying type
		assert(false && "typedef is sugar -- use underlying type");
	}

	//we should only call intercept if type has a typeDecl
	assert(irType && "irType");
		
	VLOG(1) << "build interceptedType " << irType;
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
	} else if( llvm::isa<clang::TypedefType>(type) ) {
		// don't intercept typedefs -> only sugar, we can use underlying type
		return false;
	}

	if(typeDecl) {
		return isIntercepted(typeDecl->getQualifiedNameAsString());
	}

	return false;
}

bool Interceptor::isIntercepted(const clang::FunctionDecl* decl) const {
	if(toIntercept.empty()) {return false; }
	return regex_match(decl->getQualifiedNameAsString(), rx);
}

insieme::core::ExpressionPtr Interceptor::intercept(const clang::FunctionDecl* decl, insieme::frontend::conversion::Converter& convFact, const bool explicitTemplateArgs) const {
	
	auto builder = convFact.getIRBuilder();
	std::stringstream ss;
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
			{
				VLOG(2) << "TK_FunctionTemplateSpecialization";
				std::vector<string> templateParams;

				clang::FunctionTemplateSpecializationInfo* funcTempSpecInfo= decl->getTemplateSpecializationInfo();
				auto getTemplateArgs = [&](const clang::TemplateArgumentList* args) {
				for(unsigned i=0; i<args->size(); ++i){
				//for(unsigned i=0; i<decl->getTemplateSpecializationArgs()->size(); ++i){
					const clang::TemplateArgument& arg = args->get(i);
					//only add something if we have a type
					switch(arg.getKind()) {
						case clang::TemplateArgument::Expression: {
							VLOG(2) << arg.getAsExpr()->getType().getAsString();
							//templateParams.push_back(arg.getAsExpr()->getType().getAsString());
							break;
						}
						case clang::TemplateArgument::Type: {
							std::string typeName = arg.getAsType().getAsString();
							VLOG(2) << typeName;
							//type has struct/class in name remove it
							boost::replace_all(typeName, "class ", ""); 
							boost::replace_all(typeName, "struct ", ""); 
							VLOG(2) << typeName;

							templateParams.push_back(typeName);
							break;
						}
						case clang::TemplateArgument::Null: {
							VLOG(2) << "null";
							//templateParams.push_back("null");
							break;
						}
						case clang::TemplateArgument::Declaration: {
							VLOG(2) << arg.getAsDecl()->getType().getAsString();
							//templateParams.push_back(arg.getAsDecl()->getType().getAsString());
							break;
						}
						case clang::TemplateArgument::NullPtr: {
							VLOG(2) << "nullptr";
							//templateParams.push_back("nullptr");
							break;
						}
						case clang::TemplateArgument::Integral:  {
							VLOG(2) << arg.getAsIntegral().toString(10);
							templateParams.push_back(arg.getAsIntegral().toString(10));
							break;
						}
						case clang::TemplateArgument::Template: {
							VLOG(2) << arg.getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString();
							//templateParams.push_back(arg.getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString());
						}
						case clang::TemplateArgument::TemplateExpansion: {
							VLOG(2) << "TemplateExpansion";
							//I don't know what to do here
							break;
						}
						case clang::TemplateArgument::Pack: {
							for(clang::TemplateArgument::pack_iterator it = arg.pack_begin(), end = arg.pack_end();it!=end;it++) {
								const clang::QualType& argType = (*it).getAsType();
								VLOG(2) << argType.getAsString();
								//templateParams.push_back(argType.getAsString());
							}
							break;
						}
						default:
							assert(false);
					}
				}
				};

				if(explicitTemplateArgs) {
					//append template args explicitly
					getTemplateArgs(funcTempSpecInfo->TemplateArguments);
				}
				if(!templateParams.empty()) {
					ss << " < " << join(", ",templateParams) <<  " > ";
					VLOG(2) << ss.str();
				}
			}
			break;
		case clang::FunctionDecl::TemplatedKind::TK_DependentFunctionTemplateSpecialization:
			VLOG(2) << "TK_DependentFunctionTemplateSpecialization";
			break;
		default:
			assert(false);
	}


	core::FunctionTypePtr type = convFact.convertType( decl->getType().getTypePtr() ).as<core::FunctionTypePtr>();

	//fix types for ctor, mfunc, ...
	std::string literalName = decl->getQualifiedNameAsString();

	//append eventual templateSpecializations
	literalName.append(ss.str());

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
	convFact.getHeaderTagger().addHeaderForDecl(interceptExpr, decl, true);

	VLOG(2) << interceptExpr << " " << interceptExpr->getType();

	if(insieme::annotations::c::hasIncludeAttached(interceptExpr)) {
		VLOG(2) << "\t attached header: " << insieme::annotations::c::getAttachedInclude(interceptExpr);
	}
	return interceptExpr;
}

	
insieme::core::ExpressionPtr Interceptor::intercept(const clang::EnumConstantDecl* enumConstant, insieme::frontend::conversion::Converter& convFact) const {
	const clang::EnumType* enumType = llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(enumConstant->getDeclContext())->getTypeForDecl());
	auto enumDecl = enumType->getDecl();

	std::string qualifiedTypeName = enumDecl->getQualifiedNameAsString();
	std::string typeName = enumDecl->getNameAsString();
	std::string constantName = enumConstant->getNameAsString();

	//remove typeName from qualifiedTypeName and append enumConstantName
	size_t pos = qualifiedTypeName.find(typeName);
	assert(pos!= std::string::npos);
	std::string fixedQualifiedName = qualifiedTypeName.replace(pos,typeName.size(), constantName);

	VLOG(2) << qualifiedTypeName << " " << typeName << " " << constantName;
	VLOG(2) << fixedQualifiedName;

	std::string enumConstantName = fixedQualifiedName;
	core::TypePtr enumTy = convFact.convertType(enumType);
	return convFact.getIRBuilder().literal(enumConstantName, enumTy);
}


} // end utils namespace
} // end frontend namespace
} // end insieme namespace
