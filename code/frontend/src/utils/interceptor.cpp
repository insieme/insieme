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

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include "insieme/frontend/utils/interceptor.h"

#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>
#include <boost/regex.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/annotations/c/include.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/clang_config.h"
		
namespace insieme {
namespace frontend { 
namespace utils {

namespace {

namespace fs = boost::filesystem;

boost::optional<fs::path> toStdLibHeader(const fs::path& path) {
	static const fs::path stdLibRoot = fs::canonical(CXX_INCLUDES);
	static const boost::optional<fs::path> fail;

	if (path == stdLibRoot) {
		return fs::path();
	}

	if (!path.has_parent_path()) {
		return fail;
	}

	// if it is within the std-lib directory, build relative path
	auto res = toStdLibHeader(path.parent_path());
	return (res)? (*res/path.filename()) : fail;
	
}

void addHeaderForDecl(const core::NodePtr& node, const clang::Decl* decl, const Indexer& indexer) {

	// check whether there is a declaration at all
	if (!decl) return;

	VLOG(2) << "Searching header for: " << node << " of type " << node->getNodeType() << "\n";
	clang::SourceManager& sm = decl->getASTContext().getSourceManager();
	clang::SourceLocation includeLoc = sm.getIncludeLoc(sm.getFileID(decl->getLocation()));
	std::string fileName;
	if(includeLoc.isValid()) {
		// decl comes from included header
		fileName = sm.getPresumedLoc(includeLoc).getFilename();
	} else {
		assert(false && "no includeLoc found");
		return;
	}

	// get absolute path of header file
	fs::path header = fs::canonical(fileName);

	// check whether it is within the clang STL header library
	if (auto stdLibHeader = toStdLibHeader(header)) {
		header = *stdLibHeader;
	}

	// use resulting header
	insieme::annotations::c::attachInclude(node, header.string());
}

std::string fixQualifiedName(std::string name) {
	// get rid of inline namespace utilized by the clang headers
	boost::replace_all(name, "std::__1::", "std::");
	return name;
}

} //end anonymous namespace

void Interceptor::loadConfigFile(std::string fileName) {
	namespace fs = boost::filesystem;
	const fs::path configPath = fileName;
	if(	fs::exists(configPath) ) {
		fs::ifstream configFile(configPath);
		if(!configFile.is_open()) {
			LOG(WARNING) << "Interceptor couldn't open config file " << fileName;	
			return;
		}

		std::set<std::string> set;
		std::string nameToIntercept;
		while( getline(configFile, nameToIntercept) ) {
			set.insert(nameToIntercept);
		}
		configFile.close();

		// adapt configuration
		loadConfigSet(set);
	} else {
		LOG(WARNING) << "Interceptor didn't find config file " << fileName;	
		return;
	}
}

void Interceptor::loadConfigSet(std::set<std::string> tI) {
	toIntercept.insert(tI.begin(), tI.end());

	// update regular expression:

	//use one big regex for all strings to intercept
	rx = boost::regex("("+toString(join(")|(", toIntercept))+")");
}

insieme::core::TypePtr Interceptor::intercept(const clang::Type* type, insieme::frontend::conversion::ConversionFactory& convFact) {

	InterceptTypeVisitor iTV(convFact, indexer, rx);
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
	addHeaderForDecl(irType, typeDecl, indexer);
	VLOG(1) << "build interceptedType " << type << " ## " << irType;
	return irType;
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
		return regex_match(typeDecl->getQualifiedNameAsString(), rx);
	}
	
	return false;
}

bool Interceptor::isIntercepted(const clang::FunctionDecl* decl) const {
	if(toIntercept.empty()) { return false; }

	return regex_match(decl->getQualifiedNameAsString(), rx);
}

insieme::core::ExpressionPtr Interceptor::intercept(const clang::FunctionDecl* decl, insieme::frontend::conversion::ConversionFactory& convFact) {
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
	/*
	if( const clang::CXXConstructorDecl* ctorDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(decl)) {
		core::TypePtr thisTy = convFact.convertType(ctorDecl->getParent()->getTypeForDecl());
		core::TypeList paramTys = type->getParameterTypeList();
		paramTys.insert(paramTys.begin(), builder.refType(thisTy));
		
		//FIXME can we use memberize()?
		type = builder.functionType( paramTys, builder.refType(thisTy), core::FK_CONSTRUCTOR);
		
		// update literal name (only class name type)
		literalName = ctorDecl->getParent()->getQualifiedNameAsString();

	} else if(const clang::CXXMethodDecl* methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(decl) ) {
		core::TypePtr thisTy = convFact.convertType(methodDecl->getParent()->getTypeForDecl());
		core::TypeList paramTys = type->getParameterTypeList();
		paramTys.insert(paramTys.begin(), builder.refType(thisTy));

		//FIXME can we use memberize()?
		type = builder.functionType( paramTys, type.getReturnType(), core::FK_MEMBER_FUNCTION);

		// just use name of method as the resulting literal name
		literalName = methodDecl->getNameAsString();
	}
	*/

	if(const clang::CXXMethodDecl* methodDecl = llvm::dyn_cast<clang::CXXMethodDecl>(decl) ) {
		core::TypePtr thisTy = convFact.convertType(methodDecl->getParent()->getTypeForDecl());
		core::TypeList paramTys = type->getParameterTypeList();
		paramTys.insert(paramTys.begin(), builder.refType(thisTy));

		if( const clang::CXXConstructorDecl* ctorDecl = llvm::dyn_cast<clang::CXXConstructorDecl>(decl)) {
			//FIXME can we use memberize()?
			type = builder.functionType( paramTys, builder.refType(thisTy), core::FK_CONSTRUCTOR);
			
			// update literal name (only class name type)
			literalName = ctorDecl->getParent()->getQualifiedNameAsString();
		} else {
			//FIXME can we use memberize()?
			type = builder.functionType( paramTys, type.getReturnType(), core::FK_MEMBER_FUNCTION);

			// just use name of method as the resulting literal name
			literalName = methodDecl->getNameAsString();
		}
	}
	VLOG(2) << decl << " functionType " << type;

	// remove Clang inline namespace from header literal name (if present)
	literalName = fixQualifiedName(literalName);

	core::ExpressionPtr interceptExpr = builder.literal( literalName, type);

	addHeaderForDecl(interceptExpr, decl, indexer);

	VLOG(2) << interceptExpr << " " << type;
	if(insieme::annotations::c::hasIncludeAttached(interceptExpr)) {
		VLOG(2) << "\t attached header: " << insieme::annotations::c::getAttachedInclude(interceptExpr);
	}
	return interceptExpr;
}

InterceptTypeVisitor::InterceptTypeVisitor(insieme::frontend::conversion::ConversionFactory& convFact, const insieme::frontend::utils::Indexer& indexer, const boost::regex& rx)
		: convFact(convFact), builder(convFact.getIRBuilder()), indexer(indexer), rx(rx) {}

/*
core::TypePtr InterceptTypeVisitor::VisitTypedefType(const clang::TypedefType* typedefType) {
	std::string typeName = fixQualifiedName(typedefType->getDecl()->getQualifiedNameAsString());
	VLOG(2) << typeName;
	return builder.genericType(typeName, insieme::core::TypeList(), insieme::core::IntParamList());
}
*/
core::TypePtr InterceptTypeVisitor::VisitTagType(const clang::TagType* tagType) {
	const clang::TagDecl* tagDecl = tagType->getDecl();
	VLOG(2) << tagDecl;

	insieme::core::TypeList typeList; //empty typelist  = insieme::core::TypeList();
	if(llvm::isa<clang::ClassTemplateSpecializationDecl>(tagDecl)) {
		const clang::TemplateArgumentList& args= llvm::cast<clang::ClassTemplateSpecializationDecl>(tagDecl)->getTemplateArgs();
		for(size_t i = 0; i<args.size();i++) {
			switch(args[i].getKind()) {
				case clang::TemplateArgument::ArgKind::Null: VLOG(2) << "ArgKind::Null not supported"; break;
				case clang::TemplateArgument::ArgKind::Type:
					{
						const clang::Type* argType = args[i].getAsType().getTypePtr();
						typeList.insert( typeList.end(), convFact.convertType(argType) );
					}
					break;
				case clang::TemplateArgument::ArgKind::Declaration: VLOG(2) << "ArgKind::Declaration not supported"; break;
				case clang::TemplateArgument::ArgKind::NullPtr: VLOG(2) << "ArgKind::NullPtr not supported"; break;
				case clang::TemplateArgument::ArgKind::Integral: VLOG(2) << "ArgKind::Integral not supported"; break;
				case clang::TemplateArgument::ArgKind::Template: VLOG(2) << "ArgKind::Template not supported"; break;
				case clang::TemplateArgument::ArgKind::TemplateExpansion: VLOG(2) << "ArgKind::TemplateExpansion not supported"; break;
				case clang::TemplateArgument::ArgKind::Expression: VLOG(2) << "ArgKind::Expression not supported"; break;
				case clang::TemplateArgument::ArgKind::Pack: VLOG(2) << "ArgKind::Pack not supported"; break;
			}
		}
	}

	// obtain type name
	std::string typeName = fixQualifiedName(tagDecl->getQualifiedNameAsString());

	core::TypePtr retTy = builder.genericType(typeName, typeList, insieme::core::IntParamList());
	addHeaderForDecl(retTy, tagDecl, indexer);
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
			case clang::TemplateArgument::ArgKind::NullPtr:  VLOG(2) << "ArgKind::NullPtr not supported"; break;
			case clang::TemplateArgument::ArgKind::Integral: VLOG(2) << "ArgKind::Integral not supported"; break;
			case clang::TemplateArgument::ArgKind::Template: VLOG(2) << "ArgKind::Template not supported"; break;
			case clang::TemplateArgument::ArgKind::TemplateExpansion: VLOG(2) << "ArgKind::TemplateExpansion not supported"; break;
			case clang::TemplateArgument::ArgKind::Expression: VLOG(2) << "ArgKind::Expression not supported"; break;
			case clang::TemplateArgument::ArgKind::Pack: VLOG(2) << "ArgKind::Pack not supported"; break;
		}
	}

	const clang::TemplateDecl* templDecl = templTy->getTemplateName().getAsTemplateDecl();
	// compute resulting type name
	string typeName = fixQualifiedName(templDecl->getQualifiedNameAsString());

	// build resulting type
	core::TypePtr retTy = builder.genericType(typeName, typeList, insieme::core::IntParamList());
	addHeaderForDecl(retTy, templDecl, indexer);
	return retTy;
}

core::TypePtr InterceptTypeVisitor::VisitTemplateTypeParmType(const clang::TemplateTypeParmType* templParmType) {
	if( const clang::TemplateTypeParmDecl* tD = templParmType->getDecl() ) {
		string typeName = fixQualifiedName(tD->getNameAsString());
		VLOG(2) << typeName;
		core::TypePtr retTy = builder.genericType(typeName, insieme::core::TypeList(), insieme::core::IntParamList());
		addHeaderForDecl(retTy, tD, indexer);
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
		if (boost::regex_match(name, rx)) {
			addHeaderForDecl(res, type->getAsCXXRecordDecl(), indexer);
		}
	}

	return res;
}	

} // end utils namespace
} // end frontend namespace
} // end insieme namespace
