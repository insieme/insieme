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

	/*
	clang::SourceManager& sm = decl->getASTContext().getSourceManager();
	clang::SourceLocation loc = sm.getSpellingLoc(decl->getLocation());
	if(!sm.getFilename(loc).empty()) {
		return sm.getFilename(loc).data();
	}
	*/

	/*
	//TODO currently only gets the fileName where the declaration is located need a way to get includeFile for declaration
	//TODO currently gets the TU for the definition of an declaration -- no problem for header-only
	//libs but need solution if .h and .cpp are indexed
	const TranslationUnit* tu = (indexer.getDefAndTUforDefinition(decl)).second;
	if( !tu ) return;
	clang::SourceManager& sm = tu->getCompiler().getSourceManager(); 
	*/

	clang::SourceManager& sm = decl->getASTContext().getSourceManager();

	std::cout << "Searching header for: " << node << " of type " << node->getNodeType() << "\n";
	clang::SourceLocation loc = sm.getFileLoc(decl->getLocation());
	if (loc.isInvalid()) return;

	std::pair<clang::FileID, unsigned> loc_info = sm.getDecomposedLoc(loc);
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(loc_info.first);

	// get absolute path of header file
	fs::path header = fs::canonical(fileEntry->getName());

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

//void InterceptVisitor::VisitCallExpr(const clang::CallExpr* callExpr) {};

//void InterceptVisitor::VisitDeclRefExpr(const clang::DeclRefExpr* declRefExpr) {};

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

void Interceptor::collectDeclsToIntercept() {
	//nothing to intercept
	if(toIntercept.empty()) { return; }

	InterceptVisitor vis(interceptedDecls, interceptedFuncMap, toIntercept);

	auto elem = indexer.decl_begin();
	auto end = indexer.decl_end();
	for(;elem != end; elem++) {
		if(const clang::FunctionDecl* decl = llvm::dyn_cast<clang::FunctionDecl>(*elem)) {
			if( regex_match(decl->getQualifiedNameAsString(), rx) ) {
					interceptedDecls.insert(decl);
					interceptedFuncMap.insert( {decl,decl->getQualifiedNameAsString()} );
			} else {
				// check if an intercepted VariableType is used
				vis.intercept(decl, rx);
			}	
		} 
	}

	VLOG(2) << interceptedDecls;
	VLOG(2) << interceptedFuncMap;
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
	} else if( llvm::isa<clang::TypedefType>(type) ) {
		// we use the underlying type of the typedef -- don't intercept typedef
		assert(false && "intercepting TypedefType");
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

	if(typeDecl) {
		return regex_match(typeDecl->getQualifiedNameAsString(), rx);
	}
	
	return false;
}

/// builds expressions (literals) for the intercepted functions and fills the exprCache of the conversionFactory
/// carefull, needs a prefilled (by buildInterceptedTypeCache()) typeCache in the convFact
Interceptor::InterceptedExprCache Interceptor::buildInterceptedExprCache(insieme::frontend::conversion::ConversionFactory& convFact) {
	InterceptedExprCache cache;

	for ( auto it = interceptedFuncMap.begin(), end = interceptedFuncMap.end(); it != end; ++it ) {
		const clang::FunctionDecl* decl = it->first;
		if( !convFact.getProgram().getCallGraph().find(decl).first ) {
			//only called functions should be considered (check callgraph) 
			//FIXME how to handle Templates?
			VLOG(2) << decl << " not called";
			continue;
		}
		
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

		//convertType only works if convFact.ctx.type cache was filled properly with buildInterceptedTypeCache
		core::FunctionTypePtr type = convFact.convertType( decl->getType().getTypePtr() ).as<core::FunctionTypePtr>();
		VLOG(2) << decl << " functionType " << type;
		//fix types for ctor, mfunc, ...
		std::string literalName = it->second;
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

		// remove Clang inline namespace from header literal name (if present)
		literalName = fixQualifiedName(literalName);

		core::ExpressionPtr interceptExpr = builder.literal( literalName, type);

		addHeaderForDecl(interceptExpr, decl, indexer);
		cache.insert( {decl, interceptExpr} );

		VLOG(2) << interceptExpr << " " << type;
		if(insieme::annotations::c::hasIncludeAttached(interceptExpr)) {
			VLOG(2) << "\t attached header: " << insieme::annotations::c::getAttachedInclude(interceptExpr);
		}
	}

	return cache;
}

void InterceptVisitor::intercept(const clang::FunctionDecl* d, boost::regex rx) {
	this->rx = rx;
	if(d->hasBody()) {
		Visit(d->getBody());
	}
}

void InterceptVisitor::VisitStmt(clang::Stmt* stmt) {
	std::for_each(stmt->child_begin(), stmt->child_end(),
		[ this ](clang::Stmt* curr) {if(curr) this->Visit(curr);} );
}

void InterceptVisitor::VisitDeclStmt(const clang::DeclStmt* declStmt) {

	for (auto it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it ) {
		if(const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(*it) ) {
			const clang::Type* type = varDecl->getType().getTypePtr();

			if(const clang::TagType* tagType = type->getAs<clang::TagType>()) {
				const clang::TagDecl* tagDecl = tagType->getDecl();
				if( regex_match(tagDecl->getQualifiedNameAsString(), rx) ) {
					//if we have a varDecl with an intercepted Type
					interceptedDecls.insert( varDecl );

					if(const clang::CXXRecordDecl* cxxRecDecl = llvm::dyn_cast<clang::CXXRecordDecl>(tagDecl)) {
						for(auto it=cxxRecDecl->method_begin(), end=cxxRecDecl->method_end(); it!=end;it++) {
							if( regex_match((*it)->getQualifiedNameAsString(), rx) ) {
								interceptedDecls.insert(*it);
								interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
							}
						}
						
						for(auto it=cxxRecDecl->ctor_begin(), end=cxxRecDecl->ctor_end(); it!=end;it++) {
							if( regex_match((*it)->getQualifiedNameAsString(), rx) ) {
								interceptedDecls.insert(*it);
								interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
							}
						}
					}
				}
			}
		}
	}


}

InterceptTypeVisitor::InterceptTypeVisitor(insieme::frontend::conversion::ConversionFactory& convFact, const insieme::frontend::utils::Indexer& indexer, const boost::regex& rx)
		: convFact(convFact), builder(convFact.getIRBuilder()), indexer(indexer), rx(rx) {}

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

core::TypePtr 
InterceptTypeVisitor::VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy) {
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
