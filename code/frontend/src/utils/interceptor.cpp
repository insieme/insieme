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


	//TODO currently only gets the fileName where the declaration is located need a way to get includeFile for declaration
	const TranslationUnit* tu = (indexer.getDefAndTUforDefinition(decl)).second;
	if( !tu ) return;

	std::cout << "Searching header for: " << node << " of type " << node->getNodeType() << "\n";
	clang::SourceLocation loc = tu->getCompiler().getSourceManager().getFileLoc(decl->getLocation());
	if (loc.isInvalid()) return;

	std::pair<clang::FileID, unsigned> loc_info = tu->getCompiler().getSourceManager().getDecomposedLoc(loc);
	const clang::FileEntry* fileEntry = tu->getCompiler().getSourceManager().getFileEntryForID(loc_info.first);

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

/// takes a pair of strings and looks for functionsi (funcDecl) with the same name as the first string, 
/// adds the second string associated with the funcDecl to the interceptedFuncCache
void Interceptor::intercept() {
	if(toIntercept.empty()) {
		return;
	}

	InterceptVisitor vis(interceptedDecls, interceptedFuncMap, interceptedTypes, toIntercept);

	auto elem = indexer.begin();
	auto end = indexer.end();
	for(;elem != end; elem++) {
		if(const clang::FunctionDecl* decl = llvm::dyn_cast<clang::FunctionDecl>(*elem)) {
			if( regex_match(decl->getQualifiedNameAsString(), rx) ) {
					interceptedDecls.insert(decl);
					interceptedFuncMap.insert( {decl,decl->getQualifiedNameAsString()} );
			} else {
				// check if an intercepted VariableType is used
				vis.intercept(decl, rx);
			}	
		} else if( const clang::TypeDecl* typeDecl = llvm::dyn_cast<clang::TypeDecl>(*elem) ) {
			if( regex_match(fixQualifiedName(typeDecl->getQualifiedNameAsString()), rx) ) {
				interceptedTypes.insert( typeDecl );
		
				if(const clang::CXXRecordDecl* cxxRecDecl = llvm::dyn_cast<clang::CXXRecordDecl>(typeDecl)) {
					for(auto mit=cxxRecDecl->method_begin(), end=cxxRecDecl->method_end(); mit!=end;mit++) {
						if( regex_match(fixQualifiedName((*mit)->getQualifiedNameAsString()), rx) ) {
							interceptedDecls.insert(*mit);
							interceptedFuncMap.insert( {(*mit), (*mit)->getQualifiedNameAsString() } ); 
						}
					}
					
					for(auto cit=cxxRecDecl->ctor_begin(), end=cxxRecDecl->ctor_end(); cit!=end;cit++) {
						if( regex_match(fixQualifiedName((*cit)->getQualifiedNameAsString()), rx) ) {
							interceptedDecls.insert(*cit);
							interceptedFuncMap.insert( {(*cit), (*cit)->getQualifiedNameAsString() } ); 
						}
					}
				}
			}		
		}
	}

	VLOG(2) << interceptedDecls;
	VLOG(2) << interceptedFuncMap;
	VLOG(2) << interceptedTypes;
}

/// builds and fills the typeCache used by the conversionFactory
/// carefull, a call to convertType() also adds the convertedType to the typeCache used in the
/// conversion step
Interceptor::InterceptedTypeCache Interceptor::buildInterceptedTypeCache(insieme::frontend::conversion::ConversionFactory& convFact) {
	InterceptedTypeCache cache;
	InterceptTypeVisitor iTV(convFact, indexer, rx);

	for( auto it = interceptedTypes.begin(), end=interceptedTypes.end(); it != end; it++) {
		const clang::TypeDecl* typeDecl = *it;
		const clang::Type* currInterceptedType = typeDecl->getTypeForDecl();

		// resolve type and save in cache
		core::TypePtr irType = iTV.Visit( currInterceptedType );
		cache.insert( { currInterceptedType, irType } );
		VLOG(1) << "build interceptedType " << (*it)->getQualifiedNameAsString() << " ## " << irType;

		// add header file
		addHeaderForDecl(irType, typeDecl, indexer);
	}
	return cache;
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
		VLOG(2) << interceptExpr << " " << type;

		addHeaderForDecl(interceptExpr, decl, indexer);
		cache.insert( {decl, interceptExpr} );

		if(insieme::annotations::c::hasIncludeAttached(interceptExpr)) {
			VLOG(2) << insieme::annotations::c::getAttachedInclude(interceptExpr);
		}
	}

	return cache;
}



void InterceptVisitor::intercept(const clang::FunctionDecl* d, boost::regex rx) {
	this->rx = rx;
	Visit(d->getBody());
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
					interceptedTypes.insert( tagDecl );

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

	insieme::core::TypeList typeList; //empty typelist  = insieme::core::TypeList();
	if(llvm::isa<clang::ClassTemplateSpecializationDecl>(tagDecl)) {
		const clang::TemplateArgumentList& args= llvm::cast<clang::ClassTemplateSpecializationDecl>(tagDecl)->getTemplateArgs();
		for(size_t i = 0; i<args.size();i++) {
			switch(args[i].getKind()) {
				case clang::TemplateArgument::ArgKind::Null: VLOG(2) << "ArgKind::Null not supported"; break;
				case clang::TemplateArgument::ArgKind::Type:
					{
						const clang::Type* argType = args[i].getAsType().getTypePtr();
						typeList.insert( typeList.end(), Visit(argType) );
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

	core::TypePtr irType = builder.genericType(typeName, typeList, insieme::core::IntParamList());
	return irType;
}

core::TypePtr InterceptTypeVisitor::VisitInjectedClassNameType(const clang::InjectedClassNameType* type) {
	insieme::core::TypeList typeList; //empty typelist  = insieme::core::TypeList();
	
	VLOG(2) << type->getInjectedSpecializationType().getTypePtr()->getTypeClassName();
	return Visit(type->getInjectedSpecializationType().getTypePtr());
}

core::TypePtr InterceptTypeVisitor::VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy) {
	VLOG(2) << "TemplateName: " << templTy->getTemplateName().getAsTemplateDecl()->getQualifiedNameAsString();
	VLOG(2) << "numTemplateArg: " << templTy->getNumArgs();
	insieme::core::TypeList typeList; //empty typelist  = insieme::core::TypeList();
	for(size_t argId=0, end=templTy->getNumArgs(); argId < end; argId++) {
		const clang::TemplateArgument arg = templTy->getArg(argId);

		switch(arg.getKind()) {
			case clang::TemplateArgument::ArgKind::Null: VLOG(2) << "ArgKind::Null not supported"; break;
			case clang::TemplateArgument::ArgKind::Type:
				{
					const clang::Type* argType = arg.getAsType().getTypePtr();
					core::TypePtr irArgType = Visit(argType);
					if(irArgType) {
						typeList.insert( typeList.end(), irArgType );
					} else {
						VLOG(2) << argType->getTypeClassName() << " type not supported";
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

	// compute resulting type name
	string typeName = fixQualifiedName(templTy->getTemplateName().getAsTemplateDecl()->getQualifiedNameAsString());

	// build resulting type
	core::TypePtr retTy = builder.genericType(typeName, typeList, insieme::core::IntParamList());

	VLOG(2)<<retTy;
	return retTy;
}

core::TypePtr InterceptTypeVisitor::VisitTemplateTypeParmType(const clang::TemplateTypeParmType* templParmType) {
	if( const clang::TemplateTypeParmDecl* tD = templParmType->getDecl() ) {
		VLOG(2) << tD->getNameAsString();
		return builder.genericType(fixQualifiedName(tD->getNameAsString()), insieme::core::TypeList(), insieme::core::IntParamList());
	} 
	return builder.genericType("asdf", insieme::core::TypeList(), insieme::core::IntParamList());
}

core::TypePtr InterceptTypeVisitor::VisitElaboratedType(const clang::ElaboratedType* elabType) {
	return Visit( elabType->getNamedType().getTypePtr() );
}

core::TypePtr InterceptTypeVisitor::VisitBuiltinType(const clang::BuiltinType* type) { 
	return convFact.convertType(type);
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
