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

#include "insieme/frontend/convert.h"
#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/regex.hpp>
		
namespace insieme {
namespace frontend { 
namespace utils {

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

		std::string nameToIntercept;
		while( getline(configFile, nameToIntercept) ) {
			toIntercept.insert(nameToIntercept);
		}
		configFile.close();
	} else {
		LOG(WARNING) << "Interceptor didn't find config file " << fileName;	
		return;
	}
}

void Interceptor::loadConfigSet(std::set<std::string> tI) {
	toIntercept.insert(tI.begin(), tI.end());
}

/// takes a pair of strings and looks for functionsi (funcDecl) with the same name as the first string, 
/// adds the second string associated with the funcDecl to the interceptedFuncCache
void Interceptor::intercept() {
	if(toIntercept.empty()) {
		return;
	}

	//use one big regex for all strings to intercept
	using boost::regex;
	auto it = toIntercept.begin();
	auto interceptEnd = toIntercept.end();
	std::string tI("("+(*it)+")");
	for(it++ ;it != interceptEnd; it++) {
		tI = tI + "|("+ (*it) +")";	
	}
	regex rx(tI);

	InterceptVisitor vis(interceptedDecls, interceptedFuncMap, interceptedTypes, toIntercept);

	auto elem = indexer.begin();
	auto end = indexer.end();
	for(;elem != end; elem++) {
		if(llvm::isa<clang::FunctionDecl>(*elem)) {
			const clang::FunctionDecl* decl = llvm::cast<clang::FunctionDecl>(*elem);

			//FIXME unique name? -- use buildNameTypeChain from indexer?
			if( regex_match(decl->getQualifiedNameAsString(), rx) && (decl)->hasBody()) {
				VLOG(2) << "intercept funcDecl " << decl->getQualifiedNameAsString();
				interceptedDecls.insert(decl);
				interceptedFuncMap.insert( {decl,decl->getQualifiedNameAsString()} );
			} else {
				// check if an intercepted VariableType is used
				vis.intercept(decl, rx);
			}
		} else if( llvm::isa<clang::TypeDecl>(*elem) ) {
			const clang::TypeDecl* typeDecl = llvm::cast<clang::TypeDecl>(*elem);

			if( regex_match(typeDecl->getQualifiedNameAsString(), rx) ) {
				VLOG(2) << "intercept TypeDecl " << typeDecl->getQualifiedNameAsString();
				interceptedTypes.insert( typeDecl );
		
				if(llvm::isa<clang::CXXRecordDecl>(typeDecl)) {
					const clang::CXXRecordDecl* cxxRecDecl = llvm::cast<clang::CXXRecordDecl>(typeDecl);
					VLOG(2) << "intercept CXXRecordDecl " << cxxRecDecl->getQualifiedNameAsString();
					if(cxxRecDecl->getDescribedClassTemplate()) {
						VLOG(2) << "describedClassTemplate " << cxxRecDecl->getDescribedClassTemplate()->getQualifiedNameAsString();
					}

					for(auto it=cxxRecDecl->method_begin(), end=cxxRecDecl->method_end(); it!=end;it++) {
						if( regex_match((*it)->getQualifiedNameAsString(), rx) && (*it)->hasBody()) {
							VLOG(2) << "intercept CXXMethodDecl " << (*it)->getQualifiedNameAsString();
							interceptedDecls.insert(*it);
							interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
						}
					}
					
					for(auto it=cxxRecDecl->ctor_begin(), end=cxxRecDecl->ctor_end(); it!=end;it++) {
						if( regex_match((*it)->getQualifiedNameAsString(), rx) && (*it)->hasBody()) {
							VLOG(2) << "intercept CXXConstructorDecl " << (*it)->getQualifiedNameAsString();
							interceptedDecls.insert(*it);
							interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
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
	for( auto it = interceptedTypes.begin(), end=interceptedTypes.end(); it != end; it++) {
		//FIXME Templates? Inheritance?
		//genericType(nameString, ParentTypeList, TypeParamList, emptyIntParamList
		
		insieme::core::TypeList typeList; //empty typelist  = insieme::core::TypeList();

		if(llvm::isa<clang::ClassTemplateSpecializationDecl>(*it)) {
			VLOG(2) << (*it)->getQualifiedNameAsString() << " take care of template types";
			const clang::TemplateArgumentList& args= llvm::cast<clang::ClassTemplateSpecializationDecl>(*it)->getTemplateArgs();
			for(size_t i = 0; i<args.size();i++) {
				VLOG(2) << args[i].getKind();
				switch(args[i].getKind()) {//
					case clang::TemplateArgument::ArgKind::Null: (*it)->dump(); VLOG(2) << "ArgKind::Null not supported"; break;
					case clang::TemplateArgument::ArgKind::Type:
						{
							const clang::Type* argType = args[i].getAsType().getTypePtr();
							VLOG(2) << args[i].getAsType().getAsString() << " isBuiltinType " << argType->isBuiltinType(); 

							if( argType->isBuiltinType() ) {
								//for builtinTypes use typeConverter
								//typeList.insert(typeList.end(), builder.typeVariable("var"+toString(i)));
								
								//FIXME how do we handle the std::nullptr_t -> currently "null" from typeConverter
								typeList.insert( typeList.end(), convFact.convertType( argType ) );
							} else {
								typeList.insert( typeList.end(), builder.typeVariable("var"+toString(i)) );
							}
						}
						break;
					case clang::TemplateArgument::ArgKind::Declaration:
						(*it)->dump();
						VLOG(2) << "ArgKind::Declaration not supported"; break;
					case clang::TemplateArgument::ArgKind::NullPtr: (*it)->dump(); VLOG(2) << "ArgKind::NullPtr not supported"; break;
					case clang::TemplateArgument::ArgKind::Integral: (*it)->dump(); VLOG(2) << "ArgKind::Integral not supported"; break;
					case clang::TemplateArgument::ArgKind::Template: (*it)->dump(); VLOG(2) << "ArgKind::Template not supported"; break;
					case clang::TemplateArgument::ArgKind::TemplateExpansion: (*it)->dump(); VLOG(2) << "ArgKind::TemplateExpansion not supported"; break;
					case clang::TemplateArgument::ArgKind::Expression: (*it)->dump(); VLOG(2) << "ArgKind::Expression not supported"; break;
					case clang::TemplateArgument::ArgKind::Pack: (*it)->dump(); VLOG(2) << "ArgKind::Pack not supported"; break;
				}
			}
		}

		//FIXME annotate header in type
		/*
		if(const clang::TranslationUnit* tu = (indexer.getDefinitionFor(*it)).second ) {
			//"getheader"
			clang::SourceLocation loc = tu->getCompiler().getSourceManager().getFileLoc((*it)->getLocation());
			std::pair<clang::FileID, unsigned> loc_info = tu->getCompiler().getSourceManager().getDecomposedLoc(loc);
			const clang::FileEntry* file_entry = tu->getCompiler().getSourceManager().getFileEntryForID(loc_info.first);
			VLOG(2) << file_entry->getName();
		}
		*/

		core::TypePtr type = builder.genericType((*it)->getQualifiedNameAsString(), typeList, insieme::core::IntParamList());
		VLOG(2) << "buildInterceptedType " << (*it)->getQualifiedNameAsString() << " ## " << type;
		cache.insert( { (*it)->getTypeForDecl(), type });
	}
	VLOG(2) << cache;
	return cache;
}

/// builds expressions (literals) for the intercepted functions and fills the exprCache of the conversionFactory
/// carefull, needs a prefilled (by buildInterceptedTypeCache()) typeCache in the convFact
Interceptor::InterceptedExprCache Interceptor::buildInterceptedExprCache(insieme::frontend::conversion::ConversionFactory& convFact) {
	InterceptedExprCache cache;

	for ( auto it = interceptedFuncMap.begin(), end = interceptedFuncMap.end(); it != end; ++it ) {
		const clang::FunctionDecl* decl = it->first;
		//FIXME only called functions should be considered (check callgraph) -> how to handle Templates?
		if( !convFact.getProgram().getCallGraph().find(decl).first ) { VLOG(2) << decl << " not called"; continue; }
		
		/*FIXME create generric type for templates?
		 * switch( decl->getTemplatedKind() ) {
			case clang::FunctionDecl::TemplatedKind::TK_NonTemplate:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplate:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_MemberSpecialization:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplateSpecialization:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_DependentFunctionTemplateSpecialization:
				break;
		}*/

		//core::FunctionTypePtr type = builder.functionType( insieme::core::TypeList(), builder.getLangBasic().getUnit());
		//FIXME convertType only works currently for simple types !!!NON-CXXClasses!!!
		core::FunctionTypePtr type = convFact.convertType( decl->getType().getTypePtr() ).as<core::FunctionTypePtr>();
		VLOG(2) << decl->getQualifiedNameAsString() << " " << type;

		//FIXME fix types for ctor, mfunc, ...
		if( llvm::isa<clang::CXXConstructorDecl>(decl) ) {
			const clang::CXXConstructorDecl* ctorDecl = llvm::cast<clang::CXXConstructorDecl>(decl);

			core::TypePtr thisTy = convFact.convertType(ctorDecl->getParent()->getTypeForDecl());
			core::TypeList paramTys = type->getParameterTypeList();
			paramTys.insert(paramTys.begin(), builder.refType(thisTy));
			
			//FIXME can we use memberize()?
			type = builder.functionType( paramTys, builder.refType(thisTy), core::FK_CONSTRUCTOR);
		} else if( llvm::isa<clang::CXXMethodDecl>(decl) ) {
			const clang::CXXMethodDecl* methodDecl = llvm::cast<clang::CXXMethodDecl>(decl);

			core::TypePtr thisTy = convFact.convertType(methodDecl->getParent()->getTypeForDecl());
			//core::TypePtr thisTy = builder.genericType(methodDecl->getParent()->getQualifiedNameAsString(), insieme::core::TypeList(), insieme::core::IntParamList());
			core::TypeList paramTys = type->getParameterTypeList();
			//FIXME currently only a typeVariable for "this"-Type
			paramTys.insert(paramTys.begin(), builder.refType(thisTy));

			//FIXME can we use memberize()?
			type = builder.functionType( paramTys, type.getReturnType(), core::FK_MEMBER_FUNCTION);
		}

		core::ExpressionPtr interceptExpr = builder.literal( it->second, type);
		VLOG(2) << interceptExpr << " " << type;
	
		//FIXME annotate header
		if( const TranslationUnit* tu = ((indexer.getDefAndTUforDefinition(decl)).second) ) {
			//"getheader"
			clang::SourceLocation loc = tu->getCompiler().getSourceManager().getFileLoc(decl->getLocation());
			std::pair<clang::FileID, unsigned> loc_info = tu->getCompiler().getSourceManager().getDecomposedLoc(loc);
			const clang::FileEntry* file_entry = tu->getCompiler().getSourceManager().getFileEntryForID(loc_info.first);
			VLOG(2) << file_entry->getName();
		}	

		cache.insert( {decl, interceptExpr} );
	}

	return cache;
}

void InterceptVisitor::intercept(const clang::FunctionDecl* d, boost::regex rx) {
	this->rx = rx;
	Visit(d->getBody());
}

void InterceptVisitor::VisitStmt(clang::Stmt* stmt) {
	std::for_each(stmt->child_begin(), stmt->child_end(),
		[ this ](clang::Stmt* curr) {if(curr) this->Visit(curr);});
}

void InterceptVisitor::VisitDeclStmt(const clang::DeclStmt* declStmt) {

	for (auto it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it ) {
		if( llvm::isa<clang::VarDecl>(*it) ) {
			const clang::VarDecl* varDecl = llvm::cast<clang::VarDecl>(*it);
			
			if(const clang::CXXRecordDecl* cxxRecDecl = varDecl->getType().getTypePtr()->getAsCXXRecordDecl()) {

				if( regex_match(cxxRecDecl->getQualifiedNameAsString(), rx) ) {
					//if we have a varDecl with an intercepted Type
					interceptedDecls.insert( varDecl );
					interceptedTypes.insert( cxxRecDecl );

					for(auto it=cxxRecDecl->method_begin(), end=cxxRecDecl->method_end(); it!=end;it++) {
						if( regex_match((*it)->getQualifiedNameAsString(), rx) ) {
							VLOG(2) << "intercept CXXMethodDecl " << (*it)->getQualifiedNameAsString();
							interceptedDecls.insert(*it);
							interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
						}
					}
					
					for(auto it=cxxRecDecl->ctor_begin(), end=cxxRecDecl->ctor_end(); it!=end;it++) {
						if( regex_match((*it)->getQualifiedNameAsString(), rx) ) {
							VLOG(2) << "intercept CXXConstructorDecl " << (*it)->getQualifiedNameAsString();
							interceptedDecls.insert(*it);
							interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
						}
					}
				}
			}
		}
	}


}


} // end utils namespace
} // end frontend namespace
} // end insieme namespace
