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
		
namespace insieme {
namespace frontend { 
namespace utils {

//void InterceptVisitor::VisitCallExpr(const clang::CallExpr* callExpr) {};

//void InterceptVisitor::VisitDeclRefExpr(const clang::DeclRefExpr* declRefExpr) {};

void Interceptor::loadConfigFile(std::string fileName) {
	namespace fs = boost::filesystem;
	//FIXME toIntercept should use regex -> convert strings into regex before storing them
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
	//FIXME use regex, turn strings in tI into regex
	toIntercept.insert(tI.begin(), tI.end());
}

/// takes a pair of strings and looks for functionsi (funcDecl) with the same name as the first string, 
/// adds the second string associated with the funcDecl to the interceptedFuncCache
void Interceptor::intercept() {
	if(toIntercept.empty()) {
		VLOG(2) << "nothing to intercept";
		return;
	}

	InterceptVisitor vis(interceptedDecls, interceptedTypes, toIntercept);

	auto elem = indexer.begin();
	auto end = indexer.end();
	for(;elem != end; elem++) {
		if(llvm::isa<clang::FunctionDecl>(*elem)) {
			const clang::FunctionDecl* decl = llvm::cast<clang::FunctionDecl>(*elem);

			//FIXME unique name? -- use buildNameTypeChain from indexer?
			if( toIntercept.find(decl->getQualifiedNameAsString()) != toIntercept.end() && (decl)->hasBody()) {
				VLOG(2) << "intercept funcDecl " << decl->getQualifiedNameAsString();
				interceptedDecls.insert(decl);
				interceptedFuncMap.insert( {decl,decl->getQualifiedNameAsString()} );
				interceptedTypes.insert( decl->getType().getTypePtr());
			} else {
				// check if an intercepted VariableType is used
				vis.intercept(decl);
			}
		}

		if(llvm::isa<clang::CXXRecordDecl>(*elem)) {
			const clang::CXXRecordDecl* decl = llvm::cast<clang::CXXRecordDecl>(*elem);

			if( toIntercept.find(decl->getQualifiedNameAsString()) != toIntercept.end()) {
				VLOG(2) << "intercept CXXRecordDecl " << decl->getQualifiedNameAsString();
				interceptedTypes.insert( decl->getTypeForDecl() );
			}

			for(auto it=decl->method_end(), end=decl->method_end(); it!=end;it++) {

				if( toIntercept.find((*it)->getQualifiedNameAsString()) != toIntercept.end()) {
					VLOG(2) << "intercept memberFunc " << (*it)->getQualifiedNameAsString();
					interceptedDecls.insert(*it);
					interceptedFuncMap.insert( {(*it), (*it)->getQualifiedNameAsString() } ); 
					interceptedTypes.insert( (*it)->getType().getTypePtr());
				}
			}
		}
	}

	VLOG(2) << interceptedDecls;
	VLOG(2) << interceptedFuncMap;
	VLOG(2) << interceptedTypes;
}

Interceptor::InterceptedExprCache Interceptor::buildInterceptedExprCache(insieme::frontend::conversion::ConversionFactory& convFact) {
	InterceptedExprCache cache;

	for ( auto it = interceptedFuncMap.begin(), end = interceptedFuncMap.end(); it != end; ++it ) {
		VLOG(2) << it->first->getNameAsString();
		core::TypePtr&& type = convFact.convertType((it->first)->getType().getTypePtr());

		core::ExpressionPtr interceptExpr = builder.literal( it->second, type);
		VLOG(2) << interceptExpr;
		
		//FIXME annotate literal - headerfile?
		cache.insert( {it->first, interceptExpr} );
	}
	VLOG(2) << cache;

	/*
	for( auto it = interceptedTypes.begin(), end=interceptedTypes.end(); it != end; it++) {
		core::TypePtr&& type = convFact.convertType(*it);
		//FIXME annotate type - headerFile?
		//
		//genericType("namespace::type" : listofparents<T>)
	}

	VLOG(2) << cache;
	*/

	return cache;
}

void InterceptVisitor::intercept(const clang::FunctionDecl* d) {
	Visit(d->getBody());
}

void InterceptVisitor::VisitStmt(clang::Stmt* stmt) {
	std::for_each(stmt->child_begin(), stmt->child_end(),
		[ this ](clang::Stmt* curr) {if(curr) this->Visit(curr);});
}

void InterceptVisitor::VisitDeclStmt(const clang::DeclStmt* declStmt) {

	//FIXME currently only for SingleDecls
	if( llvm::isa<clang::VarDecl>(declStmt->getSingleDecl()) ) {
		const clang::VarDecl* varDecl = llvm::cast<clang::VarDecl>(declStmt->getSingleDecl());
		
		if(const clang::CXXRecordDecl* cxxRecDecl = varDecl->getType().getTypePtr()->getAsCXXRecordDecl()) {
			if( toIntercept.find(cxxRecDecl->getQualifiedNameAsString()) != toIntercept.end()) {
				//if we have a varDecl with an intercepted Type
				VLOG(2) << "intercept VarDecl (class type) " << cxxRecDecl->getQualifiedNameAsString() << " name " << varDecl->getQualifiedNameAsString();
				interceptedDecls.insert(varDecl);
				interceptedTypes.insert(varDecl->getType().getTypePtr());
			}
		}
	} else {
		assert(false && "only single decls are intercepted currently");
	}
}


} // end utils namespace
} // end frontend namespace
} // end insieme namespace
