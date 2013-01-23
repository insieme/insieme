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

#include <iostream>
#include <assert.h>
#include <string>
#include <map>
#include <stdint.h>

// FIXME this needs a cleaner solution
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "insieme/frontend/compiler.h"
#include "insieme/frontend/utils/indexer.h"
#include "insieme/frontend/pragma/handler.h"

#include "insieme/utils/logging.h"

#include "clang/Sema/Sema.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Parse/ParseAST.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclBase.h"





namespace insieme{
namespace frontend{
namespace utils{

//////////////////////////////////////////////////////////////////////////////////////////////
/// the indexer generates an index of 
/// the context will be the owner of all generated AST nodes
Indexer::Indexer()
: mIndex() { 
	clang::Decl* ptr1 = NULL;
	insieme::frontend::TranslationUnit* ptr2 = NULL;
	voidPair = std::make_pair(ptr1, ptr2);
}

////////////////////////////////////////////////
//  AST has being already gerated at the parse step
//  It could be parsed here, but there are spetial requirements for the #pragma handeling.
//  As is already build, we just need to iterate the ASTContext and anotate those
//  elements which have a body. those correspond to the Definition and not just declarations.
void Indexer::indexTU (insieme::frontend::TranslationUnit* tu){
	const ClangCompiler& compiler = tu->getCompiler();

	VLOG(1) << "=== Indexing TU ====";
	clang::TranslationUnitDecl* tuDecl = compiler.getASTContext().getTranslationUnitDecl();
	assert(tuDecl && "AST has not being build");

	clang::DeclContext* ctx= clang::TranslationUnitDecl::castToDeclContext (tuDecl);
	assert(ctx && "AST has no decl context");

	clang::DeclContext::decl_iterator it = ctx->decls_begin();
	clang::DeclContext::decl_iterator end = ctx->decls_end();
	for (; it != end; it++){
		if (llvm::isa<clang::FunctionDecl>(*it)){

			clang::Decl *decl = llvm::cast<clang::Decl>(*it);
			clang::NamedDecl *named = llvm::cast<clang::NamedDecl>(*it);

			assert (decl && "no declaration");
			assert (named && "no name");
		
			if (named->hasBody()){
				tStored elem =  std::make_pair(decl,tu); 
				mIndex[named->getNameAsString()] = elem; // FIXME: check if qualified name needed, or just name
			}
		}
	}
	VLOG(1) << "=== Indexing DONE ====";
}

////////////////////////////////////////////////
//
Indexer::tStored Indexer::getDefAndTUforDefinition (const std::string& symbol) const{

	tIndex::const_iterator match = this->mIndex.find(symbol);
	if (match != this->mIndex.end()){
		assert(match->second.first && match->second.second && " found a wrong definition");
		return match->second;
	}else {
		return  voidPair;
	}

}

////////////////////////////////////////////////
//
clang::Decl* Indexer::getDefDefinitionFor (const std::string& symbol) const{

	tIndex::const_iterator match = this->mIndex.find(symbol);
	if (match != this->mIndex.end()){
		assert(match->second.first && " found a wrong definition");
		return match->second.first;
	}
	else {
		return  NULL;
	}
}

////////////////////////////////////////////////
///
Indexer::tStored Indexer::getDefAndTUforDefinition (clang::Decl* decl) const{
	return getDefAndTUforDefinition(llvm::cast<clang::NamedDecl>(decl)->getNameAsString());
}


////////////////////////////////////////////////
//
clang::Decl* Indexer::getDefDefinitionFor (clang::Decl* decl) const{
	return getDefDefinitionFor(llvm::cast<clang::NamedDecl>(decl)->getNameAsString());
}


////////////////////////////////////////////////
//
void Indexer::dump() const{
	tIndex::const_iterator it = mIndex.begin();
	tIndex::const_iterator end = mIndex.end();
	for (;it != end; it++){
		std::cout << "\t[" << it->first << " ," << it->second << "]" << std::endl;
	}
}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
