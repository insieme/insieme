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

#include "insieme/frontend/utils/indexer.h"
#include "insieme/utils/logging.h"

#include "clang/Sema/Sema.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Parse/ParseAST.h"


namespace insieme{
namespace frontend{
namespace utils{


//////////////////////////////////////////////////////////////////////////////////////////////
// the indexer consumer populates the index as
// the first pass parse generates AST
//////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////
template <class tIndex>
class indexerASTConsumer : public clang::ASTConsumer {

	tIndex& mIndex;
	insieme::frontend::TranslationUnit* tu;

public:
	//////////////////////////////////////////////////
	//
    indexerASTConsumer(tIndex& index, insieme::frontend::TranslationUnit* tu_)
	: mIndex(index),
	  tu(tu_)
    {}

	//////////////////////////////////////////////////
    /// Override the method that gets called for each parsed top-level
    /// declaration.
    virtual bool HandleTopLevelDecl(clang::DeclGroupRef DR) {

		for (clang::DeclGroupRef::iterator b = DR.begin(), e = DR.end();
				b != e; ++b){

			if (llvm::isa<clang::NamedDecl>(*b)){
				clang::Decl *decl = llvm::cast<clang::Decl>(*b);
				clang::NamedDecl *named = llvm::cast<clang::NamedDecl>(*b);

				assert (decl && "no declaration");
				assert (named && "no name");

				
				if (named->hasBody()){
					//tStored elem = 
					std::pair<clang::Decl*, insieme::frontend::TranslationUnit*> elem =  std::make_pair(decl,tu); 
																					//decl->getTranslationUnitDecl();
					// FIXME: check if qualified name needed, or just name
					mIndex[named->getNameAsString()] = elem;
				}
			}
		}

		return true;
    }
};


//////////////////////////////////////////////////////////////////////////////////////////////
// the indexer generates an index of 
//////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////
// the context will be the owner of all generated AST nodes
Indexer::Indexer()
: mIndex() { 
	clang::Decl* ptr1 = NULL;
	insieme::frontend::TranslationUnit* ptr2 = NULL;
	voidPair = std::make_pair(ptr1, ptr2);
}

////////////////////////////////////////////////
//
void Indexer::indexTU (insieme::frontend::TranslationUnit* tu){
	
	const ClangCompiler& compiler = tu->getCompiler();

	clang::Preprocessor& preprocessor = compiler.getPreprocessor();
	clang::ASTContext& mASTContext   = compiler.getASTContext();

	clang::ASTConsumer *consumer = new indexerASTConsumer<tIndex>(mIndex, tu);

	// FIXME: is needed to use our sema??
	clang::Sema mySema (preprocessor, mASTContext, *consumer);
	ParseAST(mySema, false, false);

	delete consumer;
}

////////////////////////////////////////////////
//
tStored Indexer::getDefAndTUforDefinition (std::string symbol){

	tIndex::iterator match = this->mIndex.find(symbol);
	if (match != this->mIndex.end())
		return match->second;
	else 
		return  voidPair;

}

////////////////////////////////////////////////
//
clang::Decl* Indexer::getDefDefinitionFor (std::string symbol){

	tIndex::iterator match = this->mIndex.find(symbol);
	if (match != this->mIndex.end())
		return match->second.first;
	else 
		return  NULL;
}

////////////////////////////////////////////////
//
void Indexer::dump(){
	tIndex::iterator it = mIndex.begin();
	tIndex::iterator end = mIndex.end();
	for (;it != end; it++){
		std::cout << "\t[" << it->first << " ," << it->second << "]" << std::endl;
	}
}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
