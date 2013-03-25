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
#include "clang/AST/DeclTemplate.h"


namespace insieme{
namespace frontend{
namespace utils{

namespace {

	std::string buildNameTypeChain(const  clang::Decl* decl){
		assert(llvm::isa<clang::NamedDecl>(decl) && "only named decl can be converted to name + type");
		std::string res  = llvm::cast<clang::NamedDecl>(decl)->getQualifiedNameAsString();
		
		if (llvm::isa<clang::FunctionDecl>(decl)){
			res.append(" {");
			res.append(llvm::cast<clang::ValueDecl>(decl)->getType().getAsString());
			res.append("}");
		}
		else if (llvm::isa<clang::CXXRecordDecl>(decl)){
			res.append(" {class}");
		}

		return res;
	}
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class IndexerVisitor{

	private:
		insieme::frontend::TranslationUnit* mTu;
		Indexer::tIndex& mIndex; 

	public:
	IndexerVisitor	(insieme::frontend::TranslationUnit* tu,
					 Indexer::tIndex& index):
		mTu(tu), mIndex(index)
	{ }

	void indexDeclaration(clang::Decl* decl){
		// if it does not have a name, it is another artifact
		// and we dont want to index it
		if (!llvm::isa<clang::NamedDecl>(decl))
				return;

		clang::NamedDecl *named = llvm::cast<clang::NamedDecl>(decl);
		assert (named && "no name Decl, can not be indexed and we dont know what it is");

		if (llvm::isa<clang::FunctionDecl>(decl)) {
			if (decl->hasBody()){
				Indexer::TranslationUnitPair elem =  std::make_pair(decl,mTu); 
				mIndex[buildNameTypeChain(decl)] = elem; 

				// we could write main function in many different ways,
				// best way to find it, is to keep a simple record to address it
				if(named->getNameAsString() == "main")
					mIndex["main"] = elem; 
			}
		}
		else if (const clang::CXXRecordDecl *recDecl = llvm::dyn_cast<clang::CXXRecordDecl>(decl)){
			if (recDecl->hasDefinition()){
				Indexer::TranslationUnitPair elem =  std::make_pair(llvm::cast<clang::Decl>(recDecl->getDefinition()),mTu); 
				mIndex[buildNameTypeChain(decl)] = elem;
			}
			// index inner functions as well
			indexDeclContext(llvm::cast<clang::DeclContext>(decl));
		}
		else if (llvm::isa<clang::NamespaceDecl>(decl)){
			indexDeclContext(llvm::cast<clang::DeclContext>(decl));
		} 
		else if(const clang::TemplateDecl* templDecl = llvm::dyn_cast<clang::TemplateDecl>(decl)) {
			indexDeclaration(templDecl->getTemplatedDecl());
		}
	}

	void indexDeclContext(clang::DeclContext* ctx){
		clang::DeclContext::decl_iterator it = ctx->decls_begin();
		clang::DeclContext::decl_iterator end = ctx->decls_end();
		for (; it != end; it++){
			indexDeclaration(llvm::cast<clang::Decl>(*it));
		}
	}
};


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/////////////////////////////////////////////////
/// the indexer generates an index of all function definitions and Classes
/// the context will be the owner of all generated AST nodes
Indexer::Indexer()
: mIndex() { 
	clang::Decl* ptr1 = NULL;
	insieme::frontend::TranslationUnit* ptr2 = NULL;
	voidPair = std::make_pair(ptr1, ptr2);
}

////////////////////////////////////////////////
///  AST has being already gerated at the parse step
///  It could be parsed here, but there are spetial requirements for the #pragma handeling.
///  As is already build, we just need to iterate the ASTContext and anotate those
///  elements which have a body. those correspond to the Definition and not just declarations.
void Indexer::indexTU (insieme::frontend::TranslationUnit* tu){
	VLOG(1) << " ************* Indexing: " << tu->getFileName() << " ****************";
	const ClangCompiler& compiler = tu->getCompiler();

	clang::TranslationUnitDecl* tuDecl = compiler.getASTContext().getTranslationUnitDecl();
	assert(tuDecl && "AST has not being build");

	clang::DeclContext* ctx= clang::TranslationUnitDecl::castToDeclContext (tuDecl);
	assert(ctx && "AST has no decl context");

	IndexerVisitor indexer(tu, mIndex);
	indexer.indexDeclContext(ctx);
	
	VLOG(1) << " ************* Indexing DONE ****************";
	if (VLOG_IS_ON(2)){
		dump();
	}
}



////////////////////////////////////////////////
//
Indexer::TranslationUnitPair Indexer::getDefAndTUforDefinition (const std::string& symbol) const{

	VLOG(2) << "looking for "<< symbol;
	tIndex::const_iterator match = this->mIndex.find(symbol);
	if (match != this->mIndex.end()){
		assert(match->second.first && match->second.second && " found a wrong definition");
		return match->second;
	}

	return  voidPair;
}

////////////////////////////////////////////////
//
clang::Decl* Indexer::getDefinitionFor (const std::string& symbol) const{

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
Indexer::TranslationUnitPair Indexer::getDefAndTUforDefinition (const clang::Decl* decl) const{
	assert(decl && "Cannot look up null pointer!");

	if(const clang::FunctionDecl* fd = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
		VLOG(2) << fd->getTemplatedKind();
		switch( fd->getTemplatedKind() ) {
			case clang::FunctionDecl::TemplatedKind::TK_NonTemplate:
				return getDefAndTUforDefinition(buildNameTypeChain(fd));
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplate:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_MemberSpecialization:
				VLOG(2) << buildNameTypeChain(fd);
				VLOG(2) << buildNameTypeChain(fd->getMemberSpecializationInfo()->getInstantiatedFrom());
				//FIXME hack (for interception) to get correct translationunit for templatespecialization
				return TranslationUnitPair( { const_cast<clang::FunctionDecl*>(fd), getDefAndTUforDefinition(buildNameTypeChain(fd->getMemberSpecializationInfo()->getInstantiatedFrom())).second});
				//return getDefAndTUforDefinition(buildNameTypeChain(fd->getMemberSpecializationInfo()->getInstantiatedFrom()));
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplateSpecialization:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_DependentFunctionTemplateSpecialization:
				break;
		}
	}
	return getDefAndTUforDefinition(buildNameTypeChain(decl));
}


////////////////////////////////////////////////
//
clang::Decl* Indexer::getDefinitionFor (const clang::Decl* decl) const{
	return getDefinitionFor(buildNameTypeChain(decl));
}


////////////////////////////////////////////////
//
clang::Decl* Indexer::getMainFunctionDefinition () const{
	return getDefinitionFor("main");
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

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//            Indexer iterators
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clang::Decl*& Indexer::iterator::operator*(){
	return curr->second.first;
}

clang::Decl** Indexer::iterator::operator->(){
	return &(curr->second.first);
}

Indexer::iterator Indexer::iterator::operator++(){
	++curr;
	return *this;
}

Indexer::iterator Indexer::iterator::operator++(int d){
	++curr;
	return *this;
}

bool Indexer::iterator::operator!=(const Indexer::iterator& i) const{
	return this->curr != i.curr;
}

Indexer::iterator Indexer::begin(){
	return iterator(mIndex.begin());
}

Indexer::iterator Indexer::end(){
	return iterator(mIndex.end());
}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
