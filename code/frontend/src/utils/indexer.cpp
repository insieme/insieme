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
#include "clang/AST/DeclFriend.h"
#include "clang/AST/DeclTemplate.h"


namespace insieme{
namespace frontend{
namespace utils{

namespace {

	std::string buildNameTypeChain(const  clang::Decl* decl){
		assert(llvm::isa<clang::NamedDecl>(decl) && "only named decl can be converted to name + type");
		std::string res  = llvm::cast<clang::NamedDecl>(decl)->getQualifiedNameAsString();
			
//		// remove type spetialitation
//		std::string tmp;
//		unsigned cnt = 0;
//		for (unsigned i= 0; i < res.size(); i++){
//			char cur = res.c_str()[i];
//			if (cur == '<') cnt++;
//			else if (cur == '>') cnt--;
//			else if (cnt == 0) tmp.insert(tmp.end(), 1, cur);
//		}
//		if (cnt == 0)
//			res = tmp;

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
		std::set<const clang::Decl*> processed;

	public:
	IndexerVisitor	(insieme::frontend::TranslationUnit* tu,
					 Indexer::tIndex& index):
		mTu(tu), mIndex(index) 
	{ }

	void indexDeclaration(clang::Decl* decl){

		// if already index, done
		if(!processed.insert(decl).second) return;

		// === FRIEND DECL ====
		if( const clang::FriendDecl* f = llvm::dyn_cast<clang::FriendDecl>(decl) ) {
			//friendDecl is not a nameDecl
			if(f->getFriendDecl()) {	
				//get the actual friendDecl
				indexDeclaration(f->getFriendDecl());	
			} else {
				//friendType --> getFriendType()
				//assert(false && "indexer -- friendType not implemented");
			}
		// === named DECL ====
		// this might be anything with a name
		} else if (const clang::NamedDecl *named = llvm::dyn_cast<clang::NamedDecl>(decl)) {
			assert (named && "no name Decl, can not be indexed and we dont know what it is");

			// === Function Decl ===
			if (llvm::isa<clang::FunctionDecl>(decl)) {
				Indexer::TranslationUnitPair elem =  std::make_pair(decl,mTu); 
				if (decl->hasBody()){
					mIndex[buildNameTypeChain(decl)] = elem; 

					// we could write main function in many different ways,
					// best way to find it, is to keep a simple record to address it
					if(named->getNameAsString() == "main")
						mIndex["main"] = elem; 
				}
			}
			// === tag Decl ===
			else if (const clang::TagDecl* tag = llvm::dyn_cast<clang::TagDecl>(decl)) {

				switch (tag->getTagKind ()){
					case clang::TagDecl::TagKind::TTK_Struct :
					case clang::TagDecl::TagKind::TTK_Union 	:
					case clang::TagDecl::TagKind::TTK_Class 	:
						{
						Indexer::TranslationUnitPair elem =  std::make_pair(decl,mTu); 
						mIndex[buildNameTypeChain(decl)] = elem;
						indexDeclContext(llvm::cast<clang::DeclContext>(decl));
						}
						break;
					case clang::TagDecl::TagKind::TTK_Enum 	:

						// index enums?? 
						break;

					case clang::TagDecl::TagKind::TTK_Interface :
						// FIXME: do we need this??
						break;
				}
			}
			// === Namespace Decl ===
			else if (llvm::isa<clang::NamespaceDecl>(decl)){
				indexDeclContext(llvm::cast<clang::DeclContext>(decl));
			} 
			// === templDecl Decl ===
			else if(const clang::TemplateDecl* templDecl = llvm::dyn_cast<clang::TemplateDecl>(decl)) {
				indexDeclaration(templDecl->getTemplatedDecl());
			}
			// === variable Decl ===
			else if (const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl)){
				// if is a variable, we migh not need to index itself, but 
				// template spetialitation might have member functions with a new signature
				// those member functions need to be indexed, to enable dependence analysis 
				
				const clang::Type* type = varDecl->getType().getTypePtr();
				if (const clang::RecordType* rec = llvm::dyn_cast<clang::RecordType>(type)){
					indexDeclContext(llvm::cast<clang::DeclContext>(rec->getDecl()));
				}
			}
		} 
		// === linkage spec DECL ====
		else if (llvm::isa<clang::LinkageSpecDecl>(decl)) {
			indexDeclContext(llvm::cast<clang::DeclContext>(decl));
		}
		// === default ====
		else {
			//default case -- if DeclContext, try to index it
			if(clang::DeclContext* declContext = llvm::dyn_cast<clang::DeclContext>(decl)) {
				indexDeclContext(declContext);
			}
		}
				
		// if it does not have a name, it is another artifact
		// and we dont want to index it
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



	tIndex::const_iterator match = this->mIndex.find(symbol);
	if (match != this->mIndex.end()){
		assert(match->second.first && match->second.second && " found a wrong definition");
		return match->second;
	}
	return  voidPair;
}

////////////////////////////////////////////////
///
Indexer::TranslationUnitPair Indexer::getDefAndTUforDefinition (const clang::Decl* decl) const{
	assert(decl && "Cannot look up null pointer!");

	if(const clang::FunctionDecl* fd = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
		switch( fd->getTemplatedKind() ) {
			case clang::FunctionDecl::TemplatedKind::TK_NonTemplate:
				VLOG(2) << "TK_NonTemplate";
				VLOG(2) << buildNameTypeChain(fd);
				return getDefAndTUforDefinition(buildNameTypeChain(fd));
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplate:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_MemberSpecialization:
				VLOG(2) << "TK_MemberSpecialization";
				VLOG(2) << buildNameTypeChain(fd);
				VLOG(2) << buildNameTypeChain(fd->getMemberSpecializationInfo()->getInstantiatedFrom());
				//FIXME hack (for interception) to get correct translationunit for templatespecialization
				return TranslationUnitPair( { const_cast<clang::FunctionDecl*>(fd), getDefAndTUforDefinition(buildNameTypeChain(fd->getMemberSpecializationInfo()->getInstantiatedFrom())).second});
				//return getDefAndTUforDefinition(buildNameTypeChain(fd->getMemberSpecializationInfo()->getInstantiatedFrom()));
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplateSpecialization:
				VLOG(2) << "TK_FunctionTemplateSpecialization";
				if(const clang::FunctionTemplateSpecializationInfo* ti = fd->getTemplateSpecializationInfo() ) {

					VLOG(2) << ti->getTemplate()->getTemplatedDecl();
					VLOG(2) << "fd: " << buildNameTypeChain(fd);
				//	VLOG(2) << "templateDecl: " << buildNameTypeChain(ti->getTemplate()->getTemplatedDecl());
					VLOG(2) << "is explicit: " << ti->isExplicitSpecialization();
					return TranslationUnitPair( { const_cast<clang::FunctionDecl*>(fd), getDefAndTUforDefinition(buildNameTypeChain(ti->getTemplate()->getTemplatedDecl())).second});
				}
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
	return getDefAndTUforDefinition(decl).first;
}


////////////////////////////////////////////////
//
clang::Decl* Indexer::getMainFunctionDefinition () const{
	return getDefAndTUforDefinition("main").first;
}

////////////////////////////////////////////////
//
void Indexer::dump() const{
	tIndex::const_iterator it = mIndex.begin();
	tIndex::const_iterator end = mIndex.end();
	for (;it != end; it++){
		std::cout << "\t[" << it->first << " (" << it->second.first->getDeclKindName() << ") ," << it->second << "]" << std::endl;
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
