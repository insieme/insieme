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

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/frontend/omp/omp_pragma.h"

#include "insieme/frontend/analysis/global_variables.h"

#include "insieme/frontend/convert.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"

#include "insieme/annotations/c/naming.h"

// [3.0]
//#include "clang/Index/Entity.h"
//#include "clang/Index/Indexer.h"
//#include "clang/Index/Program.h"
//#include "clang/Index/TranslationUnit.h"

#include "clang/Basic/FileManager.h"

#include "clang/AST/VTableBuilder.h"

using namespace clang;


namespace insieme {
namespace frontend {
namespace analysis {


bool CXXGlobalVarCollector::VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr) {
	/*
//	Expr* 		 callee = callExpr->getCallee()->IgnoreParens();
//	MemberExpr* 	 memberExpr = cast<MemberExpr>(callee);
//	CXXMethodDecl* methodDecl = cast<CXXMethodDecl>(memberExpr->getMemberDecl());

	FunctionDecl* funcDecl;
	if( CXXMethodDecl* methodDecl = dyn_cast<CXXMethodDecl>(callExpr->getCalleeDecl()) ) {
		//operator defined as member function
		funcDecl = dyn_cast<FunctionDecl>(methodDecl);

		//if virtual function call -> add the enclosing function to usingGlobals
		if( methodDecl->isVirtual() ) {
			//enclosing function needs access to globals as virtual function tables are stored as global variable
			VLOG(2) << "possible virtual call " << methodDecl->getParent()->getNameAsString() << "->" << methodDecl->getNameAsString();
			usingGlobals.insert( funcStack.top() );
		}
	} else {
		//operator defined as non-member function
		funcDecl = dyn_cast<clang::FunctionDecl>(callExpr->getCalleeDecl());
	}

	const FunctionDecl *definition = NULL;

	// save the translation unit for the current function
	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {

		// if the function is not defined in this translation unit, maybe it is defined in another
		// we already loaded  use the clang indexer to lookup the definition for this function
		// declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		// if the called function access the global data structure also the current function
		// has to be marked (otherwise the global structure will not correctly forwarded)
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	*/
	return true;
}

bool CXXGlobalVarCollector::VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) {
	/*
	Expr* 		 callee = callExpr->getCallee()->IgnoreParens();
	MemberExpr* 	 memberExpr = cast<MemberExpr>(callee);
	CXXMethodDecl* methodDecl = cast<CXXMethodDecl>(memberExpr->getMemberDecl());

	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(methodDecl);
	const FunctionDecl *definition = NULL;

	// save the translation unit for the current function
	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {

		// if the function is not defined in this translation unit, maybe it is defined in another
		// we already loaded  use the clang indexer to lookup the definition for this function
		// declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	//if virtual function call -> add the enclosing function to usingGlobals
	if( methodDecl->isVirtual() ) {
		collectVTableData(methodDecl->getParent());

		//enclosing function needs access to globals as virtual function tables are stored as global variable
		VLOG(2) << "possible virtual call " << methodDecl->getParent()->getNameAsString() << "->" << methodDecl->getNameAsString();
		usingGlobals.insert( funcStack.top() );
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		// if the called function access the global data structure also the current function
		// has to be marked (otherwise the global structure will not correctly forwarded)
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	*/
	return true;
}

bool CXXGlobalVarCollector::VisitCXXDeleteExpr(clang::CXXDeleteExpr* deleteExpr) {
	/*
	if(!deleteExpr->getDestroyedType().getTypePtr()->isStructureOrClassType()) {
		//for non struct/class types (--> builtin) nothing to do
		return true;
	}

	//we have a delete for a class/struct type
	//get the destructor decl
	CXXRecordDecl* classDecl = deleteExpr->getDestroyedType()->getAsCXXRecordDecl();
	CXXDestructorDecl* dtorDecl = classDecl->getDestructor();

	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(dtorDecl);
	const FunctionDecl *definition = NULL;

	// save the translation unit for the current function
	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {

		// if the function is not defined in this translation unit, maybe it is defined in another
		// we already loaded  use the clang indexer to lookup the definition for this function
		// declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	//if virtual dtor call -> add the enclosing function to usingGlobals
	if( dtorDecl->isVirtual() ) {
		collectVTableData(dtorDecl->getParent());

		//enclosing function needs access to globals as virtual function tables are stored as global variable
		VLOG(2) << "possible virtual call " << dtorDecl->getParent()->getNameAsString() << "->" << dtorDecl->getNameAsString();
		usingGlobals.insert( funcStack.top() );
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		// if the called function access the global data structure also the current function
		// has to be marked (otherwise the global structure will not correctly forwarded)
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	*/
	return true;
}

bool CXXGlobalVarCollector::VisitCXXNewExpr(clang::CXXNewExpr* newExpr) {

	/*

	//check if allocated type is builtin
	if( newExpr->getAllocatedType().getTypePtr()->isBuiltinType() ) {
		//if -> nothing to be done
		return true;
	}

	CXXConstructorDecl* ctorDecl = newExpr->getConstructor();
	CXXRecordDecl* recDecl = ctorDecl->getParent();
	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(ctorDecl);
	const FunctionDecl *definition = NULL;

	if( recDecl->isPolymorphic() ) {
		collectVTableData(recDecl);

		// go through virtual functions and check them for globals/virtual function calls
		for(clang::CXXRecordDecl::method_iterator mit = recDecl->method_begin(); mit != recDecl->method_end(); mit++) {
			if( mit->isVirtual() ) {
				FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(*mit);
				funcStack.push(funcDecl);
				(*this)(funcDecl);
				funcStack.pop();
			}
		}
	}

	// save the translation unit for the current function
	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {

		// if the function is not defined in this translation unit, maybe it is defined in another
		// we already loaded  use the clang indexer to lookup the definition for this function
		// declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	// handle initializers
	for (clang::CXXConstructorDecl::init_iterator iit =
			ctorDecl->init_begin(), iend =
			ctorDecl->init_end(); iit != iend; iit++) {
		clang::CXXCtorInitializer* initializer = *iit;
		this->TraverseStmt(initializer->getInit());

		// check if the current initializer is a CXXConstructExpr
		// -> using a ctor to initializer a class member
		if( const CXXConstructExpr *initCtor = dyn_cast<CXXConstructExpr>(initializer->getInit()) ) {
			//if this ctor is using globalVars add the surrounding ctor to usingGlobals
			if(usingGlobals.find(initCtor->getConstructor()) != usingGlobals.end()) {
				usingGlobals.insert( definition );
			}
		}
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		// if the called function access the global data structure also the current function
		// has to be marked (otherwise the global structure will not correctly forwarded)
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;
	*/
	return true;
}

bool CXXGlobalVarCollector::VisitCXXConstructExpr(clang::CXXConstructExpr* ctorExpr) {
	CXXConstructorDecl* ctorDecl = ctorExpr->getConstructor();
	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(ctorDecl);
	const FunctionDecl *definition = NULL;

	CXXRecordDecl* recDecl = ctorExpr->getConstructor()->getParent();

	if( recDecl->isPolymorphic() ) {
		// go through virtual functions and check them for globals/virtual function calls
		for(clang::CXXRecordDecl::method_iterator mit = recDecl->method_begin(); mit != recDecl->method_end(); mit++) {
			if( mit->isVirtual() ) {
				FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(*mit);
				funcStack.push(funcDecl);
				(*this)(funcDecl);
				funcStack.pop();
			}
		}
	}

	std::pair<clang::Decl*, insieme::frontend::TranslationUnit*> ret;
	if(!funcDecl->hasBody(definition)) {
		// if the function is not defined in this translation unit, maybe it is defined in another
		// we already loaded  use the clang indexer to lookup the definition for this function
		// declarations
		ret = indexer.getDefAndTUforDefinition(funcDecl);
		if (!ret.first)
			return true;
		definition = llvm::cast<FunctionDecl>(ret.first);
	}

	// handle initializers
	for (clang::CXXConstructorDecl::init_iterator iit =
			ctorDecl->init_begin(), iend =
			ctorDecl->init_end(); iit != iend; iit++) {
		clang::CXXCtorInitializer* initializer = *iit;
		this->TraverseStmt(initializer->getInit());

		// check if the current initializer is a CXXConstructExpr
		// -> using a ctor to initializer a class member
		if( const CXXConstructExpr *initCtor = dyn_cast<CXXConstructExpr>(initializer->getInit()) ) {
			//if this ctor is using globalVars add the surrounding ctor to usingGlobals
			if(usingGlobals.find(initCtor->getConstructor()) != usingGlobals.end()) {
				usingGlobals.insert( definition );
			}
		}
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		// if the called function access the global data structure also the current function
		// has to be marked (otherwise the global structure will not correctly forwarded)
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}

	return true;
}

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace
