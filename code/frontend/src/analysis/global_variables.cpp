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

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"
#include "clang/Index/Program.h"
#include "clang/Index/TranslationUnit.h"

#include "clang/Basic/FileManager.h"

#include "clang/AST/VTableBuilder.h"

using namespace clang;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::NamedDecl* decl) {
	return out << decl->getNameAsString();
}
std::ostream& operator<<(std::ostream& out, const insieme::frontend::analysis::GlobalVarCollector& globals) {
	globals.dump(out);
	return out;
}
std::ostream& operator<<(std::ostream& out, const insieme::frontend::analysis::GlobalVarCollector::GlobalIdentMap::value_type& ident) {
	return out << *ident.second;
}
} // end std namespace 

namespace insieme {
namespace frontend {
namespace analysis {

core::StringValuePtr
GlobalVarCollector::buildIdentifierFromVarDecl( clang::VarDecl* varDecl, const clang::FunctionDecl* func ) const {

	const clang::SourceManager& srcMgr = 
				const_cast<clang::idx::TranslationUnit*>(currTU)->getASTContext().getSourceManager();
			
	FileID&& fileId = srcMgr.getMainFileID();
	const clang::FileEntry* fileEntry = srcMgr.getFileEntryForID(fileId);

	int s = (int) *fileEntry->getName();
	std::ostringstream ss;
	// if the file name starts with a number, we add an underscore to make it a valid C literal
	if (s>=48 && s<=57) { ss << '_'; }

	ss << fileEntry->getName();
	if (func) {	ss << "_" << func->getNameAsString(); }
	ss << "_" << varDecl->getNameAsString();

	std::string&& str = ss.str();
	for(auto it=str.begin(), end=str.end(); it!=end; ++it) {
		int val = (int) *it;
		if ( (val < 48 || val > 57) && (val < 65 || val > 90) && (val < 97 || val > 122) && val != 95) {
			// this is not a valid C identifier, therefore we need to replace this character 
			*it = '_';
		}
	}
	return convFact.getIRBuilder().stringValue( str );
}

void GlobalVarCollector::operator()(const clang::Decl* decl) {
	bool isFuncDecl = false;
	if(const clang::FunctionDecl* funcDecl = dyn_cast<const clang::FunctionDecl>(decl)) {
		if(visited.find(funcDecl) != visited.end())
			return; // function declaration already visited
		visited.insert(funcDecl);
		isFuncDecl = true;
		funcStack.push(funcDecl);
	}

	TraverseDecl(const_cast<clang::Decl*>(decl));

	if(isFuncDecl) { funcStack.pop(); }
}

// needed to check for the use of global variables in expressions inside pragmas
bool GlobalVarCollector::VisitStmt(clang::Stmt* stmt) {
    // check if there is a datarange pragma
    const frontend::pragma::PragmaStmtMap::StmtMap& pragmaStmtMap = convFact.getPragmaMap().getStatementMap();
    std::pair<frontend::pragma::PragmaStmtMap::StmtMap::const_iterator, frontend::pragma::PragmaStmtMap::StmtMap::const_iterator> iter =
    		pragmaStmtMap.equal_range(stmt);

    // if a datarange pragma is found, check if there are global variables used inside the expressions of the pragma
    std::for_each(iter.first, iter.second,
        [ & ](const frontend::pragma::PragmaStmtMap::StmtMap::value_type& curr){
        const frontend::InsiemeDatarange* dr = dynamic_cast<const frontend::InsiemeDatarange*>( &*(curr.second) );
        if(dr) {

			pragma::MatchMap mmap = dr->getMatchMap();

			auto ranges = mmap.find("ranges");
			if(ranges == mmap.end())
				return;


			for(auto I = ranges->second.begin(); I != ranges->second.end(); ++I){
				clang::Stmt* token = (*I)->get<clang::Stmt*>();

				this->TraverseStmt(token);

			}
		}
    });


    return this->clang::RecursiveASTVisitor<GlobalVarCollector>::VisitStmt(stmt);
}

bool GlobalVarCollector::VisitVarDecl(clang::VarDecl* decl) {
	if(decl->hasGlobalStorage()) {
		globals.insert( decl );
		varTU.insert( std::make_pair(decl, currTU) );

		const FunctionDecl* enclosingFunc = funcStack.top();
		assert(enclosingFunc);
		usingGlobals.insert(enclosingFunc); // the enclosing function uses globals

		auto&& fit = varIdentMap.find(decl);
		assert(fit == varIdentMap.end() && "Variable already declared");

		varIdentMap.insert( std::make_pair(decl, buildIdentifierFromVarDecl(decl, enclosingFunc)) );
	}
	return true;
}

bool GlobalVarCollector::VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
	if(VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
		if( !varDecl->hasGlobalStorage() ) { return true; }

		const FunctionDecl* enclosingFunc = funcStack.top();
		assert(enclosingFunc);
		usingGlobals.insert(enclosingFunc); // the enclosing function uses globals

		core::StringValuePtr&& ident = buildIdentifierFromVarDecl(varDecl);

		auto&& fit = std::find_if(globals.begin(), globals.end(), 
				[&varDecl] (const VarDecl* cur) -> bool { return varDecl->getNameAsString() == cur->getNameAsString(); }  
			);
		// add the variable to the list of global vars (if not already there)
		if(fit == globals.end()) {
			globals.insert( varDecl );
			auto ret = varTU.insert( std::make_pair(varDecl, currTU) );
			assert(ret.second && "Variable name already exists within the list of global variables.");
			
			varIdentMap.insert( std::make_pair(varDecl, ident) );
		} else {
			//it could be that a variable is already in the list of globals with an external storage
			//specifier and we encounter the global declaration in another translation unit, in that
			//case we have to replace the collected VarDecl with this new instance 
			if ( !varDecl->hasExternalStorage() && (*fit)->hasExternalStorage() ) {
				// do replace
				globals.erase(fit);
				
				auto&& vit = varTU.find( *fit );
				varTU.erase( vit );
	
				// Switch the value of the identifier for the variable already in the map to the
				// this varDecl because the fit is defined extern 
				auto&& iit = varIdentMap.find( *fit );
				iit->second = ident;
				
				globals.insert( varDecl );
				varTU.insert( std::make_pair(varDecl, currTU) );

				varIdentMap.insert( std::make_pair(varDecl, ident) );
			} else {
				auto&& iit = varIdentMap.find( *fit );
				assert( iit != varIdentMap.end() );
				varIdentMap.insert( std::make_pair(varDecl, iit->second ) );
			}
		}

	}
	return true;
}

bool GlobalVarCollector::VisitCallExpr(clang::CallExpr* callExpr) {
	FunctionDecl* funcDecl = callExpr->getDirectCallee();
	const FunctionDecl *definition = NULL;

	// save the translation unit for the current function
	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {
		/*******************************************************************************************
		 * if the function is not defined in this translation unit, maybe it is defined in another
		 * we already loaded  use the clang indexer to lookup the definition for this function
		 * declarations
		 ******************************************************************************************/
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		/*
		 * if the called function access the global data structure also the current function
		 * has to be marked (otherwise the global structure will not correctly forwarded)
		 */
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;
	return true;
}

bool GlobalVarCollector::VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr) {
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
		/*******************************************************************************************
		 * if the function is not defined in this translation unit, maybe it is defined in another
		 * we already loaded  use the clang indexer to lookup the definition for this function
		 * declarations
		 ******************************************************************************************/
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		/*
		 * if the called function access the global data structure also the current function
		 * has to be marked (otherwise the global structure will not correctly forwarded)
		 */
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	return true;
}

bool GlobalVarCollector::VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) {
	Expr* 		 callee = callExpr->getCallee()->IgnoreParens();
	MemberExpr* 	 memberExpr = cast<MemberExpr>(callee);
	CXXMethodDecl* methodDecl = cast<CXXMethodDecl>(memberExpr->getMemberDecl());

	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(methodDecl);
	const FunctionDecl *definition = NULL;

	// save the translation unit for the current function
	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {
		/*******************************************************************************************
		 * if the function is not defined in this translation unit, maybe it is defined in another
		 * we already loaded  use the clang indexer to lookup the definition for this function
		 * declarations
		 ******************************************************************************************/
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

		/*
		 * if the called function access the global data structure also the current function
		 * has to be marked (otherwise the global structure will not correctly forwarded)
		 */
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	return true;
}

bool GlobalVarCollector::VisitCXXDeleteExpr(clang::CXXDeleteExpr* deleteExpr) {
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
		/*******************************************************************************************
		 * if the function is not defined in this translation unit, maybe it is defined in another
		 * we already loaded  use the clang indexer to lookup the definition for this function
		 * declarations
		 ******************************************************************************************/
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

		/*
		 * if the called function access the global data structure also the current function
		 * has to be marked (otherwise the global structure will not correctly forwarded)
		 */
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	return true;
}

bool GlobalVarCollector::VisitCXXNewExpr(clang::CXXNewExpr* newExpr) {

	//check if allocated type is builtin
	if( newExpr->getAllocatedType().getTypePtr()->isBuiltinType() ) {
		//if -> nothing to be done
		return true;
	}

	CXXRecordDecl* recDecl = newExpr->getConstructor()->getParent();
	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(newExpr->getConstructor());
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
		/*******************************************************************************************
		 * if the function is not defined in this translation unit, maybe it is defined in another
		 * we already loaded  use the clang indexer to lookup the definition for this function
		 * declarations
		 ******************************************************************************************/
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		/*
		 * if the called function access the global data structure also the current function
		 * has to be marked (otherwise the global structure will not correctly forwarded)
		 */
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;
	return true;
}

bool GlobalVarCollector::VisitCXXConstructExpr(clang::CXXConstructExpr* ctorExpr) {
	FunctionDecl* funcDecl = dynamic_cast<FunctionDecl*>(ctorExpr->getConstructor());
	const FunctionDecl *definition = NULL;

	CXXRecordDecl* recDecl = ctorExpr->getConstructor()->getParent();

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
		/*******************************************************************************************
		 * if the function is not defined in this translation unit, maybe it is defined in another
		 * we already loaded  use the clang indexer to lookup the definition for this function
		 * declarations
		 ******************************************************************************************/
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		/*
		 * if the called function access the global data structure also the current function
		 * has to be marked (otherwise the global structure will not correctly forwarded)
		 */
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}
	// reset the translation unit to the previous one
	currTU = old;

	return true;
}

// prepare vTable and offsetTable and necessary maps
void GlobalVarCollector::collectVTableData(const clang::CXXRecordDecl* recDecl) {


	if( polymorphicClassMap.find(recDecl) == polymorphicClassMap.end() ) {
		//recDecl not yet collected
		clang::VTableContext vTableContext(recDecl->getASTContext());

		if( VLOG_IS_ON(2) ) {
			VLOG(2) << "CLANG -- VTableLayout";
			VLOG(2) << "	class: " << recDecl->getNameAsString() << ", # of vtable components: "<<  vTableContext.getVTableLayout(recDecl).getNumVTableComponents();
			for(VTableLayout::vtable_component_iterator it = vTableContext.getVTableLayout(recDecl).vtable_component_begin();
					it != vTableContext.getVTableLayout(recDecl).vtable_component_end(); it++) {
				switch(it->getKind()) {

					case clang::VTableComponent::CK_VCallOffset:
						VLOG(2) << "		VCallOffset" << it->getVCallOffset().getQuantity(); break;
					case clang::VTableComponent::CK_VBaseOffset:
						VLOG(2) << "		VBaseOffset" << it->getVBaseOffset().getQuantity(); break;
					case clang::VTableComponent::CK_OffsetToTop:
						VLOG(2) << "		OffsetToTop:" << it->getOffsetToTop().getQuantity(); break;
					case clang::VTableComponent::CK_RTTI:
						VLOG(2) << "		RTTI: " << it->getRTTIDecl()->getNameAsString(); break;
					case clang::VTableComponent::CK_FunctionPointer:
						VLOG(2) << "		FunctionPointer: "<< it->getFunctionDecl()->getParent()->getNameAsString() << "::" << it->getFunctionDecl()->getNameAsString(); break;
							// CK_CompleteDtorPointer - A pointer to the complete destructor.
					case clang::VTableComponent::CK_CompleteDtorPointer:
						VLOG(2) << "		CompleteDtorPointer: "<< it->getDestructorDecl()->getParent()->getNameAsString() << "::" << it->getDestructorDecl()->getNameAsString(); break;
						    // CK_DeletingDtorPointer - A pointer to the deleting destructor.
					case clang::VTableComponent::CK_DeletingDtorPointer:
						VLOG(2) << "		DeletingDtorPointer: "<< it->getDestructorDecl()->getParent()->getNameAsString() << "::" << it->getDestructorDecl()->getNameAsString(); break;
						    // CK_UnusedFunctionPointer - In some cases, a vtable function pointer
						    // will end up never being called. Such vtable function pointers are
						    // represented as a CK_UnusedFunctionPointer.
					case clang::VTableComponent::CK_UnusedFunctionPointer:
						VLOG(2) << "		UnusedFunctionPointer: "<< it->getUnusedFunctionDecl()->getParent()->getNameAsString() << "::" << it->getUnusedFunctionDecl()->getNameAsString(); break;
					default:
						VLOG(2) << "		" << it->getKind();
				}
			}
		}

		//add polymorphic class to class map, use classIdCounter
		polymorphicClassMap.insert( std::make_pair( recDecl, std::make_pair( polymorphicClassMap.size(), vTableContext.getNumVirtualFunctionPointers(recDecl)) ) );
		VLOG(2) << "polymorphicClassMap[class=(classId, vFuncCount)]: " <<  polymorphicClassMap;

		//collect offset
		int offsetCounter = 0;
		VLOG(2) << "Offset for " << recDecl->getNameAsString() << ":" << recDecl->getNameAsString() << " " << offsetCounter;
		offsetMap.insert( std::make_pair( std::make_pair(recDecl, recDecl), offsetCounter) );
		offsetCounter += vTableContext.getNumVirtualFunctionPointers(recDecl);
		VLOG(2) << "offsetMap: " << offsetMap;

		vector<CXXRecordDecl*> dynamicBases = getAllDynamicBases(recDecl);
		VLOG(2) << "Dynamic bases of " << recDecl->getNameAsString() << " " << dynamicBases;
		VLOG(2) << recDecl->getNameAsString() << " number of vfunc pointers:" << vTableContext.getNumVirtualFunctionPointers(recDecl);

		for(vector<CXXRecordDecl*>::iterator it = dynamicBases.begin(); it!=dynamicBases.end(); it++) {
			VLOG(2) << (*it)->getNameAsString() << " number of vfunc pointers:" << vTableContext.getNumVirtualFunctionPointers(*it);

			//collect offset from recDecl to dynamicBases
			VLOG(2) << "Offset for " <<  recDecl->getNameAsString() << " to Base " << (*it)->getNameAsString() << ":  " << recDecl->getNameAsString() << ":" << (*it)->getNameAsString() << " " << offsetCounter;
			offsetMap.insert( std::make_pair( std::make_pair(recDecl, (*it)), offsetCounter) );
			offsetCounter += vTableContext.getNumVirtualFunctionPointers(*it);
			VLOG(2) << "offsetMap: " << offsetMap;

			//recursively collect data of all dynamic baseClasses
			collectVTableData(*it);
		}

		// store max count of virtual functions to build vtable correctly
		maxFunctionCounter = maxFunctionCounter < offsetCounter ? offsetCounter : maxFunctionCounter;

		//get vFuncIds for methods of recDecl
		for(clang::CXXRecordDecl::method_iterator mit = recDecl->method_begin(); mit != recDecl->method_end(); mit++) {
			clang::CXXMethodDecl* decl = *mit;
			VLOG(2) << decl->getParent()->getNameAsString() << "::" << decl->getNameAsString() << " isVirtual: " << decl->isVirtual();

			if(decl->isVirtual() ) {

				if(const clang::CXXDestructorDecl* dtorDecl = dynamic_cast<CXXDestructorDecl*>(decl)) {
					GlobalDecl completeDtor = clang::GlobalDecl(dtorDecl, Dtor_Complete);
					VLOG(2) << dtorDecl->getParent()->getNameAsString() << "::" << dtorDecl->getNameAsString() << " isVirtual: " << dtorDecl->isVirtual() << " vTableIndex: " << vTableContext.getMethodVTableIndex(completeDtor);
					GlobalDecl deletingDtor = clang::GlobalDecl(dtorDecl, Dtor_Deleting);
					VLOG(2) << dtorDecl->getParent()->getNameAsString() << "::" << dtorDecl->getNameAsString() << " isVirtual: " << dtorDecl->isVirtual() << " vTableIndex: " << vTableContext.getMethodVTableIndex(deletingDtor);

					if( virtualFunctionIdMap.find(decl) == virtualFunctionIdMap.end() ) {
						// FIXME which DTOR to use? complete or deleting???
						virtualFunctionIdMap.insert( std::make_pair( decl, vTableContext.getMethodVTableIndex(completeDtor) ) );
						//virtualFunctionIdMap.insert( std::make_pair( decl, vTableContext.getMethodVTableIndex(deletingDtor) ) );
					}
				} else {
					VLOG(2) << decl->getParent()->getNameAsString() << "::" << decl->getNameAsString() << " isVirtual: " << decl->isVirtual() << " vTableIndex: " << vTableContext.getMethodVTableIndex(decl);
					if( virtualFunctionIdMap.find(decl) == virtualFunctionIdMap.end() ) {
						virtualFunctionIdMap.insert( std::make_pair( decl, vTableContext.getMethodVTableIndex(decl) ) );
					}
				}
				VLOG(2) << "virtualFunctionIdMap[method=(methodid)]: " <<  virtualFunctionIdMap;
			}
		}

		// get overriders of the methods of recDecl
		//	list of pairs: pair(virtual function to be overriden, final overrider)
		vector< std::pair< const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>> finalOverriders;
		CXXFinalOverriderMap clangFinalOverriderMap;
		recDecl->getFinalOverriders(clangFinalOverriderMap);
		for(auto it = clangFinalOverriderMap.begin(); it != clangFinalOverriderMap.end();it++) {
			for(auto oit = it->second.begin();oit!=it->second.end();oit++) {
				for(auto uit = oit->second.begin();uit!=oit->second.end();uit++) {
					VLOG(2) << it->first->getParent()->getNameAsString() << "::" << it->first->getNameAsString() << \
					" final overriden by " << uit->Method->getParent()->getNameAsString() << "::" << uit->Method->getNameAsString();

					finalOverriders.push_back( std::make_pair(it->first, uit->Method) );
				}
			}
		}
		//add list of final overriders to Context::finalOverriderMap (Class, finalOverriders of class)
		finalOverriderMap.insert( std::make_pair(recDecl, finalOverriders) );
	}
}


// Returns all bases of a c++ record declaration
vector<CXXRecordDecl*> GlobalVarCollector::getAllDynamicBases(const clang::CXXRecordDecl* recDeclCXX ){
	vector<CXXRecordDecl*> bases;

	for(CXXRecordDecl::base_class_const_iterator bit=recDeclCXX->bases_begin(),
			bend=recDeclCXX->bases_end(); bit != bend; ++bit) {
		const CXXBaseSpecifier * base = bit;
		CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

		if(baseRecord->isDynamicClass() ) {
//			VLOG(2) << "dynamic base: " << baseRecord->getNameAsString();

			bases.push_back(baseRecord);

			vector<CXXRecordDecl*> subBases = getAllDynamicBases(dyn_cast<clang::CXXRecordDecl>(baseRecord));
			bases.insert(bases.end(), subBases.begin(), subBases.end());
		}
	}
	return bases;
}

/*
 * This function synthetized the global structure that will be used to hold the
 * global variables used within the functions of the input program.
 */
GlobalVarCollector::GlobalStructPair GlobalVarCollector::createGlobalStruct()  {


	// no global variable AND NO POLYMORPHIC CLASS found , we return an empty tuple
	if ( globals.empty() && polymorphicClassMap.empty()) {
		return std::make_pair(core::StructTypePtr(), core::StructExprPtr());
	}

	const core::IRBuilder& builder = convFact.getIRBuilder();
	core::StructType::Entries entries;
	core::StructExpr::Members members;

	// polymorphic class used -> add virtual function table to global struct
	if(!polymorphicClassMap.empty()) {
		core::StringValuePtr ident;
		core::TypePtr type;

		//count of polymorphicClasses=polymorphicClassMap.size, maxFunctionCounter=max # of Vfunc in a polymorphicClass
		ident = builder.stringValue("__vfunc_table");
		type =	builder.vectorType(
					builder.vectorType(
							builder.getLangBasic().getAnyRef(),
							core::ConcreteIntTypeParam::get(builder.getNodeManager(), maxFunctionCounter)
					),
					core::ConcreteIntTypeParam::get(builder.getNodeManager(), polymorphicClassMap.size())
				);
		entries.push_back( builder.namedType( ident, type ));

		// virtual function offset (row = actual objectType (use classId stored in object), col = type of pointer)
		ident = builder.stringValue("__vfunc_offset");
		type =	builder.vectorType(
					builder.vectorType(
						builder.getLangBasic().getInt4(),
						core::ConcreteIntTypeParam::get(builder.getNodeManager(), polymorphicClassMap.size())
					),
					core::ConcreteIntTypeParam::get(builder.getNodeManager(), polymorphicClassMap.size())
				);
		entries.push_back( builder.namedType( ident, type ));

		// default initialization of __vfunc_offset to -1 !!!
		core::ExpressionPtr initExpr =	builder.vectorInit(
						builder.vectorInit(
								builder.literal("-1", builder.getLangBasic().getInt4()),
								core::ConcreteIntTypeParam::get(builder.getNodeManager(), polymorphicClassMap.size())),
						core::ConcreteIntTypeParam::get(builder.getNodeManager(), polymorphicClassMap.size())
					);
		core::NamedValuePtr member = builder.namedValue(ident, initExpr);

		members.push_back( member );
	}

	for ( auto it = globals.begin(), end = globals.end(); it != end; ++it ) {
		// get entry type and wrap it into a reference if necessary
		auto fit = varTU.find(*it);
		assert(fit != varTU.end());
		/*
		 * In the case we have to resolve the initial value the current translation
		 * unit has to be set properly
		 */
		convFact.setTranslationUnit(convFact.getProgram().getTranslationUnit( fit->second ) );
		
		core::StringValuePtr ident = varIdentMap.find( *it )->second;

		core::TypePtr&& type = convFact.convertType((*it)->getType().getTypePtr());

		// If variable is marked to be volatile, make its tile volatile
		//auto&& vit1 = std::find(convFact.getVolatiles().begin(), convFact.getVolatiles().end(), *it);
	   	if(/*vit1 != convFact.getVolatiles().end() ||*/ (*it)->getType().isVolatileQualified()) {
			///[>*************************************
			//// X-MASS2011 - HACK
			///[>*************************************
			type = builder.volatileType( type );
		}

		if ( (*it)->hasExternalStorage() ) {
			/*
			 * the variable is defined as extern, so we don't have to allocate memory
			 * for it just refer to the memory location someone else has initialized
			 */
			type = builder.refType( type );
		}


		// add type to the global struct
		entries.push_back( builder.namedType( ident, type ) );
		// add initialization
		varIdentMap.insert( std::make_pair(*it, ident) ); 

		/*
		 * we have to initialize the value of this ref with the value of the extern
		 * variable which we assume will be visible from the entry point
		 */
		core::ExpressionPtr initExpr;
		if( (*it)->hasExternalStorage() ) {
			assert (type->getNodeType() == core::NT_RefType);
			core::TypePtr derefTy = core::static_pointer_cast<const core::RefType>( type )->getElementType();
			// build a literal which points to the name of the external variable 
			initExpr = builder.refVar( builder.literal((*it)->getNameAsString(), derefTy) );
		} else {
			// this means the variable is not declared static inside a function so we have to initialize its value
			initExpr = (*it)->getInit() ? 
				convFact.convertInitExpr((*it)->getInit(), type, false) : 
				convFact.defaultInitVal(type);
		}
		// default initialization
		core::NamedValuePtr member = builder.namedValue(ident, initExpr);
		
		// annotate if omp threadprivate
		auto&& vit = std::find(convFact.getThreadprivates().begin(), convFact.getThreadprivates().end(), *it);
		if(vit != convFact.getThreadprivates().end()) {
			omp::addThreadPrivateAnnotation(member);
		}

		members.push_back( member );

	}

	VLOG(1) << "Building '__insieme_globals' data structure";
	core::StructTypePtr&& structTy = builder.structType(entries);
	// we name this structure as '__insieme_globals'
	structTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(std::string("__insieme_globals")) );
	// set back the original TU
	assert(currTU && "Lost reference to the translation unit");
	convFact.setTranslationUnit(convFact.getProgram().getTranslationUnit(currTU));

	return std::make_pair(structTy, builder.structExpr(structTy, members) );
}

void GlobalVarCollector::dump(std::ostream& out) const {
	out << std::endl << 
		   "// ~~~~~~~~ GLOBAL VAR MAP ~~~~~~~~ //" 	<< std::endl;
	out << "  SIZE:      " << globals.size()  	 		<< std::endl;
	out << "  GLOBALS:   " << join(", ", globals) 		<< std::endl;
	out << "  NAMES:     " << join(", ", varIdentMap)   << std::endl;
	out << "  FUNCTIONS: " << join(" ,", usingGlobals) 	<< std::endl;
	out << "// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //";
}

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace
