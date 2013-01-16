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
/*
	assert(currTU);
	const clang::SourceManager& srcMgr = const_cast<clang::idx::TranslationUnit*>(currTU)->getASTContext().getSourceManager();
			
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
	*/
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

void GlobalVarCollector::operator()(const Program::TranslationUnitSet& tus) {
	/*
	 * FIXME:  find a way with 3.2
	
	 const clang::idx::TranslationUnit* saveTU = currTU;

	std::for_each(tus.begin(), tus.end(), [&](const TranslationUnitPtr& cur) {
		currTU = Program::getClangTranslationUnit(*cur);
		assert(currTU);

		const clang::ASTContext& ctx = cur->getCompiler().getASTContext();

		clang::DeclContext* declCtx = clang::TranslationUnitDecl::castToDeclContext(ctx.getTranslationUnitDecl());

		std::for_each(declCtx->decls_begin(), declCtx->decls_end(), [&](clang::Decl* cur) {
				
				if(clang::VarDecl* vDecl = llvm::dyn_cast<clang::VarDecl>(cur)) {
					VisitExternVarDecl(vDecl);
				}

			});
	});

	currTU = saveTU;
	*/
}

void GlobalVarCollector::VisitExternVarDecl(clang::VarDecl* decl) {

	if (decl->hasExternalStorage()) return;

	//LOG(DEBUG) << "GLOBS: " << globals;
	//LOG(DEBUG) << "IdMap: " << varIdentMap;

	auto&& git = std::find_if(globals.begin(), globals.end(), 
		[&decl] (const VarDecl* cur) -> bool { 
			return decl->getNameAsString() == cur->getNameAsString(); 
		}  
		);

	if (git == globals.end()) { return; }

	varTU.erase( varTU.find( *git ) );
	globals.erase( git );

	globals.insert( decl );
	varTU.insert( std::make_pair(decl, currTU) );

	core::StringValuePtr&& ident = buildIdentifierFromVarDecl(decl);

	// Switch the value of the identifier for the variable already in the map to the
	// this varDecl because the fit is defined extern 
	for(GlobalIdentMap::iterator it = varIdentMap.begin(), end =varIdentMap.end(); it!=end; ++it) {
		if (it->first->getNameAsString() == decl->getNameAsString()) 
			it->second = ident;
	}

	varIdentMap.insert( std::make_pair(decl, ident) );
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

		assert(!funcStack.empty());
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
				auto saveFit = *fit;
				LOG(INFO) << saveFit->getNameAsString();

				globals.erase(fit);
				
				auto&& vit = varTU.find( saveFit );
				varTU.erase( vit );
	
				// Switch the value of the identifier for the variable already in the map to the
				// this varDecl because the fit is defined extern 
				for(GlobalIdentMap::iterator it = varIdentMap.begin(), end =varIdentMap.end(); it!=end; ++it) {
					if (it->first->getNameAsString() == saveFit->getNameAsString()) 
						it->second = ident;
				}
				
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
	/*FunctionDecl* funcDecl = callExpr->getDirectCallee();
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
	return true;
	*/
}


/// This function synthetized the global structure that will be used to hold the
/// global variables used within the functions of the input program.
GlobalVarCollector::GlobalStructPair GlobalVarCollector::createGlobalStruct()  {


	// no global variable found , we return an empty tuple
	if ( globals.empty() ) {
		return std::make_pair(core::StructTypePtr(), core::StructExprPtr());
	}

	const core::IRBuilder& builder = convFact.getIRBuilder();
	core::StructType::Entries entries;
	core::StructExpr::Members members;

	for ( auto it = globals.begin(), end = globals.end(); it != end; ++it ) {
		// get entry type and wrap it into a reference if necessary
		auto fit = varTU.find(*it);
		assert(fit != varTU.end());
		
		// In the case we have to resolve the initial value the current translation
		// unit has to be set properly
		convFact.setTranslationUnit(convFact.getProgram().getTranslationUnit( fit->second ) );
		
		core::StringValuePtr ident = varIdentMap.find( *it )->second;

		core::TypePtr&& type = convFact.convertType((*it)->getType().getTypePtr());

		// if ((*it)->getNameAsString() == "ompi_mpi_comm_world") {
		//	LOG(INFO) << "OK";
		//	type = builder.getLangBasic().getUnit();
		//}

		// If variable is marked to be volatile, make its tile volatile
		//auto&& vit1 = std::find(convFact.getVolatiles().begin(), convFact.getVolatiles().end(), *it);
	   	//if(vit1 != convFact.getVolatiles().end() ||
	   	if((*it)->getType().isVolatileQualified()) {
			///[>*************************************
			//// X-MASS2011 - HACK
			///[>*************************************
			type = builder.volatileType( type );
		}

		if ( (*it)->hasExternalStorage() ) {
			// the variable is defined as extern, so we don't have to allocate memory
			// for it just refer to the memory location someone else has initialized
			type = builder.refType( type );
		}


		// add type to the global struct
		entries.push_back( builder.namedType( ident, type ) );
		// add initialization
		varIdentMap.insert( std::make_pair(*it, ident) ); 

		// we have to initialize the value of this ref with the value of the extern
		// variable which we assume will be visible from the entry point
		core::ExpressionPtr initExpr;
		if( (*it)->hasExternalStorage() ) {
			assert (type->getNodeType() == core::NT_RefType);
			// core::TypePtr derefTy = core::static_pointer_cast<const core::RefType>( type )->getElementType();
			// build a literal which points to the name of the external variable 
			initExpr = builder.literal((*it)->getNameAsString(), type);
		} else {
			// this means the variable is not declared static inside a function so we have to initialize its value
			initExpr = (*it)->getInit() ? 
				convFact.convertInitExpr(NULL, (*it)->getInit(), type, false) : 
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
