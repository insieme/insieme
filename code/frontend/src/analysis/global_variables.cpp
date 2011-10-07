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

#include "insieme/frontend/analysis/global_variables.h"

#include "insieme/frontend/convert.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"

#include "insieme/annotations/c/naming.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"
#include "clang/Index/Program.h"

using namespace clang;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::NamedDecl* decl) {
	return out << decl->getNameAsString();
}
std::ostream& operator<<(std::ostream& out, const insieme::frontend::analysis::GlobalVarCollector& globals) {
	globals.dump(out);
	return out;
}

}

namespace insieme {
namespace frontend {
namespace analysis {

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
	if(isFuncDecl)
		funcStack.pop();
}

bool GlobalVarCollector::VisitVarDecl(clang::VarDecl* decl) {
	if(decl->hasGlobalStorage()) {
		globals.insert( decl );
		varTU.insert( std::make_pair(decl, currTU) );

		const FunctionDecl* enclosingFunc = funcStack.top();
		assert(enclosingFunc);
		usingGlobals.insert(enclosingFunc); // the enclosing function uses globals
	}
	return true;
}

bool GlobalVarCollector::VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
	if(VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
		if( !varDecl->hasGlobalStorage() ) { return true; }

		const FunctionDecl* enclosingFunc = funcStack.top();
		assert(enclosingFunc);
		usingGlobals.insert(enclosingFunc); // the enclosing function uses globals

		auto&& fit = globals.find(varDecl);
		// add the variable to the list of global vars (if not already there)
		if(fit == globals.end()) {
			globals.insert( varDecl );
			varTU.insert( std::make_pair(varDecl, currTU) );
		} else {
			//it could be that a variable is already in the list of globals with an external storage
			//specifier and we encounter the global declaration in another translation unit, in that
			//case we have to replace the collected VarDecl with this new instance 
			if ( !varDecl->hasExternalStorage() && (*fit)->hasExternalStorage() ) {
				// do replace
				globals.erase(fit);
				
				auto&& vit = varTU.find( *fit );
				varTU.erase( vit );

				globals.insert(varDecl);
				varTU.insert( std::make_pair(varDecl, currTU) );
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
		/*
		 * if the function is not defined in this translation unit, maybe it is
		 * defined in another we already loaded  use the clang indexer to lookup
		 * the definition for this function declarations
		 */
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

/*
 * This function synthetized the global structure that will be used to hold the
 * global variables used within the functions of the input program.
 */
GlobalVarCollector::GlobalStructPair GlobalVarCollector::createGlobalStruct(conversion::ConversionFactory& fact)  {
	// no global variable found, we return an empty tuple
	if ( globals.empty() ) {
		return std::make_pair(core::StructTypePtr(), core::StructExprPtr());
	}

	const core::ASTBuilder& builder = fact.getASTBuilder();
	core::StructType::Entries entries;
	core::StructExpr::Members members;
	for ( auto it = globals.begin(), end = globals.end(); it != end; ++it ) {
		// get entry type and wrap it into a reference if necessary
		auto fit = varTU.find(*it);
		assert(fit != varTU.end());

		/*
		 * In the case we have to resolve the initial value the current translation
		 * unit has to be set properly
		 */
		fact.setTranslationUnit(fact.getProgram().getTranslationUnit(fit->second));
		core::IdentifierPtr ident = builder.identifier((*it)->getNameAsString());

		core::TypePtr&& type = fact.convertType((*it)->getType().getTypePtr());
		if ( (*it)->hasExternalStorage() ) {
			/*
			 * the variable is defined as extern, so we don't have to allocate memory
			 * for it just refer to the memory location someone else has initialized
			 */
			type = builder.refType( type );
		}

		// add type to the global struct
		entries.push_back( core::StructType::Entry( ident, type ) );
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
			if ( (*it)->getInit() ) {
				// this means the variable is not declared static inside a function so we have to initialize its value
				initExpr = fact.convertInitExpr((*it)->getInit(), type, false);
			} else {
				initExpr = fact.defaultInitVal(type);
			}
		}
		// default initialization
		members.push_back( core::StructExpr::Member(ident, initExpr) );

	}
	core::StructTypePtr&& structTy = builder.structType(entries);
	// we name this structure as '__insieme_globals'
	structTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(std::string("__insieme_globals")) );
	// set back the original TU
	assert(currTU && "Lost reference to the translation unit");
	fact.setTranslationUnit(fact.getProgram().getTranslationUnit(currTU));

	return std::make_pair(structTy, builder.structExpr(structTy, members) );
}

void GlobalVarCollector::dump(std::ostream& out) const {
	out << std::endl << "// ~~~~~~~~ GLOBAL VAR MAP ~~~~~~~~ //" << std::endl;
	out << "  SIZE:      " << globals.size() << std::endl;
	out << "  GLOBALS:   " << join(", ", globals) << std::endl;
	out << "  FUNCTIONS: " << join(" ,", usingGlobals) << std::endl;
	out << "// ~~~~~~~~~~~~~~~~~~~~~~~~ //";
}

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace
