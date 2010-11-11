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

#include "insieme/frontend/conversion.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"
#include "insieme/c_info/naming.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"
#include "clang/Index/Program.h"

using namespace clang;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::NamedDecl* func) {
	return out << func->getNameAsString();
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
		globals.insert( std::make_pair(decl, std::make_pair(false, false)) );

		const FunctionDecl* enclosingFunc = funcStack.top();
		assert(enclosingFunc);
		usingGlobals.insert(enclosingFunc); // the enclosing function uses globals
	}
	return true;
}

bool GlobalVarCollector::VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
	if(VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
		if(varDecl->hasGlobalStorage()) {

			// add the variable to the list of global vars (if not already there)
			if(globals.find(varDecl) == globals.end())
				globals.insert( std::make_pair(varDecl, std::make_pair(true, false)) );

			const FunctionDecl* enclosingFunc = funcStack.top();
			assert(enclosingFunc);
			usingGlobals.insert(enclosingFunc); // the enclosing function uses globals
		}
	}
	return true;
}

bool GlobalVarCollector::VisitCallExpr(clang::CallExpr* callExpr) {
	FunctionDecl* funcDecl = callExpr->getDirectCallee();
	const FunctionDecl *definition = NULL;

	if(!funcDecl->hasBody(definition)) {
		// if the function is not defined in this translation unit, maybe it is defined in another we already loaded
		// use the clang indexer to lookup the definition for this function declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		definition = indexer.getDefinitionFor(funcEntity).first;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();
	}

	return true;
}

// This function syntetize the global structure that will be used to hold
// the global variables used within the functions of the input program
std::pair<core::StructTypePtr, core::StructExprPtr> GlobalVarCollector::createGlobalStruct(const conversion::ConversionFactory& fact) const {
	core::StructType::Entries entries;
	core::StructExpr::Members members;
	for(auto it = globals.begin(), end = globals.end(); it != end; ++it) {
		core::TypePtr&& entryType = fact.getASTBuilder().refType( fact.convertType(it->first->getType().getTypePtr()) );
		if(it->second.second) {
			// the variable is defined as exter, so we don't have to allocate memory for it
			// just refear to the memory location someone else has defined
			entryType = fact.getASTBuilder().refType( entryType );
		}
		core::Identifier ident(it->first->getNameAsString());
		// add type to the global struct
		entries.push_back( core::StructType::Entry( ident, entryType ) );
		// add proper initializer
		if(it->second.first) {
			// this means the variable is not declared static inside a function so we have to initialize its value
			if(it->first->getInit()) {
				core::ExpressionPtr&& initExpr = fact.convertExpr(it->first->getInit());
				members.push_back(
					core::StructExpr::Member(ident, fact.getASTBuilder().callExpr( entryType, core::lang::OP_REF_VAR_PTR, toVector( initExpr ) ))
				);
				continue;
			}
		}
		// default initialization
		members.push_back( core::StructExpr::Member(ident, fact.defaultInitVal(entryType)) );

	}
	core::StructTypePtr&& structTy = fact.getASTBuilder().structType(entries);
	// we name this structure as '__insieme_globals'
	structTy->addAnnotation( std::make_shared<c_info::CNameAnnotation>(std::string("__insieme_globals")) );
	return std::make_pair(structTy, fact.getASTBuilder().structExpr(members) );
}

void GlobalVarCollector::dump(std::ostream& out) const {
	out << std::endl << "// ~~~~~~~~ GLOBAL VAR MAP" << std::endl;
	out << "  SIZE:      " << globals.size() << std::endl;
	out << "  GLOBALS:   " << join(", ", globals) << std::endl;
	out << "  FUNCTIONS: " << join(" ,", usingGlobals) << std::endl;
	out << "// ~~~~~~~~~~~~~~~~~~~~~~~~";
}

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::frontend::analysis::GlobalVarCollector& globals) {
	globals.dump(out);
	return out;
}
} // end std namespace
