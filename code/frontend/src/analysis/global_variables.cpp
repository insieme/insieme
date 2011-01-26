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
#include "insieme/c_info/naming.h"

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
		globals.insert( std::make_pair(decl, false) );
		varTU.insert( std::make_pair(decl, currTU) );

		const FunctionDecl* enclosingFunc = funcStack.top();
		assert(enclosingFunc);
		usingGlobals.insert(enclosingFunc); // the enclosing function uses globals
	}
	return true;
}

bool GlobalVarCollector::VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
	if(VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
		if(varDecl->hasGlobalStorage()) {

			const FunctionDecl* enclosingFunc = funcStack.top();
			assert(enclosingFunc);
			usingGlobals.insert(enclosingFunc); // the enclosing function uses globals

			// add the variable to the list of global vars (if not already there)
			if(globals.find(varDecl) == globals.end()) {
				if(varDecl->hasExternalStorage()) {
					// look for the definition. If we find it it means we have access to the translation unit defining this variable
					const VarDecl* def = varDecl->getDefinition();
					if(!def) {
						globals.insert( std::make_pair(varDecl, true) );
						varTU.insert( std::make_pair(varDecl, currTU) );
						return true;
					}
				}
				globals.insert( std::make_pair(varDecl, false) );
				varTU.insert( std::make_pair(varDecl, currTU) );
			}
		}
	}
	return true;
}

bool GlobalVarCollector::VisitCallExpr(clang::CallExpr* callExpr) {
	FunctionDecl* funcDecl = callExpr->getDirectCallee();
	const FunctionDecl *definition = NULL;

	const clang::idx::TranslationUnit* old = currTU;
	if(!funcDecl->hasBody(definition)) {
		// if the function is not defined in this translation unit, maybe it is defined in another we already loaded
		// use the clang indexer to lookup the definition for this function declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, indexer.getProgram());
		std::pair<FunctionDecl*, clang::idx::TranslationUnit*> ret = indexer.getDefinitionFor(funcEntity);
		definition = ret.first;
		currTU = ret.second;
	}

	if(definition) {
		funcStack.push(definition);
		(*this)(definition);
		funcStack.pop();

		// if the called function access the global data structure
		// also the current function has to be marked (otherwise the
		// global structure will not correctly forwarded)
		if(usingGlobals.find(definition) != usingGlobals.end()) {
			usingGlobals.insert( funcStack.top() );
		}
	}

	currTU = old;

	return true;
}

// This function syntetize the global structure that will be used to hold the global variables used within the functions of the input program
std::pair<core::StructTypePtr, core::StructExprPtr> GlobalVarCollector::createGlobalStruct(conversion::ConversionFactory& fact) const {
	// no global variable found, we return an empty tuple
	if(globals.empty())
		return std::make_pair(core::StructTypePtr(), core::StructExprPtr());

	const core::ASTBuilder& builder = fact.getASTBuilder();
	core::StructType::Entries entries;
	core::StructExpr::Members members;
	for(auto it = globals.begin(), end = globals.end(); it != end; ++it) {
		// get entry type and wrap it into a reference if necessary
		auto fit = varTU.find(it->first);
		assert(fit != varTU.end());
		// in the case we have to resolve the initial value the current translation unit
		// has to be set properly
		fact.setTranslationUnit(fact.getProgram().getTranslationUnit(fit->second));
		core::Identifier ident(it->first->getNameAsString());

		bool addPtr = false;
		core::TypePtr&& type = fact.convertType(it->first->getType().getTypePtr());
		if(type->getNodeType() == core::NT_VectorType) {
			type = builder.arrayType( builder.refType(type) );
			fact.addDerefField(ident);
			addPtr = true;
		}
		core::RefTypePtr&& entryType = builder.refType( type );

		if(it->second) {
			// the variable is defined as extern, so we don't have to allocate memory for it
			// just refear to the memory location someone else has initialized
			entryType = builder.refType( entryType );
		}

		// add type to the global struct
		entries.push_back( core::StructType::Entry( ident, entryType ) );
		// add initialization

		// we have to initialize the value of this ref with the value of the extern
		// variable which we assume will be visible from the entry point
		core::ExpressionPtr initExpr;
		if(it->second) {
			initExpr = builder.literal(it->first->getNameAsString(), entryType);
		} else {
			if(it->first->getInit()) {
				// this means the variable is not declared static inside a function so we have to initialize its value
				initExpr = fact.convertInitExpr(it->first->getInit(), entryType->getElementType(), false);
				if(addPtr && !fact.getASTBuilder().getBasicGenerator().isNullPtr(entryType->getElementType())) {
					initExpr = fact.getASTBuilder().callExpr(entryType, fact.getASTBuilder().getBasicGenerator().getArrayCreate1D(),
							fact.getASTBuilder().refVar(initExpr),
							fact.getASTBuilder().literal("1", fact.getASTBuilder().getBasicGenerator().getInt4()));
				}
			} else {
				initExpr = fact.defaultInitVal(entryType->getElementType());
			}
			// allocate vectors in the heap with ref.new
			if(entryType->getElementType()->getNodeType() == core::NT_VectorType) {
				initExpr = builder.callExpr(entryType, builder.getBasicGenerator().getRefNew(), initExpr);
			} else {
				initExpr = builder.callExpr(entryType, builder.getBasicGenerator().getRefVar(), initExpr);
			}
		}
		// default initialization
		members.push_back( core::StructExpr::Member(ident, initExpr) );

	}
	core::StructTypePtr&& structTy = builder.structType(entries);
	// we name this structure as '__insieme_globals'
	structTy->addAnnotation( std::make_shared<c_info::CNameAnnotation>(std::string("__insieme_globals")) );
	// set back the original TU
	assert(currTU);
	fact.setTranslationUnit(fact.getProgram().getTranslationUnit(currTU));
	return std::make_pair(structTy, builder.structExpr(structTy, members) );
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
