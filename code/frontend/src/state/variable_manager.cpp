/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/state/variable_manager.h"

#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace frontend {
namespace state {

	VariableManager::VariableManager(Converter& converter) : converter(converter) { pushScope(false); }

	core::ExpressionPtr VariableManager::lookup(const clang::VarDecl* varDecl) const {
		// lookup in innermost lambda scope if available
		if(lambdaScopes.size()>=1) {
			auto f = lambdaScopes.back().find(varDecl);
			if(f != lambdaScopes.back().cend()) return f->second(getThis());
		}

		// lookup globals in outermost scope
		if(varDecl->hasGlobalStorage() && !varDecl->isStaticLocal()) {
			frontend_assert(::containsKey(storage.front().variables, varDecl)) << "Trying to look up global variable not previously declared: "
				                                                               << dumpClang(varDecl);
			return storage.front().variables.find(varDecl)->second;
		}

		// lookup local vars in all applicable scopes starting from innermost
		for(auto it = storage.crbegin(); it != storage.crend(); ++it) {
			auto loc = it->variables.find(varDecl);
			if(loc!=it->variables.end()) return loc->second;
			if(!it->nested) break;
		}

		frontend_assert(false) << "Trying to look up local variable not previously declared: " << dumpClang(varDecl);
		return core::ExpressionPtr();
	}

	void VariableManager::undefine(const clang::VarDecl* varDecl) {
		if(::containsKey(storage.back().variables, varDecl)) {
			storage.back().variables.erase(varDecl);
		}
	}

	void VariableManager::insert(const clang::VarDecl* varDecl, const core::ExpressionPtr& var) {
		if(varDecl->hasGlobalStorage() && !varDecl->isStaticLocal()) frontend_assert(storage.size() == 1) << "Global variable not inserted at global scope";
		frontend_assert(!::containsKey(storage.back().variables, varDecl)) << "Trying to insert variable already declared previously: "<< dumpClang(varDecl);
		converter.applyHeaderTagging(var, varDecl);
		storage.back().variables[varDecl] = var;
	}

	size_t VariableManager::numVisibleDeclarations() const {
		size_t sum = storage.front().variables.size(); // globals always visible
		for(auto it = storage.crbegin(); it != storage.crend()-1; ++it) {
			sum += it->variables.size();
			if(!it->nested) break;
		}
		return sum;
	}

	void VariableManager::setThis(const core::ExpressionPtr& thisVar) {
		frontend_assert(!storage.back().thisExpr) << "Trying to set \"this\" variable for current scope, but already set.";
		storage.back().thisExpr = thisVar;
	}

	core::ExpressionPtr VariableManager::getThis() const {
		// lookup this in all applicable scopes starting from innermost
		for(auto it = storage.crbegin(); it != storage.crend(); ++it) {
			if(it->thisExpr) return it->thisExpr;
			if(!it->nested) break;
		}

		frontend_assert(false) << "Trying to look up \"this\" variable, but not defined in any applicable scope";
		return {};
	}

	void VariableManager::setRetType(const core::TypePtr& retType) {
		frontend_assert(!storage.back().retType) << "Trying to set return type for current scope, but already set.";
		storage.back().retType = retType;
	}

	core::TypePtr VariableManager::getRetType() {
		// lookup this in all applicable scopes starting from innermost
		for(auto it = storage.crbegin(); it != storage.crend(); ++it) {
			if(it->retType) return it->retType;
			if(!it->nested) break;
		}

		frontend_assert(false) << "Trying to look up return type, but not defined";
		return {};
	}

	void VariableManager::pushLambda(const LambdaScope& lambdaScope) {
		lambdaScopes.push_back(lambdaScope);
	}

	void VariableManager::popLambda() {
		frontend_assert(!lambdaScopes.empty()) << "Trying to pop lambda scope, but none open";
		lambdaScopes.pop_back();
	}

} // end namespace state
} // end namespace frontend
} // end namespace insieme
