/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/frontend/state/variable_manager.h"

#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/lang/pointer.h"

namespace insieme {
namespace frontend {
namespace state {

	VariableManager::VariableManager(Converter& converter) : converter(converter) { pushScope(false); }

	core::ExpressionPtr VariableManager::lookup(const clang::VarDecl* varDecl) const {
		// lookup in innermost lambda scope if available
		if(lambdaScopes.size()>=1) {
			auto f = lambdaScopes.back().find(varDecl);
			if(f != lambdaScopes.back().cend()) {
				// push empty lambda scope to ensure standard handling of "this" lookup (rather than getting the intercepted "this")
				auto generator = f->second;
				lambdaScopes.push_back(LambdaScope());
				auto actualThis = getThis();
				lambdaScopes.pop_back();
				return generator(actualThis);
			}
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

		// true constants are not captured in lambdas, and we need to copy the value
		if(lambdaScopes.size()>=1 && lambdaScopes.back().isaLambda()) {
			auto varInit = varDecl->getInit();
			frontend_assert(varInit && varDecl->getType().isConstQualified()) << "Nondeclared local var in lambda without const init";
			return converter.getIRBuilder().refTemp(converter.convertExpr(varInit));
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
		core::ExpressionPtr ret;

		// lookup this in all applicable scopes starting from innermost
		for(auto it = storage.crbegin(); it != storage.crend(); ++it) {
			if(it->thisExpr) {
				ret = it->thisExpr;
				break;
			}
			if(!it->nested) break;
		}

		// deref once
		ret = core::IRBuilder(ret.getNodeManager()).deref(ret);

		// if in lambda with captures, use that "this"
		if(ret && lambdaScopes.size()>=1 && lambdaScopes.back().hasThis()) {
			ret = core::lang::buildPtrToRef(core::IRBuilder(ret.getNodeManager()).deref(lambdaScopes.back().getThisGenerator()(ret)));
		}

		if(ret) return ret;

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
