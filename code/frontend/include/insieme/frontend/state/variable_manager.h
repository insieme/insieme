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
#pragma once

#include <map>

#include "insieme/frontend/converter.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace frontend {
namespace state {
	using namespace conversion;

	/// Manages variable identities from clang to its INSPIRE translation
	class VariableManager {
	public:
		using AccessGenerator = std::function<core::ExpressionPtr(core::ExpressionPtr)>;

		/// map from clang VarDecl to functor which generates the access to that variable upon being passed the current "this" IR representation
		class LambdaScope : public std::map<const clang::VarDecl*, AccessGenerator> {
			AccessGenerator thisGenerator;
			bool hasThis_ = false;
			bool isaLambda_ = false;
		public:
			void setThisGenerator(AccessGenerator gen) {
				thisGenerator = gen;
				hasThis_ = true;
			}
			const AccessGenerator& getThisGenerator() const {
				return thisGenerator;
			}
			bool hasThis() const { return hasThis_; }
			void setIsaLambda() { isaLambda_ = true; }
			bool isaLambda() { return isaLambda_; }
		};

	private:
		Converter& converter;

		struct Scope {
			Scope(bool n) : nested(n) {}
			/// stores variables declared in this scope
			std::map<const clang::VarDecl*, core::ExpressionPtr> variables;
			/// determines whether variables from upper scopes are visible
			bool nested;
			/// stores the "this" parameter for methods
			core::ExpressionPtr thisExpr = nullptr;
			/// stores the return type for functions and methods
			core::TypePtr retType = nullptr;
		};

		/// Internal storage for mappings from clang variable declarations to
		/// IR variables for locals and literals for globals
		std::vector<Scope> storage;

		/// Internal storage for lambda captures
		mutable std::vector<LambdaScope> lambdaScopes;

	public:
		VariableManager(Converter& converter);

		/// Push a new scope for declared local variables
		void pushScope(bool nested) { storage.push_back(Scope{nested}); }
		/// Pop a scope after closing it
		void popScope() { storage.pop_back(); }

		core::ExpressionPtr lookup(const clang::VarDecl* varDecl) const;
		void undefine(const clang::VarDecl* varDecl);
		void insert(const clang::VarDecl* varDecl, const core::ExpressionPtr& var);

		/// Set the "this" expression for the current scope
		void setThis(const core::ExpressionPtr& thisVar);
		/// Get the "this" expression for the current scope
		core::ExpressionPtr getThis() const;

		/// Set the return type for the current scope
		void setRetType(const core::TypePtr& retType);
		/// Get the return type for the current scope
		core::TypePtr getRetType();

		/// Get the number of visible declarations in current scope (for testing)
		size_t numVisibleDeclarations() const;

		/// Push a new lambda scope
		void pushLambda(const LambdaScope& lambdaScope);
		/// Pop a lambda scope
		void popLambda();
	};

} // end namespace state
} // end namespace frontend
} // end namespace insieme
