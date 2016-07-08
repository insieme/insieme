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
		/// map from clang VarDecl to functor which generates the access to that variable upon being passed the current "this" IR representation
		using LambdaScope = std::map<const clang::VarDecl*, std::function<core::ExpressionPtr(core::ExpressionPtr)>>;

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
		std::vector<LambdaScope> lambdaScopes;

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
