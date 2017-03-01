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

#include <list>
#include <map>

#include "insieme/frontend/extensions/frontend_extension.h"

// forward decl
namespace clang {
	class VariableArrayType;
}

namespace insieme {
namespace frontend {
namespace extensions {
	
	class VariableLengthArrayExtension : public FrontendExtension {
	private:
		// store list of generated declaration expressions
		std::list<core::DeclarationStmtPtr> sizes;
		// map from clang declarations to the associated variable array type
		std::map<const clang::VariableArrayType*, core::TypePtr> arrayTypeMap;
		// whether we are currently in a declaration statement
		bool inDecl = false;

	public:
		/**
		 *  Type Visitor that converts clang variable sized array types 
		 *  into corresponding IR array types.
		 *  @param type clang type
		 *  @param converter insieme conversion factory
		 *  @return converted clang type or nullptr if not converted
		 */
		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter);

		/**
		 *  Stmt Visitor that converts clang declaration
		 *  statements of variable sized array types
		 *  into corresponding IR declaration statements.
		 *  @param stmt clang stmt
		 *  @param converter insieme conversion factory
		 *  @return converted clang stmt or empty stmt list if not converted
		 */
		virtual stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter);

		/**
		 *  Expr Visitor that handles size of expression
		 *  of variable length array types.
		 *  @param expr clang stmt
		 *  @param converter insieme conversion factory
		 *  @return converted clang stmt or empty stmt list if not converted
		 */
		virtual core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter);

		/**
		 *  Stmt Visitor that post visits clang declaration
		 *  statements of variable sized array types
		 *  and injects the index variable declarations.
		 *  @param stmt clang stmt
		 *  @param irStmt IR variable sized array decl stmt
		 *  @param converter insieme conversion factory
		 *  @return converted clang stmt or empty stmt list if not converted
		 */		
		virtual stmtutils::StmtWrapper PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
					insieme::frontend::conversion::Converter& converter);
		
	};
}
}
}