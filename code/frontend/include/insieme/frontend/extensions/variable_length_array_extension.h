/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <list>
#include <map>

#include "insieme/frontend/extensions/frontend_extension.h"

namespace insieme {
namespace frontend {
namespace extensions {
	
	class VariableLengthArrayExtension : public FrontendExtension {
	private:
		std::list<core::StatementPtr> sizes;
		std::map<const clang::Decl*, std::list<core::ExpressionPtr>> arrayDeclVarMap;
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