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

#include "insieme/frontend/clang.h"
#include "insieme/frontend/utils/source_locations.h"

using clang::SourceLocation;

namespace insieme {
namespace frontend {

namespace pragma {
	// forward declarations for pragma
	class Pragma;
	typedef std::shared_ptr<Pragma> PragmaPtr;
	typedef std::vector<PragmaPtr> PragmaList;

	class MatchMap;
} // end pragma namespace

// ------------------------------------ InsiemeSema ---------------------------

/**
 * This purpose of this class is to overload the behavior of clang parser in a way every time an AST node is created,
 * pending pragmas are correctly associated to it.
 */
class InsiemeSema : public clang::Sema {
	struct InsiemeSemaImpl;
	InsiemeSemaImpl* pimpl;

	bool isInsideFunctionDef;

	void matchStmt(clang::Stmt* S, const clang::SourceRange& bounds, const clang::SourceManager& sm, pragma::PragmaList& matched);

	InsiemeSema(const InsiemeSema& other);

	unsigned getLineNum(const clang::SourceLocation& loc) {
		return frontend::utils::Line(loc, SourceMgr);
	}

  public:
	InsiemeSema(pragma::PragmaList& pragma_list, clang::Preprocessor& pp, clang::ASTContext& ctx, clang::ASTConsumer& ast_consumer,
	            bool CompleteTranslationUnit = true, clang::CodeCompleteConsumer* CompletionConsumer = 0);

	~InsiemeSema();

	void addPragma(pragma::PragmaPtr P);

	clang::StmtResult ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R, llvm::ArrayRef<clang::Stmt*> Elts, bool isStmtExpr);

	clang::StmtResult ActOnIfStmt(clang::SourceLocation IfLoc, clang::Sema::FullExprArg CondVal, clang::Decl* CondVar, clang::Stmt* ThenVal,
	                              clang::SourceLocation ElseLoc, clang::Stmt* ElseVal);

	clang::StmtResult ActOnForStmt(clang::SourceLocation ForLoc, clang::SourceLocation LParenLoc, clang::Stmt* First, clang::Sema::FullExprArg Second,
	                               clang::Decl* SecondVar, clang::Sema::FullExprArg Third, clang::SourceLocation RParenLoc, clang::Stmt* Body);

	clang::Decl* ActOnStartOfFunctionDef(clang::Scope* FnBodyScope, clang::Declarator& D);

	clang::Decl* ActOnStartOfFunctionDef(clang::Scope* FnBodyScope, clang::Decl* D);

	clang::Decl* ActOnFinishFunctionBody(clang::Decl* Decl, clang::Stmt* Body);

	clang::Decl* ActOnDeclarator(clang::Scope* S, clang::Declarator& D);

	//	clang::StmtResult ActOnDeclStmt(clang::Sema::DeclGroupPtrTy Decl, SourceLocation StartLoc, SourceLocation EndLoc);

	void ActOnTagFinishDefinition(clang::Scope* S, clang::Decl* TagDecl, clang::SourceLocation RBraceLoc);

	/**
	 * Register the parsed pragma.
	 */
	template <class T>
	void ActOnPragma(const std::string& name, const pragma::MatchMap& mmap, clang::SourceLocation startLoc, clang::SourceLocation endLoc) {
		addPragma(pragma::PragmaPtr(new T(startLoc, endLoc, name, mmap)));
	}

	void ActOnFrontendExtensionPragma(pragma::PragmaPtr p) {
		addPragma(p);
	}
};

} // End frontend namespace
} // End insieme namespace
