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

#pragma once

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Ownership.h"
#pragma GCC diagnostic pop

using clang::SourceLocation;

namespace insieme {
namespace frontend {

namespace pragma {
// forward declarations for pragma
class Pragma;
typedef std::shared_ptr<Pragma> PragmaPtr;
typedef std::vector<PragmaPtr> 	PragmaList;

class MatchMap;
} // end pragma namespace

// ------------------------------------ InsiemeSema ---------------------------

/**
 * This purpose of this class is to overload the behavior of clang parser in a way every time an AST node is created,
 * pending pragmas are correctly associated to it.
 */
class InsiemeSema: public clang::Sema {
	class InsiemeSemaImpl;
	InsiemeSemaImpl* pimpl;

	bool isInsideFunctionDef;

	void matchStmt(clang::Stmt* 				S,
				   const clang::SourceRange& 	bounds,
				   const clang::SourceManager& 	sm,
				   pragma::PragmaList& 			matched);

	InsiemeSema(const InsiemeSema& other);

public:
	InsiemeSema (pragma::PragmaList&   			pragma_list,
				 clang::Preprocessor& 			pp,
				 clang::ASTContext& 			ctx,
				 bool 							CompleteTranslationUnit = true,
				 clang::CodeCompleteConsumer* 	CompletionConsumer = 0) ;

	~InsiemeSema();

	void addPragma(pragma::PragmaPtr P);

	clang::StmtResult ActOnCompoundStmt(clang::SourceLocation 	L,
										clang::SourceLocation 	R,
										clang::MultiStmtArg 	Elts,
										bool			 		isStmtExpr );

	clang::StmtResult ActOnIfStmt(  clang::SourceLocation 		IfLoc,
									clang::Sema::FullExprArg 	CondVal,
									clang::Decl* 				CondVar,
									clang::Stmt* 				ThenVal,
									clang::SourceLocation 		ElseLoc,
									clang::Stmt* 				ElseVal );

	clang::StmtResult ActOnForStmt( clang::SourceLocation 		ForLoc,
									clang::SourceLocation		LParenLoc,
									clang::Stmt* 				First,
									clang::Sema::FullExprArg 	Second,
									clang::Decl* 				SecondVar,
									clang::Sema::FullExprArg 	Third,
									clang::SourceLocation 		RParenLoc,
									clang::Stmt* 				Body );

	clang::Decl* ActOnStartOfFunctionDef(clang::Scope*		FnBodyScope,
										 clang::Declarator&	D );

	clang::Decl* ActOnStartOfFunctionDef(clang::Scope *FnBodyScope, clang::Decl* D);

	clang::Decl* ActOnFinishFunctionBody(clang::Decl* Decl, clang::Stmt* Body);

	clang::Decl* ActOnDeclarator(clang::Scope *S, clang::Declarator &D);

//	clang::StmtResult ActOnDeclStmt(clang::Sema::DeclGroupPtrTy Decl, SourceLocation StartLoc, SourceLocation EndLoc);

	void ActOnTagFinishDefinition(clang::Scope* S, clang::Decl* TagDecl, clang::SourceLocation RBraceLoc);

	/**
	 * Register the parsed pragma.
	 */
	template <class T>
	void ActOnPragma(const std::string& 		name,
					 const pragma::MatchMap& 	mmap,
					 clang::SourceLocation 		startLoc,
					 clang::SourceLocation 		endLoc)
	{
		addPragma( pragma::PragmaPtr(new T(startLoc, endLoc, name, mmap)) );
	}

	/**
	 * Write into the logger information about the pragmas and their associatioation to AST nodes.
	 */
	void dump();
};

} // End frontend namespace
} // End insieme namespace

