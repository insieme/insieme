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

#include "program.h"
#include "ast_builder.h"

#include "pragma_handler.h"

// Forward declarations
namespace clang {
class ASTContext;
class DeclGroupRef;
class FunctionDecl;
}

namespace insieme {
namespace frontend {
namespace conversion {

// ------------------------------------ ConversionFactory ---------------------------
/**
 * A factory used to convert clang AST nodes (i.e. statements, expressions and types) to Insieme IR nodes.
 */
class ConversionFactory {

	class ClangStmtConverter;
	class ClangTypeConverter;
	class ClangExprConverter;

	class ConversionContext;
	ConversionContext* ctx;

	core::SharedNodeManager  mgr;
	const core::ASTBuilder   builder;
    const ClangCompiler& 	 clangComp;
    PragmaStmtMap 	 		 pragmaMap;

	ClangTypeConverter* typeConv;
	ClangExprConverter* exprConv;
	ClangStmtConverter* stmtConv;

	core::ExpressionPtr defaultInitVal(const clang::Type& ty, const core::TypePtr type );
public:
	ConversionFactory(core::SharedNodeManager mgr, const ClangCompiler& clang, const PragmaList& pragmaList = PragmaList());

	const core::ASTBuilder& getASTBuilder() const { return builder; }
	core::SharedNodeManager getNodeManager() const { return mgr; }

	const PragmaStmtMap& getPragmaMap() const { return pragmaMap; }

	core::TypePtr 		convertType(const clang::Type& type) const;
	core::StatementPtr 	convertStmt(const clang::Stmt& stmt) const;
	core::ExpressionPtr convertExpr(const clang::Expr& expr) const;

	core::ExpressionPtr convertFunctionDecl(const clang::FunctionDecl* funcDecl);
	core::DeclarationStmtPtr convertVarDecl(const clang::VarDecl* funcDecl);

	core::AnnotationPtr convertAttribute(const clang::VarDecl* varDecl) const;

	core::VariablePtr lookUpVariable(const clang::VarDecl* varDecl);

	~ConversionFactory();
};

// ------------------------------------ IRConverter ---------------------------
/**
 *
 */
class IRConverter {
	const ClangCompiler& mClangComp;
	ConversionFactory    mFact;
	core::ProgramPtr     mProgram;
	const PragmaList&	 pragmaList;


public:
	IRConverter(const ClangCompiler& clangComp, const core::ProgramPtr prog, const core::SharedNodeManager& mgr, const PragmaList& pragmaList) :
		mClangComp(clangComp), mFact(mgr, clangComp, pragmaList), mProgram(prog), pragmaList(pragmaList) { }

	core::ProgramPtr getProgram() const { return mProgram; }

	void handleTopLevelDecl(const clang::DeclContext* declCtx);
	core::ExpressionPtr handleFunctionDecl(const clang::FunctionDecl* funcDecl) { return mFact.convertFunctionDecl(funcDecl); }

	core::LambdaExprPtr handleBody(const clang::Stmt* body);
};


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
