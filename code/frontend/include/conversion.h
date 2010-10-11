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

#include "pragma_handler.h"
#include "program.h"
#include "ast_builder.h"

#include "clang/AST/ASTConsumer.h"

// Forward declarations
namespace clang {
class ASTContext;
class DeclGroupRef;
}

namespace insieme {
namespace frontend {
namespace conversion {

class ClangStmtConverter;
class ClangTypeConverter;
class ClangExprConverter;

// ------------------------------------ ConversionFactory ---------------------------
/**
 * A factory used to convert clang AST nodes (i.e. statements, expressions and types) to Insieme IR nodes.
 */
class ConversionFactory {
	core::SharedNodeManager  mgr;
	const core::ASTBuilder   builder;
    const ClangCompiler& 	 clangComp;
    PragmaStmtMap 	 	 	 pragmaMap;

	ClangTypeConverter* typeConv;
	ClangExprConverter* exprConv;
	ClangStmtConverter* stmtConv;

	friend class ClangTypeConverter;
	friend class ClangExprConverter;
	friend class ClangStmtConverter;
public:
	ConversionFactory(core::SharedNodeManager mgr, const ClangCompiler& clang);

	core::TypePtr 		ConvertType(const clang::Type& type) const;
	core::StatementPtr 	ConvertStmt(const clang::Stmt& stmt) const;
	core::ExpressionPtr ConvertExpr(const clang::Expr& expr) const;

	const core::ASTBuilder&  getASTBuilder() const { return builder; }
	core::SharedNodeManager getNodeManager() const { return mgr; }

	const PragmaStmtMap& getPragmaMap() const { return pragmaMap; }
	void updatePragmaMap(const PragmaList& pragmaList) { pragmaMap = PragmaStmtMap(pragmaList); }

	void convertClangAttributes(clang::VarDecl* varDecl, core::TypePtr type);
    void convertClangAttributes(clang::ParmVarDecl* varDecl, core::TypePtr type);

	~ConversionFactory();
};

// ------------------------------------ IRConsumer ---------------------------
/**
 *
 */
class IRConsumer: public clang::ASTConsumer {
	const ClangCompiler& mClangComp;
	core::ProgramPtr     mProgram;
	ConversionFactory    mFact;
	const PragmaList&	 pragmaList;
	bool 			     mDoConversion;

public:
	IRConsumer(const ClangCompiler& clangComp, insieme::core::SharedNodeManager manager, insieme::core::ProgramPtr prog, const PragmaList& pragmaList, bool doConversion=true) :
		mClangComp(clangComp), mProgram(prog), mFact(manager, clangComp), pragmaList(pragmaList), mDoConversion(doConversion){ }

	core::ProgramPtr getProgram() const { return mProgram; }

	virtual void Initialize(clang::ASTContext &Context) { }

	virtual void HandleTopLevelDecl(clang::DeclGroupRef D);
	virtual void HandleTranslationUnit(clang::ASTContext &Ctx);
};


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
