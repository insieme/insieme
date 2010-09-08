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

#include <gtest/gtest.h>

#include "program.h"
#include "clang_compiler.h"
#include "conversion.h"

#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"

using namespace insieme::core;

using namespace insieme::frontend;
using namespace insieme::frontend::conversion;

TEST(TypeConversion, HandleForStmt) {
	using namespace clang;

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	SourceLocation emptyLoc;
	ASTContext& ctx = clang.getASTContext();

	// create the loop index variable declaration
	// uint i
	BuiltinType uintTy(BuiltinType::UInt);
	VarDecl* varDecl = VarDecl::Create(ctx, NULL, emptyLoc, clang.getPreprocessor().getIdentifierInfo("i"),	QualType(&uintTy, 0), 0, VarDecl::None);

	// create the DeclStmt
	DeclStmt* forInit = new (ctx) DeclStmt( DeclGroupRef(varDecl), emptyLoc, emptyLoc );

	// Create the body of the loop stmt, an empty CompoundStmt: { }
	clang::CompoundStmt* body = new (ctx) clang::CompoundStmt(ctx, NULL, 0, emptyLoc, emptyLoc );

	// Create the condition expression for the loop stmt, i.e. ...; i < 10; ...
	DeclRefExpr indexRef(varDecl, varDecl->getType(), emptyLoc);
	llvm::APInt int10(32, 10, false);
	IntegerLiteral intLit(int10, QualType(&uintTy, 0), emptyLoc);
	BuiltinType boolTy(BuiltinType::Bool);
	// i < 10
	BinaryOperator* condExpr = new (ctx) BinaryOperator(&indexRef, &intLit, BinaryOperator::LT, QualType(&boolTy, 0), emptyLoc);

	// Creates the increment expression for the loop stmt, i.e. ; ++i
	DeclRefExpr incIndexRef(varDecl, varDecl->getType(), emptyLoc);
	UnaryOperator* incExpr = new (ctx) UnaryOperator(&incIndexRef, UnaryOperator::PreInc, QualType(&uintTy, 0), emptyLoc);

	clang::ForStmt clangForStmt( forInit, condExpr, 0, incExpr, body, emptyLoc, emptyLoc, emptyLoc);

	StatementPtr insiemeStmt = convFactory.ConvertStmt( clangForStmt );
	EXPECT_TRUE(insiemeStmt);
	EXPECT_EQ("", insiemeStmt->toString());

}
