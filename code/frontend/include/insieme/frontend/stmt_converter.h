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

#include "insieme/frontend/convert.h"

#include <clang/AST/StmtVisitor.h>

#include "insieme/core/forward_decls.h"


namespace stmtutils {

using namespace insieme::core;

//-------------------------------------------- StmtWrapper ------------------------------------------------------------
/*
 * Utility class used as a return type for the StmtVisitor. It can store a list of statement
 * as conversion of a single C stmt can result in multiple IR statements.
 */
struct StmtWrapper: public StatementList {
	StmtWrapper() :
			StatementList() {
	}
	StmtWrapper(const insieme::core::StatementPtr& stmt) :
			StatementList( { stmt }) {
	}

	insieme::core::StatementPtr getSingleStmt() const {
		assert(size() == 1 && "More than 1 statement present");
		return front();
	}

	bool isSingleStmt() const {
		return size() == 1;
	}
};

StatementPtr tryAggregateStmt(const IRBuilder& builder, const StatementPtr& stmt);
StatementPtr tryAggregateStmts(const IRBuilder& builder, const StatementList& stmtVect);
ExpressionPtr makeOperation(const IRBuilder& builder, const ExpressionPtr& lhs,
		const ExpressionPtr& rhs, const lang::BasicGenerator::Operator& op);

}

namespace insieme {
namespace frontend {
namespace conversion {

#define FORWARD_STMT_TO_EXPR_VISITOR_CALL(StmtTy) \
	stmtutils::StmtWrapper Visit##StmtTy( clang::StmtTy* stmt ) { return stmtutils::StmtWrapper( convFact.convertExpr(stmt) ); }

#define CALL_BASE_STMT_VISIT(Base, StmtTy) \
	stmtutils::StmtWrapper Visit##StmtTy( clang::StmtTy* stmt ) { return Base::Visit##StmtTy( stmt ); }

//---------------------------------------------------------------------------------------------------------------------
//							BASE STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class Converter::StmtConverter {

protected:
	Converter& 					convFact;
	core::NodeManager& 					mgr;
	const core::IRBuilder& 				builder;
	const core::lang::BasicGenerator& 	gen;

public:
	StmtConverter(Converter& convFact) :
		convFact(convFact), mgr(convFact.mgr),
		builder(convFact.builder), gen(convFact.mgr.getLangBasic()) { }

	virtual ~StmtConverter() {}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECLARATION STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// In clang a declstmt is represented as a list of VarDecl
	stmtutils::StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitReturnStmt(clang::ReturnStmt* retStmt);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FOR STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitForStmt(clang::ForStmt* forStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								IF STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitIfStmt(clang::IfStmt* ifStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							WHILE STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitWhileStmt(clang::WhileStmt* whileStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DO STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitDoStmt(clang::DoStmt* doStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							SWITCH STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitSwitchStmt(clang::SwitchStmt* switchStmt);

	/*
	 * as a CaseStmt or DefaultStmt cannot be converted into any IR statements, we generate an error
	 * in the case the visitor visits one of these nodes, the VisitSwitchStmt has to make sure the
	 * visitor is not called on his subnodes
	 */
	stmtutils::StmtWrapper VisitSwitchCase(clang::SwitchCase* caseStmt);

	stmtutils::StmtWrapper VisitBreakStmt(clang::BreakStmt* breakStmt);

	stmtutils::StmtWrapper VisitContinueStmt(clang::ContinueStmt* contStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitCompoundStmt(clang::CompoundStmt* compStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							NULL STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitNullStmt(clang::NullStmt* nullStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							GOTO STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitGotoStmt(clang::GotoStmt* gotoStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							LABEL STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitLabelStmt(clang::LabelStmt* labelStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ASM STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitAsmStmt(clang::AsmStmt* asmStmt);


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	virtual stmtutils::StmtWrapper Visit(clang::Stmt* stmt) = 0;

	stmtutils::StmtWrapper VisitStmt(clang::Stmt* stmt);
};

//---------------------------------------------------------------------------------------------------------------------
//							C STMT CONVERTER -- takes care of C nodes
//---------------------------------------------------------------------------------------------------------------------
class Converter::CStmtConverter :
	public Converter::StmtConverter,
	public clang::StmtVisitor<Converter::CStmtConverter, stmtutils::StmtWrapper>
{

protected:
	//Converter& convFact;

public:
	CStmtConverter(Converter& convFact) : StmtConverter(convFact) /*, convFact(convFact)*/ {
	}
	virtual ~CStmtConverter() {}

	CALL_BASE_STMT_VISIT(StmtConverter, DeclStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, ReturnStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, ForStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, IfStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, WhileStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, DoStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, SwitchStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, SwitchCase)
	CALL_BASE_STMT_VISIT(StmtConverter, BreakStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, ContinueStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, CompoundStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, NullStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, GotoStmt)
    CALL_BASE_STMT_VISIT(StmtConverter, LabelStmt)
    CALL_BASE_STMT_VISIT(StmtConverter, AsmStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, Stmt)

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Visit(clang::Stmt* stmt);

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(IntegerLiteral)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(FloatingLiteral)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CharacterLiteral)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(StringLiteral)

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(BinaryOperator)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(UnaryOperator)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ConditionalOperator)

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CastExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ImplicitCastExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(PredefinedExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(DeclRefExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ArraySubscriptExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CallExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ParenExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(MemberExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CompoundLiteralExpr)
};

//---------------------------------------------------------------------------------------------------------------------
//							CXX STMT CONVERTER  -- takes care of CXX nodes and C nodes with CXX code mixed in
//---------------------------------------------------------------------------------------------------------------------
class Converter::CXXStmtConverter:
	public Converter::StmtConverter,
	public clang::StmtVisitor<Converter::CXXStmtConverter, stmtutils::StmtWrapper>
{

	Converter& ConvFact;

public:
	CXXStmtConverter(Converter& ConvFact) :
		StmtConverter(ConvFact), ConvFact(ConvFact) {
	}
	virtual ~CXXStmtConverter() {}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECLARATION STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// In clang a declstmt is represented as a list of VarDecl
	stmtutils::StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitReturnStmt(clang::ReturnStmt* retStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper VisitCompoundStmt(clang::CompoundStmt* compStmt);

	stmtutils::StmtWrapper VisitCXXCatchStmt(clang::CXXCatchStmt* catchStmt);
	stmtutils::StmtWrapper VisitCXXTryStmt(clang::CXXTryStmt* tryStmt);
	stmtutils::StmtWrapper VisitCXXForRangeStmt(clang::CXXForRangeStmt* frStmt);

	CALL_BASE_STMT_VISIT(StmtConverter, ForStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, IfStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, WhileStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, DoStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, SwitchStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, SwitchCase)
	CALL_BASE_STMT_VISIT(StmtConverter, BreakStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, ContinueStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, NullStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, GotoStmt)
    CALL_BASE_STMT_VISIT(StmtConverter, LabelStmt)
    CALL_BASE_STMT_VISIT(StmtConverter, AsmStmt)
	CALL_BASE_STMT_VISIT(StmtConverter, Stmt)

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(IntegerLiteral)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(FloatingLiteral)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CharacterLiteral)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(StringLiteral)

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(BinaryOperator)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(UnaryOperator)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ConditionalOperator)

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CastExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ImplicitCastExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(PredefinedExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(DeclRefExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ArraySubscriptExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CallExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ParenExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(MemberExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CompoundLiteralExpr)

	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXConstructExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXNewExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXDeleteExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXThisExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXThrowExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXDefaultArgExpr)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(ExprWithCleanups)
	FORWARD_STMT_TO_EXPR_VISITOR_CALL(MaterializeTemporaryExpr)

	stmtutils::StmtWrapper Visit(clang::Stmt* stmt);
};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
