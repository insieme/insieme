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
 */

#pragma once

#include "insieme/frontend/converter.h"

#include "insieme/frontend/clang.h"

#include "insieme/core/forward_decls.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

namespace insieme {
namespace frontend {
namespace conversion {

#define FORWARD_STMT_TO_EXPR_VISITOR_CALL(StmtTy)                                                                                                              \
	stmtutils::StmtWrapper Visit##StmtTy(clang::StmtTy* stmt) {                                                                                                \
		return stmtutils::StmtWrapper(converter.convertExpr(stmt));                                                                                            \
	}

#define CALL_BASE_STMT_VISIT(Base, StmtTy)                                                                                                                     \
	stmtutils::StmtWrapper Visit##StmtTy(clang::StmtTy* stmt) {                                                                                                \
		return Base::Visit##StmtTy(stmt);                                                                                                                      \
	}

	//---------------------------------------------------------------------------------------------------------------------
	//							BASE STMT CONVERTER
	//---------------------------------------------------------------------------------------------------------------------
	class Converter::StmtConverter {
	  protected:
		Converter& converter;
		core::NodeManager& mgr;
		const core::IRBuilder& builder;
		const core::lang::BasicGenerator& gen;

		stmtutils::StmtWrapper BaseVisit(clang::Stmt* stmt, std::function<stmtutils::StmtWrapper(clang::Stmt*)> self);

	  public:
		StmtConverter(Converter& converter) : converter(converter), mgr(converter.mgr), builder(converter.builder), gen(converter.mgr.getLangBasic()) {}

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
	class Converter::CStmtConverter : public Converter::StmtConverter, public clang::StmtVisitor<Converter::CStmtConverter, stmtutils::StmtWrapper> {

	  public:
		CStmtConverter(Converter& converter) : StmtConverter(converter) {}
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
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(StmtExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(AtomicExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(UnaryExprOrTypeTraitExpr)
	};

	//---------------------------------------------------------------------------------------------------------------------
	//							CXX STMT CONVERTER  -- takes care of CXX nodes and C nodes with CXX code mixed in
	//---------------------------------------------------------------------------------------------------------------------
	class Converter::CXXStmtConverter : public Converter::StmtConverter, public clang::StmtVisitor<Converter::CXXStmtConverter, stmtutils::StmtWrapper> {

	  public:
		CXXStmtConverter(Converter& ConvFact) : StmtConverter(ConvFact) {}
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
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(StmtExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(AtomicExpr)

		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXConstructExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXNewExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXDeleteExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXThisExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXThrowExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXDefaultArgExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXNullPtrLiteralExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(ExprWithCleanups)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(MaterializeTemporaryExpr)
		FORWARD_STMT_TO_EXPR_VISITOR_CALL(CXXTypeidExpr)

		stmtutils::StmtWrapper Visit(clang::Stmt* stmt);
	};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
