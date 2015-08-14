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

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/frontend_ir.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/enum_extension.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

namespace fe = insieme::frontend;

namespace insieme {
namespace frontend {
namespace conversion {
	//---------------------------------------------------------------------------------------------------------------------
	//										BASE EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------
	class Converter::ExprConverter {
	  protected:
		Converter& converter;

		core::NodeManager& mgr;
		const core::FrontendIRBuilder& builder;
		const core::lang::BasicGenerator& basic;

	  public:
		ExprConverter(Converter& converter) : converter(converter), mgr(converter.mgr), builder(converter.builder), basic(converter.builder.getLangBasic()) {}
		virtual ~ExprConverter() {}
		
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Literals
		core::ExpressionPtr VisitIntegerLiteral(const clang::IntegerLiteral* intLit);
		core::ExpressionPtr VisitFloatingLiteral(const clang::FloatingLiteral* floatLit);
		core::ExpressionPtr VisitCharacterLiteral(const clang::CharacterLiteral* charLit);
		core::ExpressionPtr VisitStringLiteral(const clang::StringLiteral* stringLit);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Operators
		core::ExpressionPtr VisitBinaryOperator(const clang::BinaryOperator* binOp);
		core::ExpressionPtr VisitUnaryOperator(const clang::UnaryOperator* unOp);
		core::ExpressionPtr VisitConditionalOperator(const clang::ConditionalOperator* condOp);
		
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Expressions
		core::ExpressionPtr VisitParenExpr(const clang::ParenExpr* parExpr);
		core::ExpressionPtr VisitGNUNullExpr(const clang::GNUNullExpr* nullExpr);
		core::ExpressionPtr VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr);
		core::ExpressionPtr VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr);
		core::ExpressionPtr VisitCastExpr(const clang::CastExpr* castExpr);
		core::ExpressionPtr VisitCallExpr(const clang::CallExpr* callExpr);
		core::ExpressionPtr VisitPredefinedExpr(const clang::PredefinedExpr* preExpr);
		core::ExpressionPtr VisitUnaryExprOrTypeTraitExpr(const clang::UnaryExprOrTypeTraitExpr* expr);
		core::ExpressionPtr VisitMemberExpr(const clang::MemberExpr* membExpr);
		core::ExpressionPtr VisitArraySubscriptExpr(const clang::ArraySubscriptExpr* arraySubExpr);
		core::ExpressionPtr VisitDeclRefExpr(const clang::DeclRefExpr* declRef);
		core::ExpressionPtr VisitInitListExpr(const clang::InitListExpr* initList);
		core::ExpressionPtr VisitCompoundLiteralExpr(const clang::CompoundLiteralExpr* compLitExpr);
		core::ExpressionPtr VisitStmtExpr(const clang::StmtExpr* stmtExpr);
		core::ExpressionPtr VisitImplicitValueInitExpr(const clang::ImplicitValueInitExpr* initExpr);
		core::ExpressionPtr VisitAtomicExpr(const clang::AtomicExpr* atom);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Overwrite the basic visit method for expression in order to automatically
		// and transparently attach annotations to node which are annotated
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		virtual core::ExpressionPtr Visit(const clang::Expr* expr) = 0;
	};

	#define CALL_BASE_EXPR_VISIT(Base, ExprTy)                                                                                                                 \
		core::ExpressionPtr Visit##ExprTy(const clang::ExprTy* expr) {                                                                                         \
			return Base::Visit##ExprTy(expr);                                                                                                                  \
		}

	//---------------------------------------------------------------------------------------------------------------------
	//										C EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------
	class Converter::CExprConverter : public ExprConverter, public clang::ConstStmtVisitor<CExprConverter, core::ExpressionPtr> {
	  public:
		CExprConverter(Converter& converter) : ExprConverter(converter) {}
		virtual ~CExprConverter(){};

		CALL_BASE_EXPR_VISIT(ExprConverter, IntegerLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, FloatingLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, CharacterLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, StringLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, ParenExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, GNUNullExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, ImplicitCastExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, ExplicitCastExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, CastExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, CallExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, PredefinedExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, UnaryExprOrTypeTraitExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, MemberExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, BinaryOperator)
		CALL_BASE_EXPR_VISIT(ExprConverter, UnaryOperator)
		CALL_BASE_EXPR_VISIT(ExprConverter, ConditionalOperator)
		CALL_BASE_EXPR_VISIT(ExprConverter, ArraySubscriptExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, DeclRefExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, InitListExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, CompoundLiteralExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, StmtExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, ImplicitValueInitExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, AtomicExpr)

		virtual core::ExpressionPtr Visit(const clang::Expr* expr);
	};

	//---------------------------------------------------------------------------------------------------------------------
	//										CXX EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------
	class Converter::CXXExprConverter : public ExprConverter, public clang::ConstStmtVisitor<CXXExprConverter, core::ExpressionPtr> {
	  public:
		CXXExprConverter(Converter& ConvFact) : ExprConverter(ConvFact) {}
		virtual ~CXXExprConverter() {}

		CALL_BASE_EXPR_VISIT(ExprConverter, IntegerLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, FloatingLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, CharacterLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, StringLiteral)
		CALL_BASE_EXPR_VISIT(ExprConverter, ParenExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, GNUNullExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, CastExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, PredefinedExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, UnaryExprOrTypeTraitExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, ConditionalOperator)
		CALL_BASE_EXPR_VISIT(ExprConverter, ArraySubscriptExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, InitListExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, CompoundLiteralExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, StmtExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, ImplicitValueInitExpr)
		CALL_BASE_EXPR_VISIT(ExprConverter, BinaryOperator)
		CALL_BASE_EXPR_VISIT(ExprConverter, AtomicExpr)

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  next methods require a specific implementation on C++
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		core::ExpressionPtr VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr);
		core::ExpressionPtr VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr);
		core::ExpressionPtr VisitCallExpr(const clang::CallExpr* callExpr);
		core::ExpressionPtr VisitMemberExpr(const clang::MemberExpr* memExpr);
		core::ExpressionPtr VisitDeclRefExpr(const clang::DeclRefExpr* declRef);
		core::ExpressionPtr VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit);
		core::ExpressionPtr VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr);
		core::ExpressionPtr VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr);
		core::ExpressionPtr VisitCXXConstructExpr(const clang::CXXConstructExpr* callExpr);
		core::ExpressionPtr VisitCXXNewExpr(const clang::CXXNewExpr* callExpr);
		core::ExpressionPtr VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr);
		core::ExpressionPtr VisitCXXThisExpr(const clang::CXXThisExpr* callExpr);
		core::ExpressionPtr VisitCXXThrowExpr(const clang::CXXThrowExpr* throwExpr);
		core::ExpressionPtr VisitCXXDefaultArgExpr(const clang::CXXDefaultArgExpr* defaultArgExpr);
		core::ExpressionPtr VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr);
		core::ExpressionPtr VisitCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr* scalarValueInit);
		core::ExpressionPtr VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr);
		core::ExpressionPtr VisitMaterializeTemporaryExpr(const clang::MaterializeTemporaryExpr* materTempExpr);
		core::ExpressionPtr VisitCXXTypeidExpr(const clang::CXXTypeidExpr* typeidExpr);
		core::ExpressionPtr VisitCXXDefaultInitExpr(const clang::CXXDefaultInitExpr* initExpr);
		core::ExpressionPtr VisitSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr* substExpr);
		core::ExpressionPtr VisitUnaryOperator(const clang::UnaryOperator* unaryOp);
		core::ExpressionPtr VisitBinPtrMemD(const clang::BinaryOperator* binPtrMemDexpr);
		core::ExpressionPtr VisitBinPtrMemI(const clang::BinaryOperator* binPtrMemIexpr);
		core::ExpressionPtr VisitTypeTraitExpr(const clang::TypeTraitExpr* typeTraitExpr);
		core::ExpressionPtr VisitSizeOfPackExpr(const clang::SizeOfPackExpr* expr);
		
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  default visitor call
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		virtual core::ExpressionPtr Visit(const clang::Expr* expr);
	};

	#undef CALL_BASE_EXPR_VISIT

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
