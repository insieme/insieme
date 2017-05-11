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
#include "insieme/frontend/converter.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/frontend_ir.h"

#include "insieme/core/lang/basic.h"

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
		const core::IRBuilder& builder;
		const core::lang::BasicGenerator& basic;

        /**
         * stores conversion map between clang operators and Inspire (BINARY)
         */
		const std::map<clang::BinaryOperator::Opcode, core::lang::BasicGenerator::Operator> binOpMap;

        /**
         * stores conversion map between clang operators and Inspire (UNARY)
         */
		const std::map<clang::UnaryOperator::Opcode, core::lang::BasicGenerator::Operator> unOpMap;

		/**
		 * Handles most of binary conversion, details handled in pointer visitor itself
		 */
		core::ExpressionPtr createBinaryExpression(core::TypePtr exprTy, core::ExpressionPtr left, core::ExpressionPtr right,
			                                       const clang::BinaryOperator* clangBinOp);

		/**
		 * Handles most if unary conversion, common to C & C++
		 */
		core::ExpressionPtr createUnaryExpression(const core::TypePtr& exprTy, const core::ExpressionPtr& subexp, clang::UnaryOperator::Opcode op);

		core::ExpressionPtr BaseVisit(const clang::Expr* expr, std::function<core::ExpressionPtr(const clang::Expr*)> self);

	  public:
		ExprConverter(Converter& converter) : converter(converter), mgr(converter.mgr), builder(converter.builder), basic(converter.builder.getLangBasic()) ,
		binOpMap ({
			{clang::BO_MulAssign, core::lang::BasicGenerator::Mul},    // a *= b
			{clang::BO_DivAssign, core::lang::BasicGenerator::Div},    // a /= b
			{clang::BO_RemAssign, core::lang::BasicGenerator::Mod},    // a %= b
			{clang::BO_AddAssign, core::lang::BasicGenerator::Add},    // a += b
			{clang::BO_SubAssign, core::lang::BasicGenerator::Sub},    // a -= b
			{clang::BO_ShlAssign, core::lang::BasicGenerator::LShift}, // a <<= b
			{clang::BO_ShrAssign, core::lang::BasicGenerator::RShift}, // a >>= b
			{clang::BO_AndAssign, core::lang::BasicGenerator::And},    // a &= b
			{clang::BO_OrAssign, core::lang::BasicGenerator::Or},      // a |= b
			{clang::BO_XorAssign, core::lang::BasicGenerator::Xor},    // a ^= b

			{clang::BO_Add, core::lang::BasicGenerator::Add},    // a + b
			{clang::BO_Sub, core::lang::BasicGenerator::Sub},    // a - b
			{clang::BO_Mul, core::lang::BasicGenerator::Mul},    // a * b
			{clang::BO_Div, core::lang::BasicGenerator::Div},    // a / b
			{clang::BO_Rem, core::lang::BasicGenerator::Mod},    // a % b
			{clang::BO_Shl, core::lang::BasicGenerator::LShift}, // a << b
			{clang::BO_Shr, core::lang::BasicGenerator::RShift}, // a >> b
			{clang::BO_And, core::lang::BasicGenerator::And},    // a & b
			{clang::BO_Xor, core::lang::BasicGenerator::Xor},    // a ^ b
			{clang::BO_Or, core::lang::BasicGenerator::Or},      // a | b

			{clang::BO_LAnd, core::lang::BasicGenerator::LAnd}, // a && b
			{clang::BO_LOr, core::lang::BasicGenerator::LOr},   // a || b

			{clang::BO_LT, core::lang::BasicGenerator::Lt}, // a < b
			{clang::BO_GT, core::lang::BasicGenerator::Gt}, // a > b
			{clang::BO_LE, core::lang::BasicGenerator::Le}, // a <= b
			{clang::BO_GE, core::lang::BasicGenerator::Ge}, // a >= b
			{clang::BO_EQ, core::lang::BasicGenerator::Eq}, // a == b
			{clang::BO_NE, core::lang::BasicGenerator::Ne}, // a != b
		}),
		 unOpMap ({

			{clang::UO_Not, core::lang::BasicGenerator::Not},         // ~a
            {clang::UO_LNot, core::lang::BasicGenerator::LNot},       // !a

            {clang::UO_Plus, core::lang::BasicGenerator::Plus},
            {clang::UO_Minus, core::lang::BasicGenerator::Minus},

			{clang::UO_PreDec, core::lang::BasicGenerator::PreDec},   // --a
			{clang::UO_PreInc, core::lang::BasicGenerator::PreInc},   // ++a
			{clang::UO_PostDec, core::lang::BasicGenerator::PostDec}, // a--
			{clang::UO_PostInc, core::lang::BasicGenerator::PostInc}, // a++

            //{clang::UO_AddrOf, NONE},
            //{clang::UO_Deref, NONE},
            //
            //{clang::UO_Imag, NONE},
            //{clang::UO_Real, NONE},
            //
            //{clang::UO_Extension, NONE}
		})
        { }

		virtual ~ExprConverter() {}

		core::ExpressionPtr convertInitExpr(const clang::Expr* original);
		core::ExpressionPtr convertCxxArgExpr(const clang::Expr* original, const core::TypePtr& targetType = nullptr);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Literals
		core::ExpressionPtr VisitIntegerLiteral(const clang::IntegerLiteral* intLit);
		core::ExpressionPtr VisitFloatingLiteral(const clang::FloatingLiteral* floatLit);
		core::ExpressionPtr VisitCharacterLiteral(const clang::CharacterLiteral* charLit);
		core::ExpressionPtr VisitStringLiteral(const clang::StringLiteral* stringLit);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Operators
		core::ExpressionPtr VisitBinaryOperator(const clang::BinaryOperator* binOp);
		core::ExpressionPtr VisitCompoundAssignOperator(const clang::CompoundAssignOperator* binOp);
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

		core::TypePtr convertExprType(const clang::Expr* expr);
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
		CALL_BASE_EXPR_VISIT(ExprConverter, CompoundAssignOperator)
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
		CALL_BASE_EXPR_VISIT(ExprConverter, AtomicExpr)

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  next methods require a specific implementation on C++
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  C++ only Expressions
		core::ExpressionPtr VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit);
		core::ExpressionPtr VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr);
		core::ExpressionPtr VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr);
		core::ExpressionPtr VisitCXXConstructExpr(const clang::CXXConstructExpr* callExpr);
		core::ExpressionPtr VisitCXXNewExpr(const clang::CXXNewExpr* callExpr);
		core::ExpressionPtr VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr);
		core::ExpressionPtr VisitCXXThisExpr(const clang::CXXThisExpr* callExpr);
		core::ExpressionPtr VisitCXXThrowExpr(const clang::CXXThrowExpr* throwExpr);
		core::ExpressionPtr VisitCXXDefaultArgExpr(const clang::CXXDefaultArgExpr* defaultArgExpr);
		core::ExpressionPtr VisitCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr* scalarValueInit);
		core::ExpressionPtr VisitCXXDefaultInitExpr(const clang::CXXDefaultInitExpr* initExpr);
		core::ExpressionPtr VisitCXXStdInitializerListExpr(const clang::CXXStdInitializerListExpr* stdinitlistexpr);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Temporaries management
		core::ExpressionPtr VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr);
		core::ExpressionPtr VisitMaterializeTemporaryExpr(const clang::MaterializeTemporaryExpr* materTempExpr);
		core::ExpressionPtr VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Operators
		core::ExpressionPtr VisitUnaryOperator(const clang::UnaryOperator* unaryOp);
		core::ExpressionPtr VisitBinaryOperator (const clang::BinaryOperator* binOp);
		core::ExpressionPtr VisitCompoundAssignOperator(const clang::CompoundAssignOperator* binOp);
		core::ExpressionPtr VisitBinPtrMemD(const clang::BinaryOperator* binPtrMemDexpr);
		core::ExpressionPtr VisitBinPtrMemI(const clang::BinaryOperator* binPtrMemIexpr);

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // Traits expressions & templates
		core::ExpressionPtr VisitTypeTraitExpr(const clang::TypeTraitExpr* typeTraitExpr);
		core::ExpressionPtr VisitSizeOfPackExpr(const clang::SizeOfPackExpr* expr);
		core::ExpressionPtr VisitCXXTypeidExpr(const clang::CXXTypeidExpr* typeidExpr);
		core::ExpressionPtr VisitSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr* substExpr);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  Expressions
		core::ExpressionPtr VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr);
		core::ExpressionPtr VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr);
		core::ExpressionPtr VisitCallExpr(const clang::CallExpr* callExpr);
		core::ExpressionPtr VisitMemberExpr(const clang::MemberExpr* memExpr);
		core::ExpressionPtr VisitDeclRefExpr(const clang::DeclRefExpr* declRef);
		core::ExpressionPtr VisitCXXNullPtrLiteralExpr(const clang::CXXNullPtrLiteralExpr* nptrExpr);
		core::ExpressionPtr VisitCXXPseudoDestructorExpr(const clang::CXXPseudoDestructorExpr* pseudo);
		core::ExpressionPtr VisitLambdaExpr(const clang::LambdaExpr* lExpr);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//  default visitor call
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		virtual core::ExpressionPtr Visit(const clang::Expr* expr);
	};

	#undef CALL_BASE_EXPR_VISIT

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
