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
#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/frontend_ir.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/enum_extension.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/types/cast_tool.h"


namespace fe = insieme::frontend;

namespace exprutils {

	using namespace insieme;

	// FIXME cleanup this namespace, document and find out if there is real usage

	/**
	 * Returns a string of the text within the source range of the input stream
	 */
	std::string GetStringFromStream(const clang::SourceManager& srcMgr, const SourceLocation& start);

	/**
	 * Special method which handle malloc and calloc which need to be treated in a special way in the IR.
	 */
	core::ExpressionPtr getCArrayElemRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr);

	core::ExpressionPtr scalarToVector(core::ExpressionPtr scalarExpr, core::TypePtr refVecTy, const core::IRBuilder& builder,
	                                   const frontend::conversion::Converter& convFact);

	/**
	 * builds a member access expresion, does conversion needed on base regarding pointer usage, and in
	 * the indentifier regarding annonimous members.
	 */
	core::ExpressionPtr getMemberAccessExpr(frontend::conversion::Converter& converter, const core::IRBuilder& builder, core::ExpressionPtr base,
	                                        const clang::MemberExpr* membExpr);


} // end anonymous namespace
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace insieme {
namespace frontend {
namespace conversion {
	//---------------------------------------------------------------------------------------------------------------------
	//										BASE EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------
	class Converter::ExprConverter {
	  protected:
		Converter& convFact;

		core::NodeManager& mgr;
		const core::FrontendIRBuilder& builder;
		const core::lang::BasicGenerator& gen;

		core::ExpressionPtr wrapVariable(const clang::Expr* expr);

		core::ExpressionPtr asLValue(const core::ExpressionPtr& value);
		core::ExpressionPtr asRValue(const core::ExpressionPtr& value);

		template <class ClangExprTy>
		ExpressionList getFunctionArguments(ClangExprTy* callExpr, const clang::FunctionDecl* declaration) {
			const core::FunctionTypePtr& funcTy = convFact.convertFunctionType(declaration).as<core::FunctionTypePtr>();
			return getFunctionArguments(callExpr, funcTy);
		}

		template <class ClangExprTy>
		ExpressionList getFunctionArguments(ClangExprTy* callExpr, const core::FunctionTypePtr& funcTy) {
			ExpressionList args;

			// if member function, need to skip one arg (the local scope arg)
			int off = 0;
			if(funcTy->isMemberFunction() || funcTy->isConstructor() || funcTy->isDestructor()) { off = 1; }

			// FIXME find a cleaner solution
			size_t argIdOffSet = 0;
			// for CXXOperatorCallExpr we need to take care of the "this" arg separately
			// is a memberfunctioncall -- arg(0) == this
			if(const clang::CXXOperatorCallExpr* oc = llvm::dyn_cast<clang::CXXOperatorCallExpr>(callExpr)) {
				if(llvm::isa<clang::CXXMethodDecl>(oc->getCalleeDecl())) {
					argIdOffSet = 1;
					off = 0;
					VLOG(2) << " == Operator call == ";
				}
			}

			for(size_t argId = argIdOffSet, end = callExpr->getNumArgs(); argId < end; ++argId) {
				core::ExpressionPtr&& arg = Visit(callExpr->getArg(argId));
				if(argId < funcTy->getParameterTypes().size()) {
					arg = fixType(arg, funcTy->getParameterTypes()[argId + off]);
				} else {
					arg = core::types::smartCast(builder.getNodeManager().getLangBasic().getVarList(), arg);
				}
				args.push_back(arg);
			}
			return args;
		}

	  public:
		// CallGraph for functions, used to resolved eventual recursive functions

		ExprConverter(Converter& convFact) : convFact(convFact), mgr(convFact.mgr), builder(convFact.builder), gen(convFact.builder.getLangBasic()) {}

		virtual ~ExprConverter() {}

		core::ExpressionPtr fixType(const core::ExpressionPtr& expr, const core::TypePtr& targetType);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//								INTEGER LITERAL
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitIntegerLiteral(const clang::IntegerLiteral* intLit);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//								FLOATING LITERAL
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitFloatingLiteral(const clang::FloatingLiteral* floatLit);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//								CHARACTER LITERAL
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitCharacterLiteral(const clang::CharacterLiteral* charLit);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//								STRING LITERAL
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitStringLiteral(const clang::StringLiteral* stringLit);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							PARENTESIS EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitParenExpr(const clang::ParenExpr* parExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//					      GNU NULL EXPR EXPRESSION
		//
		// GNUNullExpr - Implements the GNU __null extension, which is a name for a
		// null pointer constant that has integral type (e.g., int or long) and is
		// the same size and alignment as a pointer. The __null extension is
		// typically only used by system headers, which define NULL as __null in
		// C++ rather than using 0 (which is an integer that may not match the size
		// of a pointer).
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitGNUNullExpr(const clang::GNUNullExpr* nullExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						  IMPLICIT CAST EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						EXPLICIT CAST EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//								CAST EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitCastExpr(const clang::CastExpr* castExpr);

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							FUNCTION CALL EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitCallExpr(const clang::CallExpr* callExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							PREDEFINED EXPRESSION
		//
		// [C99 6.4.2.2] - A predefined identifier such as __func__.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitPredefinedExpr(const clang::PredefinedExpr* preExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						SIZEOF ALIGNOF EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// core::ExpressionPtr VisitSizeOfAlignOfExpr(const clang::SizeOfAlignOfExpr* expr) {
		// START_LOG_EXPR_CONVERSION(expr);

		// core::ExpressionPtr irNode;
		// LOG_CONVERSION(irNode);

		// if ( expr->isSizeOf() ) {
		// core::TypePtr&& type = expr->isArgumentType() ?
		// convFact.convertType( expr->getArgumentType().getTypePtr() ) :
		// convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
		// return (irNode = getSizeOfType(convFact.getIRBuilder(), type));
		//}
		// assert_fail() << "SizeOfAlignOfExpr not yet supported";
		//}

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						UnaryExprOrTypeTraitExpr
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// UnaryExprOrTypeTraitExpr - expression with either a type or (unevaluated)
		// expression operand. Used for sizeof/alignof (C99 6.5.3.4) and vec_step
		// (OpenCL 1.1 6.11.12).
		core::ExpressionPtr VisitUnaryExprOrTypeTraitExpr(const clang::UnaryExprOrTypeTraitExpr* expr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							MEMBER EXPRESSION
		//
		// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitMemberExpr(const clang::MemberExpr* membExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							BINARY OPERATOR
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitBinaryOperator(const clang::BinaryOperator* binOp);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							UNARY OPERATOR
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitUnaryOperator(const clang::UnaryOperator* unOp);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							CONDITIONAL OPERATOR
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitConditionalOperator(const clang::ConditionalOperator* condOp);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						ARRAY SUBSCRIPT EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitArraySubscriptExpr(const clang::ArraySubscriptExpr* arraySubExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//							VAR DECLARATION REFERENCE
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitDeclRefExpr(const clang::DeclRefExpr* declRef);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//                  VECTOR/STRUCT INITALIZATION EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitInitListExpr(const clang::InitListExpr* initList);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//                  	COMPOUND LITERAL EXPRESSION
		// Introduced in C99 6.5.2.5, used to initialize structures or arrays with
		// the { } expression, example:
		// 		strcut A a;
		// 		a = (struct A) { 10, 20, 30 };
		//
		//	or:
		//		((int [3]){1,2,3})[2]  -> 2
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitCompoundLiteralExpr(const clang::CompoundLiteralExpr* compLitExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//                  StmtExpr EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitStmtExpr(const clang::StmtExpr* stmtExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//                  ImplicitValueInit EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::ExpressionPtr VisitImplicitValueInitExpr(const clang::ImplicitValueInitExpr* initExpr);
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//                  Atomic EXPRESSION
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
		CExprConverter(Converter& convFact) : ExprConverter(convFact) {}
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


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace		return smartCast(type, expr);
