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
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/functionDependencyGraph.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"

#include "clang/AST/StmtVisitor.h"

// [3.0]
//#include "clang/Index/Entity.h"
//#include "clang/Index/Indexer.h"

namespace fe = insieme::frontend;

namespace exprutils {

using namespace insieme;

// FIXME 
// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
annotations::c::SourceLocation convertClangSrcLoc(clang::SourceManager& sm, const clang::SourceLocation& loc);

// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const clang::SourceManager& srcMgr, const SourceLocation& start);
/*
 * In case the the last argument of the function is a var_arg, we try pack the exceeding arguments
 * with the pack operation provided by the IR.
 */
vector<core::ExpressionPtr> tryPack(const core::IRBuilder& builder, core::FunctionTypePtr funcTy, const ExpressionList& args);

core::CallExprPtr getSizeOfType(const core::IRBuilder& builder, const core::TypePtr& type);
/**
 * Special method which handle malloc and calloc which need to be treated in a special way in the IR. 
 */
core::ExpressionPtr handleMemAlloc(const core::IRBuilder& builder, const core::TypePtr& type, const core::ExpressionPtr& subExpr);

core::ExpressionPtr getCArrayElemRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr);

core::ExpressionPtr scalarToVector(core::ExpressionPtr scalarExpr, core::TypePtr refVecTy,
		const core::IRBuilder& builder, const frontend::conversion::ConversionFactory& convFact);

} // end anonymous namespace

namespace insieme {
namespace frontend {
namespace conversion {

#define CALL_BASE_EXPR_VISIT(Base, ExprTy) \
	core::ExpressionPtr Visit##ExprTy( clang::ExprTy* expr ) { return Base::Visit##ExprTy( expr ); }

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())

#define GET_VEC_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::VectorType>(type)->getElementType())

#define GET_ARRAY_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::ArrayType>(type)->getElementType())

#define LOG_EXPR_CONVERSION(retIr) \
	FinalActions attachLog( [&] () { END_LOG_EXPR_CONVERSION(retIr); } )

#define START_LOG_EXPR_CONVERSION(expr) \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting expression [class: '" << expr->getStmtClassName() << "']\n" \
			 << "-> at location: (" <<	\
				utils::location(expr->getLocStart(), convFact.currTU.top()->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang expression: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		expr->dump(); \
	}

#define END_LOG_EXPR_CONVERSION(expr) \
	VLOG(1) << "Converted into IR expression: "; \
	VLOG(1) << "\t" << *expr << " type:( " << *expr->getType() << " )";

//---------------------------------------------------------------------------------------------------------------------
//										BASE EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::ExprConverter {
protected:
	ConversionFactory& convFact;
	ConversionContext& ctx;

	core::NodeManager& 					mgr;
	const core::IRBuilder& 				builder;
	const core::lang::BasicGenerator& 	gen;

	core::ExpressionPtr wrapVariable(clang::Expr* expr);

	core::ExpressionPtr asLValue(const core::ExpressionPtr& value);
	core::ExpressionPtr asRValue(const core::ExpressionPtr& value);

	template<class ClangExprTy>
	ExpressionList getFunctionArguments(const core::IRBuilder& builder, ClangExprTy* callExpr,
			const core::FunctionTypePtr& funcTy);

public:
	// CallGraph for functions, used to resolved eventual recursive functions

	ExprConverter(ConversionFactory& convFact, Program& program) :
		convFact(convFact),
		ctx(convFact.ctx),
		mgr(convFact.mgr),
		builder(convFact.builder),
		gen(convFact.builder.getLangBasic())
	{  }

	virtual ~ExprConverter() { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitIntegerLiteral(clang::IntegerLiteral* intLit);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FLOATING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitFloatingLiteral(clang::FloatingLiteral* floatLit);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CHARACTER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCharacterLiteral(clang::CharacterLiteral* charLit);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								STRING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitStringLiteral(clang::StringLiteral* stringLit);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitParenExpr(clang::ParenExpr* parExpr);
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
	core::ExpressionPtr VisitGNUNullExpr(clang::GNUNullExpr* nullExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* castExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExplicitCastExpr(clang::ExplicitCastExpr* castExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCastExpr(clang::CastExpr* castExpr);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PREDEFINED EXPRESSION
	//
	// [C99 6.4.2.2] - A predefined identifier such as __func__.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitPredefinedExpr(clang::PredefinedExpr* preExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						SIZEOF ALIGNOF EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//core::ExpressionPtr VisitSizeOfAlignOfExpr(clang::SizeOfAlignOfExpr* expr) {
	//START_LOG_EXPR_CONVERSION(expr);

	//core::ExpressionPtr irNode;
	//LOG_CONVERSION(irNode);

	//if ( expr->isSizeOf() ) {
	//core::TypePtr&& type = expr->isArgumentType() ?
	//convFact.convertType( expr->getArgumentType().getTypePtr() ) :
	//convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
	//return (irNode = getSizeOfType(convFact.getIRBuilder(), type));
	//}
	//assert(false && "SizeOfAlignOfExpr not yet supported");
	//}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						UnaryExprOrTypeTraitExpr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// UnaryExprOrTypeTraitExpr - expression with either a type or (unevaluated)
	// expression operand. Used for sizeof/alignof (C99 6.5.3.4) and vec_step
	// (OpenCL 1.1 6.11.12).
	core::ExpressionPtr VisitUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr* expr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							MEMBER EXPRESSION
	//
	// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitMemberExpr(clang::MemberExpr* membExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitBinaryOperator(clang::BinaryOperator* binOp);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitUnaryOperator(clang::UnaryOperator *unOp);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CONDITIONAL OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitConditionalOperator(clang::ConditionalOperator* condOp);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXT VECTOR ELEMENT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExtVectorElementExpr(clang::ExtVectorElementExpr* vecElemExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  VECTOR/STRUCT INITALIZATION EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitInitListExpr(clang::InitListExpr* initList);
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
	core::ExpressionPtr VisitCompoundLiteralExpr(clang::CompoundLiteralExpr* compLitExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	virtual core::ExpressionPtr Visit(clang::Expr* expr) = 0;
};

//---------------------------------------------------------------------------------------------------------------------
//										C EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::CExprConverter: public ExprConverter, public clang::StmtVisitor<CExprConverter, core::ExpressionPtr> {
protected:
//	ConversionFactory& convFact;
//	ConversionContext& ctx;
//
//	core::ExpressionPtr wrapVariable(clang::Expr* expr);
//
//	core::ExpressionPtr asLValue(const core::ExpressionPtr& value);
//	core::ExpressionPtr asRValue(const core::ExpressionPtr& value);
//
//	template<class ClangExprTy>
//	ExpressionList getFunctionArguments(const core::IRBuilder& builder, ClangExprTy* callExpr,
//			const core::FunctionTypePtr& funcTy);

public:

	CExprConverter(ConversionFactory& convFact, Program& program) :
		ExprConverter(convFact, program) {}
	virtual ~CExprConverter() {};

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
	CALL_BASE_EXPR_VISIT(ExprConverter, ExtVectorElementExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, DeclRefExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, InitListExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, CompoundLiteralExpr)

	virtual core::ExpressionPtr Visit(clang::Expr* expr);
};

//---------------------------------------------------------------------------------------------------------------------
//										CXX EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::CXXExprConverter : 
	public ExprConverter, 
	public clang::StmtVisitor<CXXExprConverter, core::ExpressionPtr> 
{

public:

	CXXExprConverter(ConversionFactory& ConvFact, Program& program) :
		ExprConverter(ConvFact, program){}
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
	CALL_BASE_EXPR_VISIT(ExprConverter, MemberExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, BinaryOperator)
	CALL_BASE_EXPR_VISIT(ExprConverter, UnaryOperator)
	CALL_BASE_EXPR_VISIT(ExprConverter, ConditionalOperator)
	CALL_BASE_EXPR_VISIT(ExprConverter, ArraySubscriptExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, ExtVectorElementExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, InitListExpr)
	CALL_BASE_EXPR_VISIT(ExprConverter, CompoundLiteralExpr)


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* castExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExplicitCastExpr(clang::ExplicitCastExpr* castExpr);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CXX BOOLEAN LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXBoolLiteralExpr(clang::CXXBoolLiteralExpr* boolLit);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX MEMBER CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX OPERATOR CALL EXPRESSION
	//
	//  A call to an overloaded operator written using operator syntax.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX CONSTRUCTOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXConstructExpr(clang::CXXConstructExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX NEW CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXNewExpr(clang::CXXNewExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX DELETE CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXDeleteExpr(clang::CXXDeleteExpr* deleteExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX THIS CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXThisExpr(clang::CXXThisExpr* callExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					EXCEPTION CXX THROW EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXThrowExpr(clang::CXXThrowExpr* throwExpr);
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX DEFAULT ARG EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXDefaultArgExpr(clang::CXXDefaultArgExpr* defaultArgExpr);

	core::ExpressionPtr VisitCXXBindTemporaryExpr(clang::CXXBindTemporaryExpr* bindTempExpr);

	core::ExpressionPtr VisitExprWithCleanups(clang::ExprWithCleanups* cleanupExpr);

	core::ExpressionPtr VisitMaterializeTemporaryExpr(clang::MaterializeTemporaryExpr* materTempExpr);

	virtual core::ExpressionPtr Visit(clang::Expr* expr);
};


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
