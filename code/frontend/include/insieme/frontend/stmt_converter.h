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

#include "insieme/frontend/convert.h"

/*
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/loop_analyzer.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/utils/ir_cast.h"

#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/mpi/mpi_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/core/transform/node_replacer.h"
*/

#include "insieme/frontend/cpp/temporary_handler.h"

#include "clang/AST/StmtVisitor.h"

namespace stmtutils {

typedef vector<insieme::core::StatementPtr> StatementList;
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

insieme::core::StatementPtr tryAggregateStmt(const insieme::core::IRBuilder& builder, const insieme::core::StatementPtr& stmt);
insieme::core::StatementPtr tryAggregateStmts(const insieme::core::IRBuilder& builder, const StatementList& stmtVect);
insieme::core::ExpressionPtr makeOperation(const insieme::core::IRBuilder& builder, const insieme::core::ExpressionPtr& lhs,
		const insieme::core::ExpressionPtr& rhs, const insieme::core::lang::BasicGenerator::Operator& op);

}

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {

#define FORWARD_STMT_TO_EXPR_VISITOR_CALL(StmtTy) \
	stmtutils::StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return stmtutils::StmtWrapper( convFact.convertExpr(stmt) ); }

#define CALL_BASE_STMT_VISIT(Base, StmtTy) \
	stmtutils::StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return Base::Visit##StmtTy( stmt ); }

#define LOG_STMT_CONVERSION(retIr) \
	FinalActions attachLog( [&] () { END_LOG_STMT_CONVERSION(retIr); } )

#define START_LOG_STMT_CONVERSION(stmt) \
	assert(convFact.currTU); \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting statement [class: '" << stmt->getStmtClassName() << "'] \n" \
			 << "-> at location: (" \
			 << utils::location(stmt->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang statement:\n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		stmt->dump(convFact.currTU->getCompiler().getSourceManager()); \
	}

#define END_LOG_STMT_CONVERSION(stmt) \
	VLOG(1) << "Converted 'statement' into IR stmt: "; \
	VLOG(1) << "\t" << *stmt;


//---------------------------------------------------------------------------------------------------------------------
//							BASE STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::StmtConverter {

protected:
	ConversionFactory& 					convFact;
	core::NodeManager& 					mgr;
	const core::IRBuilder& 				builder;
	const core::lang::BasicGenerator& 	gen;

public:
	StmtConverter(ConversionFactory& convFact) :
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
	stmtutils::StmtWrapper VisitGotoStmt(clang::GotoStmt* gotoStmt);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	virtual stmtutils::StmtWrapper Visit(clang::Stmt* stmt) = 0;

	stmtutils::StmtWrapper VisitStmt(Stmt* stmt);
};

//---------------------------------------------------------------------------------------------------------------------
//							C STMT CONVERTER -- takes care of C nodes
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::CStmtConverter : public ConversionFactory::StmtConverter, public StmtVisitor<ConversionFactory::CStmtConverter, stmtutils::StmtWrapper> {

protected:
	//ConversionFactory& convFact;

public:
	CStmtConverter(ConversionFactory& convFact) : StmtConverter(convFact) /*, convFact(convFact)*/ {
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
class CXXConversionFactory::CXXStmtConverter: public ConversionFactory::StmtConverter, public StmtVisitor<CXXConversionFactory::CXXStmtConverter, stmtutils::StmtWrapper> {
	cpp::TemporaryHandler tempHandler;
	CXXConversionFactory& cxxConvFact;

public:
	CXXStmtConverter(CXXConversionFactory& cxxConvFact) :
		StmtConverter(cxxConvFact), tempHandler(&cxxConvFact), cxxConvFact(cxxConvFact) {
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
