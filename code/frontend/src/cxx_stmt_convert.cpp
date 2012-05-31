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

#include "insieme/frontend/cxx_convert.h"


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
#include "insieme/frontend/cpp/temporary_handler.h"

#include "clang/AST/StmtVisitor.h"

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

#define LOG_CONVERSION(retIr) \
	FinalActions attachLog( [&] () { END_LOG_STMT_CONVERSION(retIr); } )

namespace {

//-------------------------------------------- StmtWrapper ------------------------------------------------------------
/*
 * Utility class used as a return type for the StmtVisitor. It can store a list of statement
 * as conversion of a single C stmt can result in multiple IR statements.
 */
struct StmtWrapper: public StatementList {
	StmtWrapper() :
			StatementList() {
	}
	StmtWrapper(const core::StatementPtr& stmt) :
			StatementList( { stmt }) {
	}

	core::StatementPtr getSingleStmt() const {
		assert(size() == 1 && "More than 1 statement present");
		return front();
	}

	bool isSingleStmt() const {
		return size() == 1;
	}
};

// prototype for below ..
core::StatementPtr tryAggregateStmt(const core::IRBuilder& builder, const core::StatementPtr& stmt);

// Tried to aggregate statements into a compound statement (if more than 1 statement is present)
core::StatementPtr tryAggregateStmts(const core::IRBuilder& builder, const StatementList& stmtVect) {
	if (stmtVect.size() == 1) {
		return tryAggregateStmt(builder, stmtVect.front());
	}
	return builder.compoundStmt(stmtVect);
}

core::StatementPtr tryAggregateStmt(const core::IRBuilder& builder, const core::StatementPtr& stmt) {
	if (stmt->getNodeType() == core::NT_CompoundStmt) {
		return tryAggregateStmts(builder, static_pointer_cast<core::CompoundStmtPtr>(stmt)->getStatements());
	}
	return stmt;
}

core::ExpressionPtr makeOperation(const core::IRBuilder& builder, const core::ExpressionPtr& lhs,
		const core::ExpressionPtr& rhs, const core::lang::BasicGenerator::Operator& op) {
	return builder.callExpr(lhs->getType(), // return type
			builder.getLangBasic().getOperator(lhs->getType(), op), // get the oprtator
			toVector<core::ExpressionPtr>(lhs, rhs) // LHS and RHS of the operation
					);
}

}

namespace insieme {
namespace frontend {
namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 										  Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define FORWARD_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( convFact.convertExpr(stmt) ); }

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
//							CLANG STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------

class CXXConversionFactory::CXXClangStmtConverter: public ConversionFactory::ClangStmtConverter {
	cpp::TemporaryHandler tempHandler;

public:
	CXXClangStmtConverter(CXXConversionFactory& convFact) :
		ClangStmtConverter(convFact), tempHandler(&convFact) {
	}
	virtual ~CXXClangStmtConverter();

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECLARATION STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// In clang a declstmt is represented as a list of VarDecl
	StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt) {
		// if there is only one declaration in the DeclStmt we return it

		core::ExpressionPtr parentThisStack = convFact.ctx.thisStack2;

		if (declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl())) {
			StmtWrapper retList;
			clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(declStmt->getSingleDecl());

			try {
				core::DeclarationStmtPtr&& retStmt = convFact.convertVarDecl(varDecl);

				// check if there is a kernelFile annotation
				ocl::attatchOclAnnotation(retStmt->getInitialization(), declStmt, convFact);
				// handle eventual OpenMP pragmas attached to the Clang node
				retList.push_back( omp::attachOmpAnnotation(retStmt, declStmt, convFact) );

				// convert the constructor of a class
				if ( varDecl->getDefinition()->getInit() ) {
					if(const clang::CXXConstructExpr* ctor =
							dyn_cast<const clang::CXXConstructExpr>(varDecl->getDefinition()->getInit())
					) {
						if(!ctor->getType().getTypePtr()->isArrayType())
							retList.push_back( convFact.convertExpr(ctor));
					}
					if(const clang::ExprWithCleanups* exprWithCleanups =
							dyn_cast<const clang::ExprWithCleanups>(varDecl->getDefinition()->getInit()))
					{
						if(!GET_TYPE_PTR(varDecl)->isReferenceType())
						{
							retList.push_back( convFact.builder.compoundStmt(convFact.convertExpr(exprWithCleanups)));
						}
					}
				}
			} catch ( const GlobalVariableDeclarationException& err ) {
				return StmtWrapper();
			}

			convFact.ctx.thisStack2 = parentThisStack;

			return retList;
		}

		// otherwise we create an an expression list which contains the multiple declaration inside the statement
		StmtWrapper retList;
		for (auto&& it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it )
		if ( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) ) {
			try {
				assert(convFact.currTU&& "translation unit is null");
				core::DeclarationStmtPtr&& retStmt = convFact.convertVarDecl(varDecl);
				// handle eventual OpenMP pragmas attached to the Clang node
				retList.push_back( omp::attachOmpAnnotation(retStmt, declStmt, convFact) );

			} catch ( const GlobalVariableDeclarationException& err ) {}
		}

		convFact.ctx.thisStack2 = parentThisStack;

		return retList;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitReturnStmt(ReturnStmt* retStmt) {
		START_LOG_STMT_CONVERSION(retStmt);

		ConversionFactory::ConversionContext::ScopeObjects parentDownStreamSScopeObjects =
				convFact.ctx.downStreamScopeObjects;
		convFact.ctx.downStreamScopeObjects = convFact.ctx.scopeObjects;

		core::StatementPtr retIr;

		LOG_CONVERSION(retIr);

		core::ExpressionPtr retExpr;
		core::TypePtr retTy;
		if ( Expr* expr = retStmt->getRetValue()) {
			retExpr = convFact.convertExpr(expr);
			retTy = convFact.convertType(expr->getType().getTypePtr());
		} else {
			retExpr = convFact.builder.getLangBasic().getUnitConstant();
			retTy = convFact.builder.getLangBasic().getUnit();
		}

		/*
		 * arrays and vectors in C are always returned as reference, so the type of the return
		 * expression is of array (or vector) type we are sure we have to return a reference, in the
		 * other case we can safely deref the retExpr
		 */
		if (retTy->getNodeType() == core::NT_ArrayType || retTy->getNodeType() == core::NT_VectorType) {

			retTy = convFact.builder.refType(retTy);

		}

		vector<core::StatementPtr> stmtList;

		convFact.stmtConv->tempHandler.handleTemporariesinScope(stmtList, convFact.ctx.downStreamScopeObjects,
				parentDownStreamSScopeObjects, false);

		retIr = convFact.builder.returnStmt(utils::cast(retExpr, retTy));
		stmtList.push_back(retIr);

		core::StatementPtr retStatement = convFact.builder.compoundStmt(stmtList);

		StmtWrapper&& body = tryAggregateStmts(convFact.builder,stmtList );

		convFact.ctx.downStreamScopeObjects = parentDownStreamSScopeObjects;

		return body;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitCompoundStmt(CompoundStmt* compStmt) {

		START_LOG_STMT_CONVERSION(compStmt);
		core::StatementPtr retIr;
		LOG_CONVERSION(retIr);

		ConversionFactory::ConversionContext::ScopeObjects parentScopeObjects = convFact.ctx.scopeObjects;
		while (!convFact.ctx.scopeObjects.empty()) {
			convFact.ctx.scopeObjects.pop();
		}

		bool hasReturn = false;

		vector<core::StatementPtr> stmtList;
		std::for_each(compStmt->body_begin(), compStmt->body_end(), [ &stmtList, this, &hasReturn ] (Stmt* stmt) {
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			// A compoundstmt can contain declaration statements.This means that a clang
			// DeclStmt can be converted in multiple  StatementPtr because an initialization
			// list such as: int a,b=1; is converted into the following sequence of statements:
			//
			// 		int<a> a = 0; int<4> b = 1;
			//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
				StmtWrapper convertedStmt;

				if(dyn_cast<ReturnStmt>(stmt)) {
					hasReturn = true;
				}

				convertedStmt = Visit(stmt);
				copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));

			});

		if (!hasReturn) {

			convFact.stmtConv->tempHandler.handleTemporariesinScope(stmtList, convFact.ctx.scopeObjects,
					parentScopeObjects, false);
		} else {

			convFact.stmtConv->tempHandler.handleTemporariesinScope(convFact.ctx.scopeObjects, parentScopeObjects);
		}

		retIr = convFact.builder.compoundStmt(stmtList);

		convFact.ctx.scopeObjects = parentScopeObjects;

		// check for datarange pragma
		attatchDatarangeAnnotation(retIr, compStmt, convFact);

		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper Visit(clang::Stmt* stmt) {
		StmtWrapper&& retStmt = StmtVisitor<ClangStmtConverter, StmtWrapper>::Visit(stmt);

		if ( retStmt.isSingleStmt() ) {
			core::StatementPtr&& irStmt = retStmt.getSingleStmt();

			// Deal with mpi pragmas
			mpi::attachMPIStmtPragma(irStmt, stmt, convFact);

			// Deal with transfromation pragmas
			pragma::attachPragma(irStmt,stmt,convFact);

			// Deal with omp pragmas
			if ( irStmt->getAnnotations().empty() )
			return omp::attachOmpAnnotation(irStmt, stmt, convFact);
		}
		return retStmt;
	}

	FORWARD_VISITOR_CALL(IntegerLiteral)
	FORWARD_VISITOR_CALL(FloatingLiteral)
	FORWARD_VISITOR_CALL(CharacterLiteral)
	FORWARD_VISITOR_CALL(StringLiteral)

	FORWARD_VISITOR_CALL(BinaryOperator)
	FORWARD_VISITOR_CALL(UnaryOperator)
	FORWARD_VISITOR_CALL(ConditionalOperator)

	FORWARD_VISITOR_CALL(CastExpr)
	FORWARD_VISITOR_CALL(ImplicitCastExpr)
	FORWARD_VISITOR_CALL(PredefinedExpr)
	FORWARD_VISITOR_CALL(DeclRefExpr)
	FORWARD_VISITOR_CALL(ArraySubscriptExpr)
	FORWARD_VISITOR_CALL(CallExpr)
	FORWARD_VISITOR_CALL(ParenExpr)
	FORWARD_VISITOR_CALL(MemberExpr)
	FORWARD_VISITOR_CALL(CompoundLiteralExpr)

	FORWARD_VISITOR_CALL(CXXConstructExpr)
	FORWARD_VISITOR_CALL(CXXNewExpr)
	FORWARD_VISITOR_CALL(CXXDeleteExpr)
	FORWARD_VISITOR_CALL(CXXThisExpr)
	FORWARD_VISITOR_CALL(CXXThrowExpr)
	FORWARD_VISITOR_CALL(CXXDefaultArgExpr)
	FORWARD_VISITOR_CALL(ExprWithCleanups);
//	FORWARD_VISITOR_CALL(MaterializeTemporaryExpr);

	StmtWrapper VisitStmt(Stmt* stmt) {
		std::for_each(stmt->child_begin(), stmt->child_end(),
				[ this ] (Stmt* stmt) {this->Visit(stmt);});
		return StmtWrapper();
	}
}
;

CXXConversionFactory::CXXClangStmtConverter* CXXConversionFactory::makeStmtConvert(CXXConversionFactory& fact) {
	return new CXXConversionFactory::CXXClangStmtConverter(fact);
}

void CXXConversionFactory::cleanStmtConvert(CXXClangStmtConverter* stmtConv) {
	delete stmtConv;
}

core::StatementPtr CXXConversionFactory::convertStmt(const clang::Stmt* stmt) const {
	assert(currTU && "translation unit is null");
	assert(stmt && "Calling convertStmt with a NULL pointer");
	return tryAggregateStmts(builder, stmtConv->Visit(const_cast<Stmt*>(stmt)));
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
