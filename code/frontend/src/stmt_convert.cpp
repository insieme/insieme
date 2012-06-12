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
#include "insieme/frontend/stmt_converter.h"

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

//#define LOG_CONVERSION(retIr) \
//	FinalActions attachLog( [&] () { END_LOG_STMT_CONVERSION(retIr); } )

namespace insieme {
namespace frontend {
namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 										  Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define FORWARD_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( convFact.convertExpr(stmt) ); }

//#define START_LOG_STMT_CONVERSION(stmt) \
//	assert(convFact.currTU); \
//	VLOG(1) << "\n****************************************************************************************\n" \
//			 << "Converting statement [class: '" << stmt->getStmtClassName() << "'] \n" \
//			 << "-> at location: (" \
//			 << utils::location(stmt->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
//	if( VLOG_IS_ON(2) ) { \
//		VLOG(2) << "Dump of clang statement:\n" \
//				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
//		stmt->dump(convFact.currTU->getCompiler().getSourceManager()); \
//	}
//
//#define END_LOG_STMT_CONVERSION(stmt) \
//	VLOG(1) << "Converted 'statement' into IR stmt: "; \
//	VLOG(1) << "\t" << *stmt;

//forward Stmts from CXXExtSmt to CXXStmt
#define FORWARD_CXXEXT_TO_CXX_STMT_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( cxxConvFact.convertCXXStmt(stmt) ); }

//forward Exprs from CStmt to CExpr
#define FORWARD_STMT_TO_EXPR_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( convFact.convertExpr(stmt) ); }

//---------------------------------------------------------------------------------------------------------------------
//			ConversionFactory utility functions for CLANG STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------
ConversionFactory::ClangStmtConverter* ConversionFactory::makeStmtConvert(ConversionFactory& fact) {
	return new ClangStmtConverter(fact);
}

void ConversionFactory::cleanStmtConvert(ClangStmtConverter* stmtConv) {
	delete stmtConv;
}

core::StatementPtr ConversionFactory::convertStmt(const clang::Stmt* stmt) const {
	assert(currTU && "translation unit is null");
	assert(stmt && "Calling convertStmt with a NULL pointer");
	return tryAggregateStmts(builder, stmtConv->Visit(const_cast<Stmt*>(stmt)));
}

//---------------------------------------------------------------------------------------------------------------------
//			ConversionFactory utility functions for CLANG CXX Extension STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------
CXXConversionFactory::CXXExtStmtConverter* CXXConversionFactory::makeStmtConvert(CXXConversionFactory& fact) {
	return new CXXExtStmtConverter(fact);
}

void CXXConversionFactory::cleanStmtConvert(CXXExtStmtConverter* stmtConv) {
	delete stmtConv;
}

//---------------------------------------------------------------------------------------------------------------------
//			ConversionFactory utility functions for CXX STMT CONVERTER
//---------------------------------------------------------------------------------------------------------------------

//class CXXConversionFactory::CXXStmtConverter : public StmtVisitor<CXXStmtConverter, stmtutils::StmtWrapper> {
//	cpp::TemporaryHandler tempHandler;
//	CXXConversionFactory& cxxConvFact;
//
//public:
//	CXXStmtConverter(CXXConversionFactory& cxxConvFact) :
//				tempHandler(&cxxConvFact), cxxConvFact(cxxConvFact) {
//	}
//	virtual ~CXXStmtConverter() {};
//
//	//TODO: take care of CXXCatch/CXX.../... stmts
//	//StmtWrapper VisitCXXCatchStmt(CXXCatchStmt* catchStmt) {
//	// ....
//	//}
//
//	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	// Overwrite the basic visit method for expression in order to automatically
//	// and transparently attach annotations to node which are annotated
//	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	stmtutils::StmtWrapper Visit(clang::Stmt* stmt) {
//		stmtutils::StmtWrapper&& retStmt = StmtVisitor<CXXStmtConverter, stmtutils::StmtWrapper>::Visit(stmt);
//
//		if ( retStmt.isSingleStmt() ) {
//			core::StatementPtr&& irStmt = retStmt.getSingleStmt();
//
//			// Deal with mpi pragmas
//			mpi::attachMPIStmtPragma(irStmt, stmt, cxxConvFact);
//
//			// Deal with transfromation pragmas
//			pragma::attachPragma(irStmt,stmt,cxxConvFact);
//
//			// Deal with omp pragmas
//			if ( irStmt->getAnnotations().empty() )
//			return omp::attachOmpAnnotation(irStmt, stmt, cxxConvFact);
//		}
//		return retStmt;
//	}
//
//	stmtutils::StmtWrapper VisitStmt(Stmt* stmt) {
//		 return stmtutils::StmtWrapper( cxxConvFact.convertStmt(stmt) );
//	}
//};

CXXConversionFactory::CXXStmtConverter*
CXXConversionFactory::makeCXXStmtConvert(CXXConversionFactory& fact) {
	return new CXXConversionFactory::CXXStmtConverter(fact);
}

void CXXConversionFactory::cleanCXXStmtConvert(CXXStmtConverter* stmtConv) {
	delete stmtConv;
}

core::StatementPtr CXXConversionFactory::convertCXXStmt(const clang::Stmt* stmt) const {
	assert(currTU && "translation unit is null");
	assert(stmt && "Calling convertStmt with a NULL pointer");
	return stmtutils::tryAggregateStmts(builder, cxxStmtConv->Visit(const_cast<Stmt*>(stmt)));
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
