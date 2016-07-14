/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/stmt_converter.h"

#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/transform/node_replacer.h"

using namespace clang;

namespace insieme {
namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//							CXX STMT CONVERTER -- takes care of CXX nodes and C nodes with CXX code mixed in
	//---------------------------------------------------------------------------------------------------------------------

	stmtutils::StmtWrapper Converter::CXXStmtConverter::Visit(clang::Stmt* stmt) {
		VLOG(2) << "CXXStmtConverter:\n" << dumpClang(stmt, converter.getSourceManager());
		return BaseVisit(stmt, [&](clang::Stmt* stmt) { return StmtVisitor<CXXStmtConverter, stmtutils::StmtWrapper>::Visit(stmt); });
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECLARATION STATEMENT
	// 			In clang a declstmt is represented as a list of VarDecl
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitDeclStmt(clang::DeclStmt* declStmt) {
		return StmtConverter::VisitDeclStmt(declStmt);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitReturnStmt(clang::ReturnStmt* retStmt) {
		return StmtConverter::VisitReturnStmt(retStmt);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCompoundStmt(clang::CompoundStmt* compStmt) {
		auto resStmt = StmtConverter::VisitCompoundStmt(compStmt);

        return resStmt;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXCatchStmt(clang::CXXCatchStmt* catchStmt) {
		frontend_assert(false && "Catch -- Taken care of inside of TryStmt!");
		return stmtutils::StmtWrapper();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXTryStmt(clang::CXXTryStmt* tryStmt) {
		stmtutils::StmtWrapper stmt;
		LOG_STMT_CONVERSION(tryStmt, stmt);
		frontend_assert(false) << "Try -- Currently not supported!";
		//core::CompoundStmtPtr body = builder.wrapBody(stmtutils::aggregateStmts(builder, Visit(tryStmt->getTryBlock())));

		//vector<core::CatchClausePtr> catchClauses;
		//unsigned numCatch = tryStmt->getNumHandlers();
		//for(unsigned i = 0; i < numCatch; i++) {
		//	clang::CXXCatchStmt* catchStmt = tryStmt->getHandler(i);

		//	core::VariablePtr var;
		//	if(const clang::VarDecl* exceptionVarDecl = catchStmt->getExceptionDecl()) {
		//		core::TypePtr exceptionTy = converter.convertType(catchStmt->getCaughtType());

		//		if(converter.varDeclMap.find(exceptionVarDecl) != converter.varDeclMap.end()) {
		//			// static cast allowed here, because the insertion of
		//			// exceptionVarDecls is exclusively done here
		//			var = (converter.varDeclMap[exceptionVarDecl]).as<core::VariablePtr>();
		//			VLOG(2) << converter.lookUpVariable(catchStmt->getExceptionDecl()).as<core::VariablePtr>();
		//		} else {
		//			var = builder.variable(exceptionTy);

		//			// we assume that exceptionVarDecl is not in the varDeclMap
		//			frontend_assert(converter.varDeclMap.find(exceptionVarDecl) == converter.varDeclMap.end() && "excepionVarDecl already in vardeclmap");
		//			// insert var to be used in conversion of handlerBlock
		//			converter.varDeclMap.insert({exceptionVarDecl, var});
		//			VLOG(2) << converter.lookUpVariable(catchStmt->getExceptionDecl()).as<core::VariablePtr>();
		//		}
		//	} else {
		//		// no exceptiondecl indicates a catch-all (...)
		//		var = builder.variable(gen.getAny());
		//	}

		//	core::CompoundStmtPtr body = builder.wrapBody(stmtutils::aggregateStmts(builder, Visit(catchStmt->getHandlerBlock())));
		//	catchClauses.push_back(builder.catchClause(var, body));
		//}

		//return stmtutils::tryAggregateStmt(builder, builder.tryCatchStmt(body, catchClauses));
		return stmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXForRangeStmt(clang::CXXForRangeStmt* frStmt) {
		frontend_assert(false && "ForRange -- Currently not supported!");
		return stmtutils::StmtWrapper();
	}

}
}
}
