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

#include "insieme/frontend/stmt_converter.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/loop_analyzer.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/mpi/mpi_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/annotations/c/location.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/core/transform/node_replacer.h"

using namespace clang;

namespace insieme {
namespace frontend {
namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//							CXX STMT CONVERTER -- takes care of CXX nodes and C nodes with CXX code mixed in
//---------------------------------------------------------------------------------------------------------------------

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

	vector<core::StatementPtr> stmtList;
	stmtutils::StmtWrapper stmt = StmtConverter::VisitReturnStmt(retStmt);
	LOG_STMT_CONVERSION(retStmt, stmt);

	if(!retStmt->getRetValue() ) {
		//if there is no return value its an empty return "return;"
		return stmt;
	}

	if(llvm::isa<clang::IntegerLiteral>(retStmt->getRetValue()) ||
		llvm::isa<clang::BinaryOperator>(retStmt->getRetValue()) ||
		llvm::isa<clang::CXXMemberCallExpr>(retStmt->getRetValue())) {
		return stmt;
	}

	core::ExpressionPtr retExpr = stmt.getSingleStmt().as<core::ReturnStmtPtr>().getReturnExpr();

	// check if the return must be converted or not to a reference,
	// is easy to check if the value has being derefed or not
	if (gen.isPrimitive(retExpr->getType()))
			return stmt;

	// if the function returns references, we wont realize, there is no cast, and the inner
	// expresion might have  no reference type
	// if there is no copy constructor on return... it seems that this is the case in which a
	// ref is returned
	// if is a ref: no cast, if is a const ref, there is a Nop cast to qualify
//	core::TypePtr funcRetTy = convFact.convertType(retStmt->getRetValue()->getType().getTypePtr());

	// we only operate this on classes
	clang::CastExpr* cast;
	clang::CXXConstructExpr* ctorExpr;
	clang::ExprWithCleanups*  cleanups;
	if ((cast = llvm::dyn_cast<clang::CastExpr>(retStmt->getRetValue())) != NULL){
		switch(cast->getCastKind () ){
			case CK_NoOp : // make constant

				if (core::analysis::isCppRef(retExpr->getType())){
					retExpr =  builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp(), retExpr);
				}
				else if (!core::analysis::isConstCppRef(retExpr->getType())){
					retExpr =  builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), retExpr);
				}

				break;
			case CK_LValueToRValue: //deref, not a ref
			default:
				break;
		}
	}
	else if ((ctorExpr = llvm::dyn_cast<clang::CXXConstructExpr>(retStmt->getRetValue())) != NULL){
	// NOTE: if there is a copy constructor inside of the return statement, it should be ignored.
	// this is produced by the AST, but we should delegate this matters to the backend compiler

		// but we DO NOT call a constructor on return in EXCEPT if :
		// 			- when we are actually creating a new object out of more than one paramenter, then we have no way to 
		// 			  return that tuple to construct afterwards
		if (retExpr->getNodeType() == core::NT_CallExpr){
			if (const core::FunctionTypePtr& ty = retExpr.as<core::CallExprPtr>().getFunctionExpr().getType().as<core::FunctionTypePtr>()){
				if(ty.isConstructor() && retExpr.as<core::CallExprPtr>()->getArguments().size()==1){
					retExpr = retExpr.as<core::CallExprPtr>()->getArgument(1); // second argument is the copyed obj
				}
			}
		}

		// fist of all, have a look of what is behind the deRef
		if (core::analysis::isCallOf(retExpr,mgr.getLangBasic().getRefDeref())){
			retExpr = retExpr.as<core::CallExprPtr>()[0];
		}

		if(IS_CPP_REF(retExpr->getType())){
			// we are returning a value, peel out the reference, or deref it
			if (core::analysis::isCallOf(retExpr,mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp()) ||
				core::analysis::isCallOf(retExpr,mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp()))
				retExpr = retExpr.as<core::CallExprPtr>()->getArgument(0);
			else{
				if (core::analysis::isCppRef(retExpr->getType())){
					retExpr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), retExpr);
				}
				else if (core::analysis::isConstCppRef(retExpr->getType())){
					retExpr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), retExpr);
				}
			}
		}

		// return  by value
		if (retExpr->getType().isa<core::RefTypePtr>()) 
			retExpr = builder.deref(retExpr);
	}
	else if ((cleanups= llvm::dyn_cast<clang::ExprWithCleanups>(retStmt->getRetValue())) != NULL){
		// do nothing, should be already OK
	}
	else{
		// not a cast, it is a ref then... only if not array
		if (!core::analysis::isCallOf(retExpr,mgr.getLangBasic().getScalarToArray()) &&
			!IS_CPP_REF(retExpr->getType())){
				retExpr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), retExpr);
		}
	}

	stmtList.push_back(builder.returnStmt(retExpr));
	core::StatementPtr retStatement = builder.compoundStmt(stmtList);
	stmt = stmtutils::tryAggregateStmts(builder,stmtList );

	return stmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							COMPOUND STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCompoundStmt(clang::CompoundStmt* compStmt) {

	return StmtConverter::VisitCompoundStmt(compStmt);
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

	//frontend_assert(false && "Try -- Currently not supported!");
	core::CompoundStmtPtr body = builder.wrapBody( stmtutils::tryAggregateStmts( builder, Visit(tryStmt->getTryBlock()) ) );

	vector<core::CatchClausePtr> catchClauses;
	unsigned numCatch = tryStmt->getNumHandlers();
	for(unsigned i=0;i<numCatch;i++) {
		clang::CXXCatchStmt* catchStmt = tryStmt->getHandler(i);

		core::VariablePtr var;
		if(const clang::VarDecl* exceptionVarDecl = catchStmt->getExceptionDecl() ) {
			core::TypePtr exceptionTy = convFact.convertType(catchStmt->getCaughtType().getTypePtr());

			var = builder.variable(exceptionTy);

			//we assume that exceptionVarDecl is not in the varDeclMap
			frontend_assert(convFact.varDeclMap.find(exceptionVarDecl) == convFact.varDeclMap.end()
					&& "excepionVarDecl already in vardeclmap");
			//insert var to be used in conversion of handlerBlock
			convFact.varDeclMap.insert( { exceptionVarDecl, var } );
			VLOG(2) << convFact.lookUpVariable(catchStmt->getExceptionDecl()).as<core::VariablePtr>();
		}
		else {
			//no exceptiondecl indicates a catch-all (...)
			var = builder.variable(gen.getAny());
		}

		core::CompoundStmtPtr body = builder.wrapBody(stmtutils::tryAggregateStmts(builder, Visit(catchStmt->getHandlerBlock())));
		catchClauses.push_back(builder.catchClause(var, body));
	}

	return stmtutils::tryAggregateStmt(builder, builder.tryCatchStmt(body, catchClauses));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXForRangeStmt(clang::CXXForRangeStmt* frStmt) {
	frontend_assert(false && "ForRange -- Currently not supported!");
	return stmtutils::StmtWrapper();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper Converter::CXXStmtConverter::Visit(clang::Stmt* stmt) {
	VLOG(2) << "CXX";

    //iterate clang handler list and check if a handler wants to convert the stmt
    stmtutils::StmtWrapper retStmt;
	for(auto plugin : convFact.getConversionSetup().getPlugins()) {
        retStmt = plugin->Visit(stmt, convFact);
		if(retStmt.size())
			break;
	}
    if(retStmt.size()==0){
		convFact.trackSourceLocation(stmt->getLocStart());
        retStmt = StmtVisitor<CXXStmtConverter, stmtutils::StmtWrapper>::Visit(stmt);
	}

	// print diagnosis messages
	convFact.printDiagnosis(stmt->getLocStart());

	// build the wrapper for single statements
	if ( retStmt.isSingleStmt() ) {
		core::StatementPtr&& irStmt = retStmt.getSingleStmt();

		// Deal with mpi pragmas
		mpi::attachMPIStmtPragma(irStmt, stmt, convFact);

		// Deal with transfromation pragmas
		pragma::attachPragma(irStmt,stmt,convFact);

		// Deal with omp pragmas
		if ( irStmt->getAnnotations().empty() )
            retStmt = omp::attachOmpAnnotation(irStmt, stmt, convFact);
	}

    // call frontend plugin post visitors
    for(auto plugin : convFact.getConversionSetup().getPlugins()) {
        retStmt = plugin->PostVisit(stmt, retStmt, convFact);
    }

	return  retStmt;
}

}
}
}
