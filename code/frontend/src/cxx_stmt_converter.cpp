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
#include "insieme/frontend/utils/ir_utils.h"
#include "insieme/frontend/utils/debug.h"

#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/mpi/mpi_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/annotations/c/naming.h"
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
	
		// of the first node after a return is a constructor, copy constructor
		// we are returning a value.
		retStmt->dump();
		
		// behind a return we might find a constructor, it might be elidable or not, but we DO NOT
		// call a constructor on return in any case
		if (retExpr->getNodeType() == core::NT_CallExpr){
			if (const core::FunctionTypePtr& ty = retExpr.as<core::CallExprPtr>().getFunctionExpr().getType().as<core::FunctionTypePtr>()){
				if(ty.isConstructor()){
					retExpr = retExpr.as<core::CallExprPtr>()->getArgument(1); // second argument is the copyed obj
				}
			}
		}

		// fist of all, have a look of what is behind the deRef
		if (core::analysis::isCallOf(retExpr,mgr.getLangBasic().getRefDeref())){
			retExpr = retExpr.as<core::CallExprPtr>()[0];
		}

		if(IS_CPP_REF_EXPR(retExpr)){
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
		// this case is by value
		retExpr = builder.deref(retExpr);
	}
	else if ((cleanups= llvm::dyn_cast<clang::ExprWithCleanups>(retStmt->getRetValue())) != NULL){
		// do nothing, should be already OK
	}
	else{
		// not a cast, it is a ref then... only if not array
		if (!core::analysis::isCallOf(retExpr,mgr.getLangBasic().getScalarToArray()) &&
			!IS_CPP_REF_EXPR(retExpr)){
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

	/*
	//START_LOG_STMT_CONVERSION(compStmt);
	core::StatementPtr retIr;
	LOG_STMT_CONVERSION(retIr);

	CXXConverter::CXXConversionContext::ScopeObjects parentScopeObjects = cxxConvFact.cxxCtx.scopeObjects;
	while (!cxxConvFact.cxxCtx.scopeObjects.empty()) {
		cxxConvFact.cxxCtx.scopeObjects.pop();
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
			stmtutils::StmtWrapper convertedStmt;

			if(dyn_cast<clang::ReturnStmt>(stmt)) {
				hasReturn = true;
			}

			convertedStmt = Visit(stmt);
			copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));

		});

	if (!hasReturn) {

		tempHandler.handleTemporariesinScope(stmtList, cxxConvFact.cxxCtx.scopeObjects,
				parentScopeObjects, false);
	} else {

		tempHandler.handleTemporariesinScope(cxxConvFact.cxxCtx.scopeObjects, parentScopeObjects);
	}

	retIr = cxxConvFact.builder.compoundStmt(stmtList);

	cxxConvFact.cxxCtx.scopeObjects = parentScopeObjects;

	// check for datarange pragma
	attatchDatarangeAnnotation(retIr, compStmt, cxxConvFact);

	return retIr;
	*/
}


stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXCatchStmt(clang::CXXCatchStmt* catchStmt) {
	assert(false && "Catch -- Taken care of inside of TryStmt!");
	//return stmtutils::tryAggregateStmts( builder, Visit(catchStmt->getHandlerBlock()) );
	return stmtutils::StmtWrapper();
}

stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXTryStmt(clang::CXXTryStmt* tryStmt) {

	//assert(false && "Try -- Currently not supported!");
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
			assert(convFact.varDeclMap.find(exceptionVarDecl) == convFact.varDeclMap.end()
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

stmtutils::StmtWrapper Converter::CXXStmtConverter::VisitCXXForRangeStmt(clang::CXXForRangeStmt* frStmt) {
	assert(false && "ForRange -- Currently not supported!");
	return stmtutils::StmtWrapper();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper Converter::CXXStmtConverter::Visit(clang::Stmt* stmt) {
	VLOG(2) << "CXX";
	stmtutils::StmtWrapper&& retStmt = StmtVisitor<CXXStmtConverter, stmtutils::StmtWrapper>::Visit(stmt);

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
		return omp::attachOmpAnnotation(irStmt, stmt, convFact);
	}
	return retStmt;
}

}
}
}
