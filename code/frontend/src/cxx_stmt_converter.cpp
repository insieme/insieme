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
stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::VisitDeclStmt(clang::DeclStmt* declStmt) {
	return StmtConverter::VisitDeclStmt(declStmt);

	/*
	// if there is only one declaration in the DeclStmt we return it

	if (declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl())) {
		stmtutils::StmtWrapper retList;
		clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(declStmt->getSingleDecl());

		try {
			core::DeclarationStmtPtr&& retStmt = cxxConvFact.convertVarDecl(varDecl);

			// check if there is a kernelFile annotation
			ocl::attatchOclAnnotation(retStmt->getInitialization(), declStmt, cxxConvFact);
			// handle eventual OpenMP pragmas attached to the Clang node
			retList.push_back( omp::attachOmpAnnotation(retStmt, declStmt, cxxConvFact) );

			// convert the constructor of a class
			if ( varDecl->getDefinition()->getInit() ) {
				if(const clang::CXXConstructExpr* ctor =
						dyn_cast<const clang::CXXConstructExpr>(varDecl->getDefinition()->getInit())
				) {
					if(!ctor->getType().getTypePtr()->isArrayType())
						retList.push_back( cxxConvFact.convertExpr(ctor));
				}
				if(const clang::ExprWithCleanups* exprWithCleanups =
						dyn_cast<const clang::ExprWithCleanups>(varDecl->getDefinition()->getInit()))
				{
					if(!GET_TYPE_PTR(varDecl)->isReferenceType())
					{
						retList.push_back( cxxConvFact.builder.compoundStmt(cxxConvFact.convertExpr(exprWithCleanups)));
					}
				}
			}
		} catch ( const GlobalVariableDeclarationException& err ) {
			return stmtutils::StmtWrapper();
		}

		return retList;
	}

	// otherwise we create an an expression list which contains the multiple declaration inside the statement
	stmtutils::StmtWrapper retList;
	for (auto&& it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it )
	if ( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) ) {
		try {
			assert(cxxConvFact.currTU&& "translation unit is null");
			core::DeclarationStmtPtr&& retStmt = cxxConvFact.convertVarDecl(varDecl);
			// handle eventual OpenMP pragmas attached to the Clang node
			retList.push_back( omp::attachOmpAnnotation(retStmt, declStmt, cxxConvFact) );

		} catch ( const GlobalVariableDeclarationException& err ) {}
	}

	return retList;
	*/
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							RETURN STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::VisitReturnStmt(clang::ReturnStmt* retStmt) {

	stmtutils::StmtWrapper stmt = StmtConverter::VisitReturnStmt(retStmt);
	core::ExpressionPtr retExpr = stmt.getSingleStmt().as<core::ReturnStmtPtr>().getReturnExpr();

	// NOTE: if there is a copy constructor inside of the return statement, it should be ignored.
	// this is produced by the AST, but we should delegate this matters to the backend compiler
	
	// fist of all, have a look of what is behind the deRef
	if (core::analysis::isCallOf(retExpr,mgr.getLangBasic().getRefDeref())){
		retExpr = retExpr.as<core::CallExprPtr>()[0];
	}

	// check if is a ctor
	if (retExpr.isa<core::CallExprPtr>()){
		auto ty = retExpr.as<core::CallExprPtr>().getFunctionExpr().getType().as<core::FunctionTypePtr>();

		if (ty.isConstructor()){
			vector<core::StatementPtr> stmtList;

			// copy ctor, what we actualy want to return is the second param (first is placeholder)
			// if it turns to be a cpp ref, we do not need to do so
			// but it might be that the variable is a ref, so is safer to deref it.
			core::ExpressionPtr ret = retExpr.as<core::CallExprPtr>()[1];
			if (core::analysis::isCppRef(ret->getType())) {
				ret =  builder.deref(builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), ret));
			}
			else if (core::analysis::isConstCppRef(ret->getType())) {
				ret =  builder.deref(builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), ret));
			}

			stmtList.push_back(builder.returnStmt(ret));
			core::StatementPtr retStatement = builder.compoundStmt(stmtList);
			stmt = stmtutils::tryAggregateStmts(builder,stmtList );
		}
	}

	return stmt;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							COMPOUND STATEMENT
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::VisitCompoundStmt(clang::CompoundStmt* compStmt) {

	return StmtConverter::VisitCompoundStmt(compStmt);

	/*
	//START_LOG_STMT_CONVERSION(compStmt);
	core::StatementPtr retIr;
	LOG_STMT_CONVERSION(retIr);

	CXXConversionFactory::CXXConversionContext::ScopeObjects parentScopeObjects = cxxConvFact.cxxCtx.scopeObjects;
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

stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::VisitCXXCatchStmt(clang::CXXCatchStmt* catchStmt) {
	assert(false && "Catch -- Currently not supported!");
	return stmtutils::StmtWrapper();
}

stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::VisitCXXTryStmt(clang::CXXTryStmt* tryStmt) {
	assert(false && "Try -- Currently not supported!");
	return stmtutils::StmtWrapper();
}

stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::VisitCXXForRangeStmt(clang::CXXForRangeStmt* frStmt) {
	assert(false && "ForRange -- Currently not supported!");
	return stmtutils::StmtWrapper();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stmtutils::StmtWrapper ConversionFactory::CXXStmtConverter::Visit(clang::Stmt* stmt) {
	VLOG(2) << "CXX";
	stmtutils::StmtWrapper&& retStmt = StmtVisitor<CXXStmtConverter, stmtutils::StmtWrapper>::Visit(stmt);

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
