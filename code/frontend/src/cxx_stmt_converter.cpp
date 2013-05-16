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

// TODO: we might initialize a variable with the coppy constructor, in this case, it will pressent a
// different shape

	std::cout << " ***** INIT ****** " << std::endl;
	declStmt->dump();

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

	vector<core::StatementPtr> stmtList;
	stmtutils::StmtWrapper stmt = StmtConverter::VisitReturnStmt(retStmt);

	if (llvm::isa<clang::IntegerLiteral>(retStmt->getRetValue()) ||
		llvm::isa<clang::BinaryOperator>(retStmt->getRetValue()) || 
		llvm::isa<clang::CXXMemberCallExpr>(retStmt->getRetValue()))
		return stmt;

	core::ExpressionPtr retExpr = stmt.getSingleStmt().as<core::ReturnStmtPtr>().getReturnExpr();

	// check if the return must be converted or not to a reference, 
	// is easy to check if the value has being derefed or not
	if (gen.isPrimitive(retExpr->getType()))
			return stmt;

	// NOTE: if there is a copy constructor inside of the return statement, it should be ignored.
	// this is produced by the AST, but we should delegate this matters to the backend compiler
	

	// if the function returns references, we wont realize, there is no cast, and the inner
	// expresion might have  no reference type
	// if there is no copy constructor on return... it seems that this is the case in which a
	// ref is returned
	// if is a ref: no cast, if is a const ref, there is a Nop cast to qualify

	core::TypePtr funcRetTy = convFact.convertType(retStmt->getRetValue()->getType().getTypePtr());


	std::cout << " return stmt " << std::endl;
	retStmt->dump();
	std::cout << " ret: " << retExpr << std::endl;
	std::cout << " type:     " << retExpr->getType() << std::endl;
	std::cout << " expected: " << funcRetTy << std::endl;

	// we only operate this on classes
	clang::CastExpr* cast;
	clang::CXXConstructExpr* ctorExpr;
	clang::ExprWithCleanups*  cleanups;
	if ((cast = llvm::dyn_cast<clang::CastExpr>(retStmt->getRetValue())) != NULL){
		std::cout << "========= with cast ========================" << std::endl;

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
	std::cout << " ret: " << retExpr << std::endl;
		std::cout << "========= with cast ========================" << std::endl;
	}
	else if ((ctorExpr = llvm::dyn_cast<clang::CXXConstructExpr>(retStmt->getRetValue())) != NULL){
		std::cout << "========= with constructor =====================" << std::endl;

		// of the first node after a return is a constructor, copy constructor
		// we are returning a value.
		retStmt->dump();
		
		std::cout << retExpr<< std::endl;
		std::cout << retExpr->getType()<< std::endl;

		// behind a return we might find a constructor, it might be elidable or not, but we DO NOT
		// call a constructor on return in any case
		if (retExpr->getNodeType() == core::NT_CallExpr){
			if (const core::FunctionTypePtr& ty = retExpr.as<core::CallExprPtr>().getFunctionExpr().getType().as<core::FunctionTypePtr>()){
				if(ty.isConstructor()){
					std::cout << "** removing ctor" << std::endl;
					retExpr = retExpr.as<core::CallExprPtr>()->getArgument(1); // second argument is the copyed obj
				}
			}
		}

		// fist of all, have a look of what is behind the deRef
		if (core::analysis::isCallOf(retExpr,mgr.getLangBasic().getRefDeref())){
			std::cout << "** derefing" << std::endl;
			retExpr = retExpr.as<core::CallExprPtr>()[0];
		}

		std::cout << retExpr<< std::endl;
		std::cout << retExpr->getType()<< std::endl;

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

		std::cout << retExpr<< std::endl;
		std::cout << retExpr->getType()<< std::endl;

		std::cout << "========= with constructor =====================" << std::endl;
	}
	else if ((cleanups= llvm::dyn_cast<clang::ExprWithCleanups>(retStmt->getRetValue())) != NULL){
		std::cout << "========= cleanups ========================" << std::endl;
	}
	else{

		std::cout << "========= ref ========================" << std::endl;
			
		std::cout << retExpr << std::endl;

		// not a cast, it is a ref then... only if not array
		if (!core::analysis::isCallOf(retExpr,mgr.getLangBasic().getScalarToArray()) &&
			!IS_CPP_REF_EXPR(retExpr)){
				retExpr = builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), retExpr);
		}

		std::cout << retExpr << std::endl;
		

		std::cout << "========= ref ========================" << std::endl;
//	// FIXME: solve the issue with the deref of this (return *this;)
//	core::ExpressionPtr myThis  = builder.literal("this", builder.refType(funcRetTy));
//	core::ExpressionPtr retThis = builder.callExpr(gen.getScalarToArray(), myThis);
//	std::cout << "#####################################" << std::endl;
//	retStmt->dump();
//	std::cout << myThis  << std::endl;
//	std::cout << myThis->getType()  << std::endl;
//	std::cout << retThis << std::endl;
//	std::cout << retExpr << std::endl;
//	core::ExpressionPtr refed = builder.callExpr( builder.refType(myThis->getType()), builder.getLangBasic().getArrayRefElem1D(), retThis, builder.uintLit(0));
//	std::cout << refed << std::endl;
//	if(*refed == *retExpr){
//	std::cout << "#####################################" << std::endl;
//	retExpr = myThis;
//	}

//		else if(gen.isRef(retExpr->getType()) && !gen.isRef(funcRetTy)){
//			retExpr = builder.deref(retExpr);
//		}
	}


	stmtList.push_back(builder.returnStmt(retExpr));
	core::StatementPtr retStatement = builder.compoundStmt(stmtList);
	stmt = stmtutils::tryAggregateStmts(builder,stmtList );

	std::cout << "=====================================" << std::endl;
	dumpPretty(funcRetTy);
	retStmt->dump();
	dumpPretty(retExpr);
	std::cout << "=====================================" << std::endl;

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
