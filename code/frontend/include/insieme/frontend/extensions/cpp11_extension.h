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

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

namespace insieme {
namespace frontend {
namespace extensions {

class Cpp11Extension : public insieme::frontend::extensions::FrontendExtension {


	/**
	 * lambda expressions are used before defined, so we'll map the pointers here, so after definition
	 * we can fix the capture list.
	 */
	std::map <const clang::Decl*, const clang::LambdaExpr*> lambdaMap;

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 stmts

	stmtutils::StmtWrapper VisitCXXForRangeStmt(const clang::CXXForRangeStmt* frStmt, frontend::conversion::Converter& convFact) ;

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 expressions

	/**
	 *			Cxx11 null pointer
	 *	Cxx11 introduces a null pointer value called nullptr, it avoids typing problems
	 */
	insieme::core::ExpressionPtr VisitCXXNullPtrLiteralExpr	(const clang::CXXNullPtrLiteralExpr* nullPtrExpr, insieme::frontend::conversion::Converter& convFact);

	/**
	 *  			Cxx11 lambda expression
	 *  		we could convert the body, encapsulate it into a function and pas all the captures
	 *  		as parameters, but this will ruin compatibility. We need a class with the operator() overload
	 */
	insieme::core::ExpressionPtr VisitLambdaExpr (const clang::LambdaExpr* lambdaExpr, insieme::frontend::conversion::Converter& convFact) ;


	/**
	 *			SizeOfPackExpr
	 */
	insieme::core::ExpressionPtr VisitSizeOfPackExpr(const clang::SizeOfPackExpr* sizeOfPackExpr, insieme::frontend::conversion::Converter& convFact);

	/**
	 *			StdInitListExpr
	 */
	insieme::core::ExpressionPtr VisitInitListExpr(const clang::CXXStdInitializerListExpr* initList, insieme::frontend::conversion::Converter& convFact);

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 types

	/**
	 * auto type
	 */
	insieme::core::TypePtr VisitAutoType(const clang::AutoType* autoTy, insieme::frontend::conversion::Converter& convFact) ;

	/**
	 * decltype(E) is the type ("declared type") of the name or expression E and can be used in declarations.
	 *	   If you just need the type for a variable that you are about to initialize auto is often a simpler choice.
	 *	   You really need decltype if you need a type for something that is not a variable, such as a return type.
	 */
	insieme::core::TypePtr VisitDecltypeType(const clang::DecltypeType* declTy, insieme::frontend::conversion::Converter& convFact) ;

	insieme::core::TypePtr VisitRValueReferenceType(const clang::RValueReferenceType* rvalref, insieme::frontend::conversion::Converter& convFact);

//////////////////////////////////////////////////////////////////////////////////////
//               Extension Hooks

	virtual stmtutils::StmtWrapper Visit (const clang::Stmt* stmt, insieme::frontend::conversion::Converter& convFact) {
		if (const clang::CXXForRangeStmt* fr =  llvm::dyn_cast<clang::CXXForRangeStmt>(stmt))
			return VisitCXXForRangeStmt (fr, convFact);
		return stmtutils::StmtWrapper();
	}

	virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact) {
		if (const clang::LambdaExpr* lambda =  llvm::dyn_cast<clang::LambdaExpr>(expr))
			return VisitLambdaExpr(lambda, convFact);
		if (const clang::CXXNullPtrLiteralExpr* nullExpr =  llvm::dyn_cast<clang::CXXNullPtrLiteralExpr>(expr))
			return VisitCXXNullPtrLiteralExpr(nullExpr, convFact);
		if(const clang::SizeOfPackExpr* sope = llvm::dyn_cast<clang::SizeOfPackExpr>(expr))
			return VisitSizeOfPackExpr(sope, convFact);
	        if(const clang::CXXStdInitializerListExpr* initList = llvm::dyn_cast<clang::CXXStdInitializerListExpr>(expr))
        		return VisitInitListExpr(initList, convFact);
		return nullptr;
	}

	virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& convFact) {
		if (const clang::AutoType* autoTy =  llvm::dyn_cast<clang::AutoType>(type.getTypePtr()))
			return VisitAutoType(autoTy, convFact);
		if (const clang::DecltypeType* declTy =  llvm::dyn_cast<clang::DecltypeType>(type.getTypePtr()))
			return VisitDecltypeType(declTy, convFact);
        if (const clang::RValueReferenceType* rvalRef = llvm::dyn_cast<clang::RValueReferenceType>(type.getTypePtr()))
			return VisitRValueReferenceType(rvalRef, convFact);
		return nullptr;
	}

	/**
	 * a post visit in needed to:
	 * 		- fix captured variables in lambdas
	 */
	virtual core::ExpressionPtr FuncDeclPostVisit(const clang::FunctionDecl* decl, core::ExpressionPtr expr, frontend::conversion::Converter& convFact, bool symbolic=false);

public:
	virtual FrontendExtension::flagHandler registerFlag(boost::program_options::options_description& options);

};

} //namespace extensions
} //namespace frontend
} //namespace insieme
