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


#include "insieme/frontend/extensions/clang_stage_plugin.h"


class Cpp11Plugin : public insieme::frontend::extensions::ClangStagePlugin {

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 expressions
	
	/**
	 *			Cxx11 null pointer
	 *	Cxx11 introduces a null pointer value called nullptr, it avoids typing problems
	 */
	core::ExpressionPtr VisitCXXNullPtrLiteralExpr	(const clang::CXXNullPtrLiteralExpr* nullPtrExpr, 
													 frontend::conversion::Converter& convFact);

	/**
	 *  			Cxx11 lambda expression
	 *  		we could convert the body, encapsulate it into a function and pas all the captures
	 *  		as parameters, but this will ruin compatibility. We need a class with the operator() overload
	 */
	core::ExpressionPtr VisitLambdaExpr (const clang::LambdaExpr* lambdaExpr, frontend::conversion::Converter& convFact) ;

//////////////////////////////////////////////////////////////////////////////////////
//               C++11 types
	
	/**
	 * auto type
	 */
	core::TypePtr VisitAutoType(const clang::AutoType* autoTy, frontend::conversion::Converter& convFact) ;

	/**
	 * decltype(E) is the type ("declared type") of the name or expression E and can be used in declarations. 
	 *	   If you just need the type for a variable that you are about to initialize auto is often a simpler choice. 
	 *	   You really need decltype if you need a type for something that is not a variable, such as a return type. 
	 */
	core::TypePtr VisitDecltypeType(const clang::DecltypeType* declTy, frontend::conversion::Converter& convFact) ;


//////////////////////////////////////////////////////////////////////////////////////
//               Plugin Hooks
	virtual core::ExpressionPtr Visit(const clang::Expr* expr, frontend::conversion::Converter& convFact) {
		if (const clang::LambdaExpr* lambda =  llvm::dyn_cast<clang::LambdaExpr>(expr))
			return VisitLambdaExpr(lambda, convFact);
		if (const clang::CXXNullPtrLiteralExpr* nullExpr =  llvm::dyn_cast<clang::CXXNullPtrLiteralExpr>(expr))
			return VisitCXXNullPtrLiteralExpr(nullExpr, convFact);
		return nullptr;
	}

	virtual core::TypePtr Visit(const clang::Type* type, frontend::conversion::Converter& convFact) {
		if (const clang::AutoType* autoTy =  llvm::dyn_cast<clang::AutoType>(type))
			return VisitAutoType(autoTy, convFact);
		if (const clang::DecltypeType* declTy =  llvm::dyn_cast<clang::DecltypeType>(type))
			return VisitDecltypeType(declTy, convFact);
		return nullptr;
	}

};
