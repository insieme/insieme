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

#include <stack>

#include "insieme/core/forward_decls.h"

namespace insieme { 
namespace frontend { 
namespace convert {
class ConversionFactory;
} // end convert namespace 
namespace cpp {

typedef std::stack<core::VariablePtr> VariableStack;

class TemporaryHandler {

	conversion::CXXConversionFactory* convFact;

public:
	TemporaryHandler(conversion::CXXConversionFactory* exprConv);

	struct FunctionComponents {
		core::VariableList 		params;
		core::ExpressionList 	args;
		core::ExpressionList 	dtorCalls;
		core::VariableList  	nonRefTemporaries;
		core::VariableList		temporaries;
		core::FunctionTypePtr 	funType;
	};

	core::FunctionTypePtr changeFunctionReturnType(
			const core::IRBuilder& builder, 
			const core::TypePtr& targetType,
			const core::FunctionTypePtr& funcType);


	static bool existInScopeObjectsStack(
			core::VariablePtr var,
			VariableStack scopeObjects,
			const core::IRBuilder& builder);

	void addDtorCallToFunctionBody(StatementList& body,ExpressionList& dtorCall);

	const clang::ValueDecl* getVariableDeclaration(
			core::VariablePtr var,
			std::map<const clang::ValueDecl*, core::VariablePtr>& varDeclMap);

	const clang::Type* getTypeDeclaration(core::TypePtr type,
			std::map<const clang::Type*, core::TypePtr>& typeDeclMap);

	core::VariableList retrieveFunctionTemporaries(
			const clang::FunctionDecl* definition,
			std::map<const clang::FunctionDecl*, core::VariableList>& fun2TempMap);

	void storeFunctionTemporaries(
			const clang::FunctionDecl* definition,
			std::map<const clang::FunctionDecl*, core::VariableList>& fun2TempMap,
			core::VariableList& temporaries);

	core::FunctionTypePtr addThisArgToFunctionType(
			const core::IRBuilder& builder, 
			const core::TypePtr& structTy,
			const core::FunctionTypePtr& funcType);

	void updateFunctionTypeWithTemporaries(
			core::FunctionTypePtr& funcType,
			core::VariableList& temporaries,
			core::ExpressionList& packedArgs);

	void addTemporariesToScope(
			core::VariableList& temporaries,
			VariableStack& scopeObjects);

	const FunctionComponents getTemporaryEffects(
			VariableStack& scopeObjects,
			core::FunctionTypePtr* funType, 
			bool captureTemps);

	void handleTemporariesinScope(
			const clang::FunctionDecl* funcDecl,
			core::FunctionTypePtr& funcType,
			core::VariableList& params,
			VariableStack& scopeObjects, 
			bool storeFunTemps,
			bool addTempsToParams, 
			bool captureTemps);

	void handleTemporariesinScope(
			core::VariableList& params,
			core::StatementList& stmts,
			VariableStack& scopeObjects,
			bool addTempsToParams, 
			bool captureTemps);

	void handleTemporariesinScope(
			core::VariableList& params,
			core::StatementList& stmts,
			core::ExpressionList& args,
			VariableStack& scopeObjects,
			bool addTempsToParams, 
			bool captureTemps);

	void handleTemporariesinScope(
			core::VariableList& params,
			core::StatementList& stmts,
			core::ExpressionList& args,
			VariableStack& scopeObjects,
			VariableStack& parentScopeObjects,
			bool addTempsToParams,
			bool captureTemps); 
/*
	void handleTemporariesinScope(const clang::FunctionDecl* funcDecl, core::FunctionTypePtr& funcType,
			std::vector<core::VariablePtr>& params, std::stack<core::VariablePtr>& scopeObjects, bool storeFunTemps,
			bool addTempsToParams, bool captureTemps);
*/
//	void handleTemporariesinScope(std::vector<core::VariablePtr>& params, std::vector<core::StatementPtr>& stmts,
//			std::stack<core::VariablePtr>& scopeObjects, bool addTempsToParams, bool captureTemps);

	void handleTemporariesinScope(std::vector<core::StatementPtr>& stmts, std::stack<core::VariablePtr>& scopeObjects,
			std::stack<core::VariablePtr>& parentScopeObjects, bool captureTemps);

//	void handleTemporariesinScope(std::vector<core::VariablePtr>& params, std::vector<core::StatementPtr>& stmts,
//			std::vector<core::ExpressionPtr>& args, std::stack<core::VariablePtr>& scopeObjects, bool addTempsToParams,
//			bool captureTemps);
	void handleTemporariesinScope(std::stack<core::VariablePtr>& scopeObjects,
			std::stack<core::VariablePtr>& parentScopeObjects);


};

} } } // end insieme::frontend::cpp namespace 
