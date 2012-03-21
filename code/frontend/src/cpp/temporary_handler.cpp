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

#include "insieme/frontend/convert.h"

#include "insieme/frontend/cpp/temporary_handler.h"

namespace insieme {
namespace frontend {
namespace cpp {

using namespace clang;

TemporaryHandler::TemporaryHandler(conversion::ConversionFactory* fact) :
		convFact(fact) {
}

core::FunctionTypePtr TemporaryHandler::changeFunctionReturnType(const core::IRBuilder& builder,
		const core::TypePtr& targetType, const core::FunctionTypePtr& funcType) {
	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	return builder.functionType(oldArgs, targetType);

}

bool TemporaryHandler::existInScopeObjectsStack(core::VariablePtr var, VariableStack scopeObjects,
		const core::IRBuilder& builder) {
	while (!scopeObjects.empty()) {

		core::VariablePtr stackVar = scopeObjects.top();
		scopeObjects.pop();
		if (var == stackVar || var == builder.deref(stackVar)) {
			return true;
		}
	}
	return false;
}

void TemporaryHandler::addDtorCallToFunctionBody(vector<core::StatementPtr>& body,
		vector<core::ExpressionPtr>& dtorCall) {

	vector<core::StatementPtr>::iterator bodyIt;

	for (bodyIt = body.begin(); bodyIt != body.end(); bodyIt++) {

		if (core::dynamic_pointer_cast<const core::ReturnStmtPtr>(*bodyIt)) {
			break;
		}
	}
	body.insert(bodyIt, dtorCall.begin(), dtorCall.end());
}

const ValueDecl* TemporaryHandler::getVariableDeclaration(core::VariablePtr var,
		std::map<const clang::ValueDecl*, core::VariablePtr>& varDeclMap) {

	const ValueDecl* varDecl = 0;

	for (std::map<const clang::ValueDecl*, core::VariablePtr>::const_iterator it = varDeclMap.begin();
			it != varDeclMap.end(); ++it) {

		if (it->second == var) {
			varDecl = it->first;
		}
	}
	return varDecl;
}

const Type* TemporaryHandler::getTypeDeclaration(core::TypePtr type,
		std::map<const clang::Type*, core::TypePtr>& typeDeclMap) {

	const Type* typeDecl = 0;

	for (std::map<const clang::Type*, core::TypePtr>::const_iterator it = typeDeclMap.begin(); it != typeDeclMap.end();
			++it) {

		if (it->second == type) {
			typeDecl = it->first;
		}
	}
	return typeDecl;
}

std::vector<core::VariablePtr> TemporaryHandler::retrieveFunctionTemporaries(const FunctionDecl* definition,
		std::map<const clang::FunctionDecl*, vector<insieme::core::VariablePtr>>& fun2TempMap) {

	std::vector<core::VariablePtr> temporaries;

	std::map<const clang::FunctionDecl*, vector<insieme::core::VariablePtr>>::const_iterator tempit = fun2TempMap.find(
			definition);
	if (tempit != fun2TempMap.end()) {

		temporaries = tempit->second;
	}
	return temporaries;
}

void TemporaryHandler::storeFunctionTemporaries(const FunctionDecl* definition,
		std::map<const clang::FunctionDecl*, vector<insieme::core::VariablePtr>>& fun2TempMap
		,std::vector<core::VariablePtr>& temporaries) {

	std::map<const clang::FunctionDecl*, vector<insieme::core::VariablePtr>>::iterator tempit = fun2TempMap.find(
			definition);

	if (tempit != fun2TempMap.end()) {

		tempit->second = temporaries;

	} else {

		fun2TempMap.insert(std::make_pair(definition, temporaries));
	}
}

core::FunctionTypePtr TemporaryHandler::addThisArgToFunctionType(const core::IRBuilder& builder,
		const core::TypePtr& structTy, const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin());
	// move THIS to the last position
	argTypes[oldArgs.size()] = builder.refType(structTy);
	return builder.functionType(argTypes, funcType->getReturnType());

}

void TemporaryHandler::updateFunctionTypeWithTemporaries(core::FunctionTypePtr& funcType,
		std::vector<core::VariablePtr>& temporaries, ExpressionList& packedArgs) {

	vector<core::VariablePtr>::iterator it;

	for (it = temporaries.begin(); it < temporaries.end(); it++) {

		core::VariablePtr var = *it;
		packedArgs.push_back(var);
		funcType = addThisArgToFunctionType(convFact->getIRBuilder(), convFact->getIRBuilder().deref(var).getType(),
				funcType);

	}
}

void TemporaryHandler::addTemporariesToScope(std::vector<core::VariablePtr>& temporaries, VariableStack& scopeObjects) {

	vector<core::VariablePtr>::iterator it;

	for (it = temporaries.begin(); it < temporaries.end(); it++) {

		core::VariablePtr var = *it;
		scopeObjects.push(var);
	}
}

const TemporaryHandler::FunctionComponents TemporaryHandler::getTemporaryEffects(VariableStack& scopeObjects,
		core::FunctionTypePtr* funType, bool captureTemps) {

	FunctionComponents functionComponents;
	const core::IRBuilder& builder = convFact->getIRBuilder();

	while (!scopeObjects.empty()) {

		core::VariablePtr tempVar = scopeObjects.top();
		scopeObjects.pop();

		const ValueDecl* varDecl = getVariableDeclaration(tempVar, convFact->ctx.varDeclMap);

		if ((captureTemps || varDecl) && !(captureTemps && varDecl)) {

			core::TypePtr irType;

			if (varDecl && GET_TYPE_PTR(varDecl)->isReferenceType()) {

				irType = builder.deref(builder.deref(tempVar)).getType();
				convFact->ctx.thisStack2 = builder.deref(tempVar);

			} else {

				irType = builder.deref(tempVar).getType();
				convFact->ctx.thisStack2 = tempVar;
			}

			const Type* typeDecl = getTypeDeclaration(irType, convFact->ctx.typeCache);

			CXXRecordDecl* classDecl = cast<CXXRecordDecl>(typeDecl->getAs<RecordType>()->getDecl());

			CXXDestructorDecl* dtorDecl = classDecl->getDestructor();

			convFact->ctx.thisStack2 = tempVar;

			if (dtorDecl) {
				core::ExpressionPtr dtorIr = core::static_pointer_cast<const core::LambdaExpr>(
						convFact->convertFunctionDecl(dtorDecl, false));

				core::ExpressionPtr dtorCallIr;

				if (varDecl && GET_TYPE_PTR(varDecl)->isReferenceType()) {

					dtorCallIr = builder.callExpr(dtorIr, builder.deref(tempVar));
				} else {

					dtorCallIr = builder.callExpr(dtorIr, tempVar);
				}

				functionComponents.dtorCalls.push_back(dtorCallIr);

			}

			functionComponents.params.push_back(tempVar);
			if (!captureTemps) {
				functionComponents.args.push_back(tempVar);
			} else {
				functionComponents.args.push_back(builder.undefinedVar(tempVar.getType()));
			}
			if (funType) {
				functionComponents.funType = addThisArgToFunctionType(builder, builder.deref(tempVar).getType(),
						*funType);
			}

		} else {

			if (!captureTemps || !GET_TYPE_PTR(varDecl)->isReferenceType()) {

				functionComponents.nonRefTemporaries.push_back(tempVar);
			}
			functionComponents.temporaries.push_back(tempVar);
		}
	}
	return functionComponents;
}

void TemporaryHandler::handleTemporariesinScope(const clang::FunctionDecl* funcDecl, core::FunctionTypePtr& funcType,
		std::vector<core::VariablePtr>& params, VariableStack& scopeObjects, bool storeFunTemps, bool addTempsToParams,
		bool captureTemps) {

	const FunctionComponents& functionComponents = getTemporaryEffects(scopeObjects, &funcType, captureTemps);

	FunctionComponents& functComponents = const_cast<FunctionComponents&>(functionComponents);

	if (!functComponents.nonRefTemporaries.empty()) {
		if (addTempsToParams) {
			std::copy(functComponents.nonRefTemporaries.begin(), functComponents.nonRefTemporaries.end(),
					std::back_inserter(params));
		} else {

			std::copy(functComponents.params.begin(), functComponents.params.end(), std::back_inserter(params));
		}

		if (storeFunTemps) {
			storeFunctionTemporaries(funcDecl, convFact->ctx.fun2TempMap, functComponents.nonRefTemporaries);
		}
		std::vector<insieme::core::VariablePtr>::iterator tempit;

		for (tempit = functComponents.nonRefTemporaries.begin(); tempit != functComponents.nonRefTemporaries.end();
				tempit++) {

			funcType = addThisArgToFunctionType(convFact->builder, convFact->builder.deref(*tempit).getType(),
					funcType);
		}
	}
}

void TemporaryHandler::handleTemporariesinScope(std::vector<core::VariablePtr>& params,
		std::vector<core::StatementPtr>& stmts, VariableStack& scopeObjects, bool addTempsToParams, bool captureTemps) {

	const FunctionComponents& functionComponents = getTemporaryEffects(scopeObjects, NULL, captureTemps);
	FunctionComponents& functComponents = const_cast<FunctionComponents&>(functionComponents);

	if (addTempsToParams) {
		std::copy(functComponents.nonRefTemporaries.begin(), functComponents.nonRefTemporaries.end(),
				std::back_inserter(params));
	} else {

		std::copy(functComponents.params.begin(), functComponents.params.end(), std::back_inserter(params));
	}

	addDtorCallToFunctionBody(stmts, functComponents.dtorCalls);
}

void TemporaryHandler::handleTemporariesinScope(std::vector<core::VariablePtr>& params,
		std::vector<core::StatementPtr>& stmts, std::vector<core::ExpressionPtr>& args, VariableStack& scopeObjects,
		bool addTempsToParams, bool captureTemps) {

	const FunctionComponents& functionComponents = getTemporaryEffects(scopeObjects, NULL, captureTemps);
	FunctionComponents& functComponents = const_cast<FunctionComponents&>(functionComponents);

	if (addTempsToParams) {
		std::copy(functComponents.nonRefTemporaries.begin(), functComponents.nonRefTemporaries.end(),
				std::back_inserter(params));
		std::copy(functComponents.nonRefTemporaries.begin(), functComponents.nonRefTemporaries.end(),
				std::back_inserter(args));
	}

	std::copy(functComponents.params.begin(), functComponents.params.end(), std::back_inserter(params));
	std::copy(functComponents.args.begin(), functComponents.args.end(), std::back_inserter(args));

	addDtorCallToFunctionBody(stmts, functComponents.dtorCalls);
}

void TemporaryHandler::handleTemporariesinScope(std::vector<core::VariablePtr>& params,
		std::vector<core::StatementPtr>& stmts, std::vector<core::ExpressionPtr>& args, VariableStack& scopeObjects,
		VariableStack& parentScopeObjects, bool addTempsToParams, bool captureTemps) {

	const FunctionComponents& functionComponents = getTemporaryEffects(scopeObjects, NULL, captureTemps);
	FunctionComponents& functComponents = const_cast<FunctionComponents&>(functionComponents);

	if (addTempsToParams) {
		std::copy(functComponents.nonRefTemporaries.begin(), functComponents.nonRefTemporaries.end(),
				std::back_inserter(params));
		std::copy(functComponents.nonRefTemporaries.begin(), functComponents.nonRefTemporaries.end(),
				std::back_inserter(args));
	}

	std::copy(functComponents.params.begin(), functComponents.params.end(), std::back_inserter(params));
	std::copy(functComponents.args.begin(), functComponents.args.end(), std::back_inserter(args));

	vector<insieme::core::VariablePtr>::iterator tempit;
	for (tempit = functComponents.temporaries.begin(); tempit != functComponents.temporaries.end(); tempit++) {
		VLOG(2)
			<< *tempit;
		parentScopeObjects.push(*tempit);

	}

	addDtorCallToFunctionBody(stmts, functComponents.dtorCalls);
}

void TemporaryHandler::handleTemporariesinScope(std::vector<core::StatementPtr>& stmts,
		std::stack<core::VariablePtr>& scopeObjects, std::stack<core::VariablePtr>& parentScopeObjects,
		bool captureTemps) {

	const FunctionComponents& functionComponents = getTemporaryEffects(scopeObjects, NULL, captureTemps);

	FunctionComponents& functComponents = const_cast<FunctionComponents&>(functionComponents);

	addDtorCallToFunctionBody(stmts, functComponents.dtorCalls);

	vector<insieme::core::VariablePtr>::iterator tempit;

	for (tempit = functComponents.temporaries.begin(); tempit != functComponents.temporaries.end(); tempit++) {
		parentScopeObjects.push(*tempit);
	}
}


void TemporaryHandler::handleTemporariesinScope(std::stack<core::VariablePtr>& scopeObjects,
		std::stack<core::VariablePtr>& parentScopeObjects) {

	while (!scopeObjects.empty()) {

		core::VariablePtr tempVar = scopeObjects.top();
		scopeObjects.pop();

		const ValueDecl* varDecl = getVariableDeclaration(tempVar, convFact->ctx.varDeclMap);

		if (!varDecl) {
			parentScopeObjects.push(tempVar);
		}
	}
}

}
}
} // end insieme::frontend::cpp namespace

