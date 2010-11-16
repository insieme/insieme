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

#include "insieme/core/ast_builder.h"

#include "insieme/core/annotated_ptr.h"
#include "insieme/core/program.h"
#include "insieme/core/statements.h"
#include "insieme/core/expressions.h"
#include "insieme/core/types.h"
#include "insieme/core/type_utils.h"

namespace insieme {
namespace core {

ProgramPtr ASTBuilder::createProgram(const Program::EntryPointSet& entryPoints, bool main) {
	return Program::create(manager, entryPoints, main);
}

// ------------------------------- Build Basic Types -------------------------

lang::UnitTypePtr ASTBuilder::unitType() const {
	return manager.get(lang::TYPE_UNIT);
}

lang::BoolTypePtr ASTBuilder::boolType() const {
	return manager.get(lang::TYPE_BOOL);
}

lang::IntTypePtr  ASTBuilder::intType (unsigned short size) const {
	return manager.get(lang::intType(size));
}

lang::UIntTypePtr ASTBuilder::uintType(unsigned short size) const {
	return manager.get(lang::uintType(size));
}

lang::RealTypePtr ASTBuilder::realType(unsigned short size) const {
	return manager.get(lang::realType(size));
}

// ---------------------------- Convenience -------------------------------------

LiteralPtr ASTBuilder::intVal(long val, unsigned short size) const {
	return literal(toString(val), intType(size));
}
LiteralPtr ASTBuilder::uintVal(long val, unsigned short size) const {
	return literal(toString(val), uintType(size));
}
LiteralPtr ASTBuilder::stringVal(const char* str) const {
	return literal(str, lang::TYPE_STRING);
}


CallExprPtr ASTBuilder::deref(const ExpressionPtr& subExpr) const {
	return callExpr(lang::OP_REF_DEREF, subExpr);
}

CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments /*= vector<ExpressionPtr>()*/) const {
	TypePtr retType = core::lang::TYPE_UNIT;
	if(auto funType = dynamic_pointer_cast<const FunctionType>(functionExpr->getType())) {
		retType = funType->getReturnType();
	}
	return callExpr(retType, functionExpr, arguments);
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return callExpr(resultType, functionExpr, toVector(arg1));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return callExpr(resultType, functionExpr, toVector(arg1, arg2));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return callExpr(resultType, functionExpr, toVector(arg1, arg2, arg3));
}
CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return callExpr(functionExpr, toVector(arg1));
}
CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return callExpr(functionExpr, toVector(arg1, arg2));
}
CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return callExpr(functionExpr, toVector(arg1, arg2, arg3));
}

LambdaExprPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const ParamList& params) const {
	return lambdaExpr(functionType(tupleType(extractParamTypes(params)), core::lang::TYPE_UNIT), params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const CaptureList& captures, const ParamList& params) const {
	return lambdaExpr(functionType(tupleType(extractParamTypes(params)), core::lang::TYPE_UNIT), captures, params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const ParamList& params) const {
	return lambdaExpr(functionType(tupleType(extractParamTypes(params)), returnType), params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureList& captures, const ParamList& params) const {
	return lambdaExpr(functionType(tupleType(extractParamTypes(params)), returnType), captures, params, body);
}

// ---------------------------- Utilities ---------------------------------------

ASTBuilder::ElementTypeList ASTBuilder::extractParamTypes(const ParamList& params) {
	ElementTypeList paramTypes;
	std::transform(params.cbegin(), params.cend(), std::back_inserter(paramTypes), 
		[](const VariablePtr& p) { return p->getType(); });
	return paramTypes;
}

#include "ast_builder_impl.inl"

} // namespace core
} // namespace insieme
