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

#include <boost/tuple/tuple.hpp>

#include "insieme/core/ast_builder.h"

#include "insieme/core/annotated_ptr.h"
#include "insieme/core/program.h"
#include "insieme/core/statements.h"
#include "insieme/core/expressions.h"
#include "insieme/core/types.h"
#include "insieme/core/type_utils.h"

namespace insieme {
namespace core {

namespace {

	typedef boost::tuple<vector<TypePtr>, vector<VariablePtr>, vector<ExpressionPtr>> InitDetails;

	InitDetails splitUp(const ASTBuilder::CaptureInits& captureInits) {

		// prepare containers
		InitDetails res;
		vector<TypePtr>& types = res.get<0>();
		vector<VariablePtr>& vars = res.get<1>();
		vector<ExpressionPtr>& inits = res.get<2>();

		// process the given map
		for_each(captureInits, [&](const ASTBuilder::CaptureInits::value_type& cur) {
			types.push_back(cur.first->getType());
			vars.push_back(cur.first);
			inits.push_back(cur.second);
		});

		// return results
		return res;
	}

}

ProgramPtr ASTBuilder::createProgram(const Program::EntryPointSet& entryPoints, bool main) {
	return Program::create(manager, entryPoints, main);
}

// ---------------------------- Convenience -------------------------------------

LiteralPtr ASTBuilder::stringLit(const char* str) const {
	return literal(str, manager.basic.getString());
}

LiteralPtr ASTBuilder::intLit(const int val) const {
    return literal(manager.basic.getInt4(), toString(val));
}
LiteralPtr ASTBuilder::uintLit(const unsigned int val) const {
    return literal(manager.basic.getUInt4(), toString(val));
}


CallExprPtr ASTBuilder::deref(const ExpressionPtr& subExpr) const {
	RefTypePtr&& refTy = dynamic_pointer_cast<const RefType>(subExpr->getType());
	assert(refTy && "Deref a non ref type.");
	return callExpr(refTy->getElementType(), manager.basic.getRefDeref(), subExpr);
}

CallExprPtr ASTBuilder::refVar(const ExpressionPtr& subExpr) const {
	return callExpr(refType(subExpr->getType()), manager.basic.getRefVar(), subExpr);
}

CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments /*= vector<ExpressionPtr>()*/) const {
	TypePtr&& retType = manager.basic.getUnit();
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
	return lambdaExpr(functionType(extractParamTypes(params), manager.basic.getUnit()), params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const CaptureList& captures, const ParamList& params) const {
	return lambdaExpr(functionType(extractParamTypes(captures), extractParamTypes(params), manager.basic.getUnit()), captures, params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const ParamList& params) const {
	return lambdaExpr(functionType(extractParamTypes(params), returnType), params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureList& captures, const ParamList& params) const {
	return lambdaExpr(functionType(extractParamTypes(captures), extractParamTypes(params), returnType), captures, params, body);
}


ExpressionPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params) const {
	return lambdaExpr(manager.basic.getUnit(), body, captureMap, params);
}

ExpressionPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params) const {

	// process capture map
	InitDetails&& details = splitUp(captureMap);

	vector<TypePtr>& captureTypes = details.get<0>();
	vector<VariablePtr>& captureVars = details.get<1>();
	vector<ExpressionPtr>& values = details.get<2>();


	// build function type
	FunctionTypePtr funType = functionType(captureTypes, extractParamTypes(params), returnType);

	LambdaExprPtr lambda = lambdaExpr(funType, captureVars, params, body);

	// add capture init expression
	if (captureMap.empty()) {
		return lambda;
	}

	// add capture init expression
	return captureInitExpr(lambda, values);
}

CallExprPtr ASTBuilder::getThreadGroup(const ExpressionPtr& level) const {
    if(!level)
        return callExpr(manager.basic.getGetThreadGroup(), uintLit(0));

    return callExpr(manager.basic.getGetThreadGroup(), level);
}



// ---------------------------- Utilities ---------------------------------------

ASTBuilder::TypeList ASTBuilder::extractParamTypes(const ParamList& params) {
	TypeList paramTypes;
	std::transform(params.cbegin(), params.cend(), std::back_inserter(paramTypes),
		[](const VariablePtr& p) { return p->getType(); });
	return paramTypes;
}

#include "ast_builder_impl.inl"

} // namespace core
} // namespace insieme
