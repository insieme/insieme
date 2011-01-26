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

#include "insieme/core/program.h"
#include "insieme/core/statements.h"
#include "insieme/core/expressions.h"
#include "insieme/core/types.h"
#include "insieme/core/type_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

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

ProgramPtr ASTBuilder::createProgram(const Program::EntryPointList& entryPoints, bool main) {
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

CallExprPtr ASTBuilder::refNew(const ExpressionPtr& subExpr) const {
	return callExpr(refType(subExpr->getType()), manager.basic.getRefNew(), subExpr);
}

ExpressionPtr ASTBuilder::invertSign(const ExpressionPtr& subExpr) const {
    // add a vector init expression if subExpr is of vector type
    ExpressionPtr&& elem = dynamic_pointer_cast<const VectorType>(subExpr->getType()) ?
            manager.basic.scalarToVector(subExpr->getType(), literal("0", manager.basic.getInt4())) :
            castExpr(subExpr->getType(), literal("0", manager.basic.getInt4()));

	return callExpr(subExpr->getType(), manager.basic.getOperator(subExpr->getType(), lang::BasicGenerator::Sub),
			elem, subExpr);
}

CallExprPtr ASTBuilder::vectorSubscript(const ExpressionPtr& vec, const ExpressionPtr& index) const {
	auto vType = dynamic_pointer_cast<const VectorType>(vec->getType());
	assert(vType && "Tried vector subscript operation on non-vector expression");
	return callExpr(vType->getElementType(), manager.basic.getVectorSubscript(), vec, index);
}
//CallExprPtr ASTBuilder::vectorSubscript(const ExpressionPtr& vec, unsigned index) const {
//	auto lit = uintLit(index);
//	vectorSubscript(vec, lit);
//}

CompoundStmtPtr ASTBuilder::compoundStmt(const StatementPtr& s1, const StatementPtr& s2) const {
	return compoundStmt(toVector(s1, s2));
}
CompoundStmtPtr ASTBuilder::compoundStmt(const StatementPtr& s1, const StatementPtr& s2, const StatementPtr& s3) const {
	return compoundStmt(toVector(s1, s2, s3));
}

namespace {

	TypePtr deduceReturnTypeForCall(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
		// check function expression
		assert(functionExpr->getType()->getNodeType() == NT_FunctionType && "Function expression is not a function!");

		// extract function type
		FunctionTypePtr funType = static_pointer_cast<const FunctionType>(functionExpr->getType());
		assert(funType->getArgumentTypes().size() == arguments.size() && "Invalid number of arguments!");

		// deduce return type
		core::TypeList argumentTypes;
		::transform(arguments, back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });
		return deduceReturnType(funType, argumentTypes);
	}

	/**
	 * Checks whether the given result type is matching the type expected when using automatic type inference.
	 */
	bool checkType(const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
		// check types
		if (*resultType != *deduceReturnTypeForCall(functionExpr, arguments)) {
			// print a warning if they are not matching
			LOG(WARNING) << "Potentially invalid return type for call specified - function type: "
							<< toString(*functionExpr->getType())
							<< ", arguments: " << join(", ", arguments, print<deref<ExpressionPtr>>());
		}
		return true;
	}


	CallExprPtr createCall(const ASTBuilder& builder, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {

		// check user-specified return type - only when compiled in debug mode
		// NOTE: the check returns true in any case, hence this assertion will never fail - its just a warning!
		assert(checkType(resultType, functionExpr, arguments) && "Incorrect user-specified return type!");

		// create calling expression
		return builder.callExpr(resultType, functionExpr, arguments);
	}
}

CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments /*= vector<ExpressionPtr>()*/) const {
	// use deduced return type to construct call
	return callExpr(deduceReturnTypeForCall(functionExpr, arguments), functionExpr, arguments);
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
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1, arg2));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1, arg2, arg3));
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


CaptureInitExprPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params) const {
	return lambdaExpr(manager.basic.getUnit(), body, captureMap, params);
}

CaptureInitExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params) const {

	// process capture map
	InitDetails&& details = splitUp(captureMap);

	vector<TypePtr>& captureTypes = details.get<0>();
	vector<VariablePtr>& captureVars = details.get<1>();
	vector<ExpressionPtr>& values = details.get<2>();


	// build function type
	FunctionTypePtr funType = functionType(captureTypes, extractParamTypes(params), returnType);

	LambdaExprPtr lambda = lambdaExpr(funType, captureVars, params, body);
	
	// add capture init expression
	return captureInitExpr(lambda, values);
}

CallExprPtr ASTBuilder::getThreadNumRange(unsigned min) const {
	TypePtr type = manager.basic.getUInt8();
	return callExpr(manager.basic.getCreateMinRange(), literal(type, toString(min)));
}

CallExprPtr ASTBuilder::getThreadNumRange(unsigned min, unsigned max) const {
	TypePtr type = manager.basic.getUInt8();
	return callExpr(manager.basic.getCreateBoundRange(), literal(type, toString(min)), literal(type, toString(max)));
}



CallExprPtr ASTBuilder::getThreadGroup(ExpressionPtr level) const {
    if(!level) level = uintLit(0);
    return callExpr(manager.basic.getGetThreadGroup(), level);
}
CallExprPtr ASTBuilder::getThreadId(ExpressionPtr level) const {
	if(!level) level = uintLit(0);
	return callExpr(manager.basic.getGetThreadId(), level);
}

CallExprPtr ASTBuilder::barrier(ExpressionPtr threadgroup) const {
	if(!threadgroup) threadgroup = getThreadGroup();
	return callExpr(manager.basic.getBarrier(), threadgroup);
}

CallExprPtr ASTBuilder::pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step) const {
	if(!step) step = uintLit(1);
	assert(manager.basic.isInt(start->getType()));
	assert(manager.basic.isInt(end->getType()));
	assert(manager.basic.isInt(step->getType()));
	return callExpr(manager.basic.getPFor(), toVector<ExpressionPtr>(getThreadGroup(), start, end, step, body));
}

CallExprPtr ASTBuilder::pfor(const ForStmtPtr& initialFor) const {
	auto decl = initialFor->getDeclaration();
	auto forBody = initialFor->getBody();
	auto loopvar = decl->getVariable();

	auto loopVarType = loopvar->getType();
	while (loopVarType->getNodeType() == NT_RefType) {
		loopVarType = static_pointer_cast<const RefType>(loopVarType)->getElementType();
	}

	// modify body to take vector iteration variable
	auto pforLambdaParam = variable(loopVarType);

	insieme::utils::map::PointerMap<NodePtr, NodePtr> modifications;
	modifications.insert(std::make_pair(loopvar, pforLambdaParam));
	modifications.insert(std::make_pair(deref(loopvar), pforLambdaParam));
	auto adaptedBody = static_pointer_cast<const Statement>(transform::replaceAll(manager, forBody, modifications));

	auto lambda = transform::extractLambda(manager, adaptedBody, true, toVector(pforLambdaParam));
	auto initExp = decl->getInitialization();

	while (analysis::isCallOf(initExp, manager.basic.getRefVar()) || analysis::isCallOf(initExp, manager.basic.getRefNew())) {
		initExp = static_pointer_cast<const CallExpr>(initExp)->getArguments()[0];
	}

	while (initExp->getType()->getNodeType() == NT_RefType) {
		initExp = deref(initExp);
	}

	return pfor(lambda, initExp, initialFor->getEnd(), initialFor->getStep());
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
