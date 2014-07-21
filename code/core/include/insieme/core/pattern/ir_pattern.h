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

#include <string>
#include <memory>
#include <ostream>
#include <unordered_map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/pattern/pattern.h"

namespace insieme {
namespace core {
namespace pattern {
namespace irp {
	using std::make_shared;

	inline TreePattern atom(const core::NodePtr& node) {
		return pattern::atom(node);
	}

	inline TreePattern atom(core::NodeManager& manager, const string& code) {
		return atom(core::IRBuilder(manager).parse(code));
	}

	inline TreePattern lazyAtom(const std::function<core::NodePtr(core::NodeManager&)>& factory) {
		return pattern::lazyAtom(factory);
	}

	inline TreePattern wrapBody(const TreePattern& body) {
		return node(core::NT_CompoundStmt, single(body)) | body;
	}

	inline TreePattern genericType(const TreePattern& family, const ListPattern& parents, const ListPattern& subtypes, const ListPattern& typeParams) {
		return node(core::NT_GenericType, family << single(node(parents)) << single(node(subtypes)) << single(node(typeParams)));
	}
	inline TreePattern genericType(const TreePattern& family, const ListPattern& subtypes = empty, const ListPattern& typeParams = empty) {
		return genericType(family, empty, subtypes, typeParams);
	}
	inline TreePattern genericType(const core::StringValuePtr& family, const ListPattern& typeParams = empty, const ListPattern& intParams = empty) {
		return genericType(atom(family.as<core::NodePtr>()), typeParams, intParams);
	}
	inline TreePattern genericType(const string& name, const ListPattern& typeParams = empty, const ListPattern& intParams = empty) {
		return genericType(value(name), typeParams, intParams);
	}

	inline TreePattern exprOfType(const TreePattern& type) {
		return node(single(type) << *any);
	}

	inline TreePattern literal(const TreePattern& type, const TreePattern& value) {
		return node(core::NT_Literal, single(type) << single(value));
	}

	inline TreePattern literal(const TreePattern& type, const core::StringValuePtr& value) {
		return literal(type, atom(value.as<core::NodePtr>()));
	}

	inline TreePattern literal(const TreePattern& type, const string& str) {
		return literal(type, value(str));
	}

	inline TreePattern literal(const string& str) {
		return literal(any, str);
	}

	inline TreePattern literal() {
		return literal(any, any);
	}

	inline TreePattern tupleType(const ListPattern& pattern) {
		return node(core::NT_TupleType, pattern);
	}

	inline TreePattern structType(const ListPattern& pattern) {
		return node(core::NT_StructType, pattern);
	}

	inline TreePattern arrayType(const TreePattern& pattern, const TreePattern& dim = pattern::any) {
		return node(core::NT_ArrayType, pattern << dim);
	}

	inline TreePattern refType(const TreePattern& elementType, const TreePattern& refKind = pattern::any) {
		return node(core::NT_RefType, elementType << refKind);
	}

	inline TreePattern variable(const TreePattern& type = pattern::any, const TreePattern& id = pattern::any) {
		return node(core::NT_Variable, single(type) << single(id));
	}

	inline TreePattern callExpr(const TreePattern& type, const TreePattern& function, const ListPattern& parameters) {
		return node(core::NT_CallExpr, type << single(function) << parameters);
	}

	inline TreePattern callExpr(const TreePattern& type, const NodePtr& function, const ListPattern& parameters) {
		return callExpr(type, atom(function), parameters);
	}

	inline TreePattern callExpr(const core::NodePtr& function, const ListPattern& parameters = anyList) {
		return callExpr(any, atom(function), parameters);
	}
	
	inline TreePattern callExpr(const core::NodePtr& function, const TreePattern& parameter) {
		return callExpr(function, single(parameter));
	}

	inline TreePattern callExpr(const TreePattern& function, const ListPattern& parameters = anyList) {
		return callExpr(any, function, parameters);
	}

	inline TreePattern callExpr(const TreePattern& fun, const TreePattern& arg0) {
		return callExpr(any, fun, single(arg0));
	}

	inline TreePattern callExpr(const TreePattern& fun, const TreePattern& arg0, const TreePattern& arg1) {
		return callExpr(any, fun, single(arg0) << single(arg1));
	}

	inline TreePattern callExpr(const TreePattern& fun, const TreePattern& arg0, const TreePattern& arg1, const TreePattern& arg2) {
		return callExpr(any, fun, single(arg0) << single(arg1) << single(arg2));
	}

	inline TreePattern castExpr(const TreePattern& type, const TreePattern& expression) {
		return node(core::NT_CastExpr, type << single(expression));
	}

	inline TreePattern bindExpr(const ListPattern& parameters, const TreePattern& call) {
		return node(core::NT_BindExpr, parameters << single(call));
	}

	inline TreePattern tupleExpr(const ListPattern& expressions) {
		return node(core::NT_TupleExpr, expressions);
	}

	inline TreePattern vectorExpr(const ListPattern& expressions) {
		return node(core::NT_VectorExpr, expressions);
	}

	inline TreePattern expressions(const ListPattern& expressions) {
		return node(core::NT_Expressions, expressions);
	}

	inline TreePattern structExpr(const ListPattern& members) {
		return node(core::NT_StructExpr, members);
	}

	inline TreePattern unionExpr(const TreePattern& memberName, const TreePattern& member) {
		return node(core::NT_UnionExpr, single(memberName) << single(member));
	}

	inline TreePattern markerExpr(const TreePattern& subExpression, const TreePattern& id) {
		return node(core::NT_MarkerExpr, single(subExpression) << single(id));
	}

	inline TreePattern lambda(const TreePattern& type, const ListPattern& parameters, const TreePattern& body) {
		return node(core::NT_Lambda, single(type) << single(node(core::NT_Parameters, parameters)) << wrapBody(body));
	}

	inline TreePattern lambdaExpr(const TreePattern& variable, const TreePattern& lambdaDef) {
		return node(core::NT_LambdaExpr, single(any) << single(variable) << single(lambdaDef));
	}

	inline TreePattern lambdaDefinition(const ListPattern& definitions) {
		return node(core::NT_LambdaDefinition, definitions);
	}

	inline TreePattern compoundStmt(const ListPattern& stmts = empty) {
		return node(core::NT_CompoundStmt, stmts);
	}
	inline TreePattern compoundStmt(const TreePattern& stmt) {
		return compoundStmt(single(stmt));
	}

	inline TreePattern declarationStmt(const TreePattern& variable = any, const TreePattern& initExpr = any) {
		return node(core::NT_DeclarationStmt, single(variable) << single(initExpr));
	}

	inline TreePattern ifStmt(const TreePattern& condition, const TreePattern& thenBody, const TreePattern& elseBody){
		return node(core::NT_IfStmt, single(condition) << wrapBody(thenBody) << wrapBody(elseBody));
	}


	inline TreePattern forStmt(const TreePattern& iterator, const TreePattern& start,
									  const TreePattern& end, const TreePattern& step,
									  const ListPattern& body )
		{
			return node(core::NT_ForStmt, single(declarationStmt(iterator,start)) <<
										  single(end) << single(step) << compoundStmt(body)
					   );
		}

	inline TreePattern forStmt(const TreePattern& iterator, const TreePattern& start,
								  const TreePattern& end, const TreePattern& step,
								  const TreePattern& body )
	{
		return node(core::NT_ForStmt, single(declarationStmt(iterator,start)) <<
										  single(end) << single(step) << wrapBody(body)
					   );
	}

	inline TreePattern forStmt(const TreePattern& body) {
		return forStmt(any, any, any, any, body);
	}

	inline TreePattern forStmt(){ return node(core::NT_ForStmt, anyList); }

	inline TreePattern whileStmt(const TreePattern& condition, const TreePattern& body) {
		return node(core::NT_WhileStmt, single(condition) << wrapBody(body));
	}

	inline TreePattern whileStmt(){ return node(core::NT_WhileStmt, anyList); }

	inline TreePattern switchStmt(const TreePattern& expression, const ListPattern& cases, const TreePattern& defaultCase) {
		return node(core::NT_SwitchStmt, single(expression) << cases << single(defaultCase));
	}

	inline TreePattern returnStmt(const TreePattern& returnExpression){
		return node(core::NT_ReturnStmt, single(returnExpression));
	}

	inline TreePattern markerStmt(const TreePattern& subExpr, const TreePattern& id) {
		return node(core::NT_MarkerStmt, single(subExpr) << single(id));
	}

	inline const TreePattern& continueStmt() {
		static const TreePattern res = node(core::NT_ContinueStmt);
		return res;
	}

	inline const TreePattern& breakStmt() {
		static const TreePattern res = node(core::NT_BreakStmt);
		return res;
	}

	inline TreePattern jobExpr(const TreePattern& threadNumRange, const ListPattern& localDecls, const ListPattern& guardedExprs,
			const TreePattern& defaultExpr) {
		return node(core::NT_JobExpr, single(any) << single(threadNumRange) << single(node(core::NT_DeclarationStmts, localDecls)) 
			<< single(node(core::NT_GuardedExprs, guardedExprs)) << single(defaultExpr));
	}

	inline TreePattern jobExpr(const TreePattern& threadNumRange, const TreePattern& defaultExpr) {
		return jobExpr(threadNumRange, anyList, anyList, defaultExpr);
	}

	/**
	 * Creates a pattern matching loops on the given level.
	 */
	inline TreePattern innerMostForLoop(unsigned level = 1) {
		if (level <= 1) { return forStmt(!aT(forStmt())); }
		return irp::forStmt(rT( innerMostForLoop(level-1) | (!irp::forStmt() & step(recurse))));
	}

	/**
	 * Creates a pattern matching deepest innermost loops of the given depth.
	 */
	inline TreePattern innerMostForLoopNest(unsigned level = 1) {
		if (level <= 1) { return forStmt(!aT(forStmt())); }
		return rT(irp::forStmt(rT( innerMostForLoopNest(level-1) | (!irp::forStmt() & step(rec("x"))), "x") & !step(aT(rec("y")))), "y");
	}
	

	//  ---- composed operations ------

	inline TreePattern assignment(const TreePattern& lhs = any, const TreePattern& rhs = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getRefAssign(); }), lhs, rhs);
	}

	inline TreePattern arrayRefElem1D(const TreePattern& data = any, const TreePattern& idx = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getArrayRefElem1D(); }), data, idx);
	}

	inline TreePattern arraySubscript1D(const TreePattern& data = any, const TreePattern& idx = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getArraySubscript1D(); }), data, idx);
	}

	inline TreePattern tupleMemberAccess(const TreePattern& data = any, const TreePattern& idx = any, const TreePattern& type = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getTupleMemberAccess(); }), data, idx, type);
	}

	inline TreePattern tupleRefElem(const TreePattern& data = any, const TreePattern& idx = any, const TreePattern& type = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getTupleRefElem(); }), data, idx, type);
	}

	inline TreePattern vectorRefElem(const TreePattern& data = any, const TreePattern& idx = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getVectorRefElem(); }), data, idx);
	}

	inline TreePattern vectorSubscript(const TreePattern& data = any, const TreePattern& idx = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getVectorSubscript(); }), data, idx);
	}

	inline TreePattern subscript1D(const TreePattern& data = any, const TreePattern& idx = any) {
		return arrayRefElem1D(data, idx) | arraySubscript1D(data, idx) | vectorRefElem(data, idx) | vectorSubscript(data, idx);
	}

	inline TreePattern subscript1D(std::string operation, const TreePattern& data = any, const TreePattern& idx = any) {
		return callExpr(var(operation, lazyAtom([&](core::NodeManager& mgr) { return mgr.getLangBasic().getArrayRefElem1D(); })) |
				var(operation, lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getArraySubscript1D(); })) |
				var(operation, lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getVectorRefElem(); })) |
				var(operation, lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getVectorSubscript(); })), data, idx);
	}

	inline TreePattern scalarToArray(const TreePattern& data = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getScalarToArray(); }), data);
	}

	inline TreePattern compositeRefElem(const TreePattern& structVar = any, const TreePattern& member = any, const TreePattern& type = any) {
		return callExpr(pattern::irp::lazyAtom([](core::NodeManager& mgr) {
			return mgr.getLangBasic().getCompositeRefElem();
		}), structVar, member, type);
	}

	inline TreePattern refVar(const TreePattern& expr = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getRefVar(); }), expr);
	}

	inline TreePattern refNew(const TreePattern& expr = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getRefNew(); }), expr);
	}

	inline TreePattern refLoc(const TreePattern& expr = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getRefLoc(); }), expr);
	}

	inline TreePattern refDelete(const TreePattern& refExpr = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getRefDelete(); }), refExpr);
	}

	inline TreePattern refDeref(const TreePattern& refExpr = any) {
		return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getRefDeref(); }), refExpr);
	}



} // end namespace irp
} // end namespace pattern
} // end namespace core
} // end namespace insieme
