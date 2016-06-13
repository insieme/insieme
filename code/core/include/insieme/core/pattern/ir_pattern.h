/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"

namespace insieme {
namespace core {
namespace pattern {
	/// Namespace for constructing/search for IR Patterns: irp
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

		inline TreePattern genericType(const TreePattern& family, const ListPattern& parents, const ListPattern& typeParams) {
			return node(core::NT_GenericType, family << single(node(parents)) << single(node(typeParams)));
		}
		inline TreePattern genericType(const TreePattern& family, const ListPattern& typeParams = empty) {
			return genericType(family, empty, typeParams);
		}
		inline TreePattern genericType(const core::StringValuePtr& family, const ListPattern& typeParams = empty) {
			return genericType(atom(family.as<core::NodePtr>()), typeParams);
		}
		inline TreePattern genericType(const string& name, const ListPattern& typeParams = empty) {
			return genericType(value(name), typeParams);
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

		inline TreePattern typeLiteral(const TreePattern& type = pattern::any) {
			return literal(genericType("type", single(type)), any);
		}

		inline TreePattern tupleType(const ListPattern& pattern) {
			return node(core::NT_TupleType, pattern);
		}

		inline TreePattern fields(const ListPattern& fields) {
			return node(core::NT_Fields, fields);
		}

		inline TreePattern parents(const ListPattern& parents) {
			return node(core::NT_Parents, parents);
		}

		inline TreePattern tagType(const TreePattern& tag, const TreePattern& def) {
			return node(core::NT_TagType, single(tag) << single(def));
		}

		inline TreePattern tagTypeDefinition(const ListPattern& defs) {
			return node(core::NT_TagTypeDefinition, defs);
		}

		inline TreePattern tagTypeBinding(const TreePattern& tag, const TreePattern& record) {
			return node(core::NT_TagTypeBinding, single(tag) << single(record));
		}

		inline TreePattern structRecord(const TreePattern& name, const TreePattern& parent, const TreePattern& fields) {
			return node(core::NT_Struct, single(name) << single(fields) << any << any << any << any << any << single(parent));
		}

		inline TreePattern unionRecord(const TreePattern& name, const TreePattern& fields) {
			return node(core::NT_Union, single(name) << single(fields) << any << any << any << any << any);
		}

		inline TreePattern structType(const ListPattern& pattern) {
			return tagType(any, tagTypeDefinition(single(tagTypeBinding(any, structRecord(any, any, fields(pattern))))));
		}

		inline TreePattern unionType(const ListPattern& pattern) {
			return tagType(any, tagTypeDefinition(single(tagTypeBinding(any, unionRecord(any, fields(pattern))))));
		}

		inline TreePattern arrayType(const TreePattern& pattern, const TreePattern& size = any) {
			return genericType("array", single(pattern) << single(size));
		}

		inline TreePattern refType(const TreePattern& elementType) {
			return genericType("ref", elementType << anyList);
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

		inline TreePattern expressions(const ListPattern& expressions) {
			return node(core::NT_Expressions, expressions);
		}

		inline TreePattern initExpr(const TreePattern& memExpr, const ListPattern& inits, const TreePattern& type = any) {
			return node(core::NT_InitExpr, single(type) << single(memExpr) << inits);
		}

		inline TreePattern markerExpr(const TreePattern& subExpression, const TreePattern& id, const TreePattern& type = any) {
			return node(core::NT_MarkerExpr, single(type) << single(id) << single(subExpression));
		}

		inline TreePattern lambda(const TreePattern& type, const ListPattern& parameters, const TreePattern& body) {
			return node(core::NT_Lambda, single(type) << single(node(core::NT_Parameters, parameters)) << wrapBody(body));
		}

		inline TreePattern lambdaExpr() {
			return node(core::NT_LambdaExpr, single(any) << single(any) << single(any));
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

		inline TreePattern declaration(const TreePattern& type = any, const TreePattern& initExpr = any) {
			return node(core::NT_Declaration, single(type) << single(initExpr));
		}

		inline TreePattern declarationStmt(const TreePattern& variable = any, const TreePattern& initExpr = any) {
			return node(core::NT_DeclarationStmt, single(declaration(any, initExpr)) << single(variable));
		}

		inline TreePattern ifStmt(const TreePattern& condition, const TreePattern& thenBody, const TreePattern& elseBody) {
			return node(core::NT_IfStmt, single(condition) << wrapBody(thenBody) << wrapBody(elseBody));
		}


		inline TreePattern forStmt(const TreePattern& iterator, const TreePattern& start, const TreePattern& end, const TreePattern& step,
		                           const ListPattern& body) {
			return node(core::NT_ForStmt, single(declarationStmt(iterator, start)) << single(end) << single(step) << compoundStmt(body));
		}

		inline TreePattern forStmt(const TreePattern& iterator, const TreePattern& start, const TreePattern& end, const TreePattern& step,
		                           const TreePattern& body) {
			return node(core::NT_ForStmt, single(declarationStmt(iterator, start)) << single(end) << single(step) << wrapBody(body));
		}

		inline TreePattern forStmt(const TreePattern& body) {
			return forStmt(any, any, any, any, body);
		}

		inline TreePattern forStmt() {
			return node(core::NT_ForStmt, anyList);
		}

		inline TreePattern whileStmt(const TreePattern& condition, const TreePattern& body) {
			return node(core::NT_WhileStmt, single(condition) << wrapBody(body));
		}

		inline TreePattern whileStmt() {
			return node(core::NT_WhileStmt, anyList);
		}

		inline TreePattern switchStmt(const TreePattern& expression, const ListPattern& cases, const TreePattern& defaultCase) {
			return node(core::NT_SwitchStmt, single(expression) << cases << single(defaultCase));
		}

		inline TreePattern returnStmt(const TreePattern& returnExpr) {
			return node(core::NT_ReturnStmt, single(declaration(any, returnExpr)));
		}

		inline TreePattern markerStmt(const TreePattern& subExpr, const TreePattern& id) {
			return node(core::NT_MarkerStmt, single(id) << single(subExpr));
		}

		inline const TreePattern& continueStmt() {
			static const TreePattern res = node(core::NT_ContinueStmt);
			return res;
		}

		inline const TreePattern& breakStmt() {
			static const TreePattern res = node(core::NT_BreakStmt);
			return res;
		}

		inline TreePattern pfor(const TreePattern& group = any, const TreePattern& start = any, const TreePattern& end = any, const TreePattern& step = any,
		                        const TreePattern& body = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getUnit(); }),
			                lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ParallelExtension>().getPFor(); }),
			                single(group) << single(start) << single(end) << single(step) << single(body));
		}

		/**
		 * Creates a pattern matching loops on the given level.
		 */
		inline TreePattern innerMostForLoop(unsigned level = 1) {
			if(level <= 1) { return forStmt(!aT(forStmt())); }
			return irp::forStmt(rT(innerMostForLoop(level - 1) | ((!irp::forStmt()) & step(recurse))));
		}

		/**
		 * Creates a pattern matching deepest innermost loops of the given depth.
		 */
		inline TreePattern innerMostForLoopNest(unsigned level = 1) {
			if(level <= 1) { return forStmt(!aT(forStmt())); }
			return rT(irp::forStmt(rT(innerMostForLoopNest(level - 1) | ((!irp::forStmt()) & step(rec("x"))), "x") & !step(aT(rec("y")))), "y");
		}


		//  ---- composed operations ------

		inline TreePattern assignment(const TreePattern& lhs = any, const TreePattern& rhs = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefAssign(); }), lhs, rhs);
		}

		inline TreePattern arrayRefElem(const TreePattern& data = any, const TreePattern& idx = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefArrayElement(); }), data, idx);
		}

		inline TreePattern tupleMemberAccess(const TreePattern& data = any, const TreePattern& idx = any, const TreePattern& type = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getTupleMemberAccess(); }), data, idx, type);
		}

		inline TreePattern tupleRefElem(const TreePattern& data = any, const TreePattern& idx = any, const TreePattern& type = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefComponentAccess(); }), data,
			                idx, type);
		}

		inline TreePattern scalarToArray(const TreePattern& data = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefScalarToRefArray(); }), data);
		}

		inline TreePattern compositeRefElem(const TreePattern& structVar = any, const TreePattern& member = any, const TreePattern& type = any) {
			return callExpr(
			    pattern::irp::lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefMemberAccess(); }), structVar,
			    member, type);
		}

		inline TreePattern compositeMemberAccess(const TreePattern& structVar = any, const TreePattern& member = any, const TreePattern& type = any) {
			return callExpr(pattern::irp::lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getCompositeMemberAccess(); }), structVar, member,
			                type);
		}

		inline TreePattern refTemp(const TreePattern& expr = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefTemp(); }), expr);
		}

		inline TreePattern refNew(const TreePattern& expr = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefNew(); }), expr);
		}

		inline TreePattern refDelete(const TreePattern& refExpr = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefDelete(); }), refExpr);
		}

		inline TreePattern refDeref(const TreePattern& refExpr = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefDeref(); }), refExpr);
		}

		inline TreePattern refReinterpret(const TreePattern& refExpr = any, const TreePattern& type = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::ReferenceExtension>().getRefReinterpret(); }), refExpr,
			                type);
		}

		inline TreePattern ptrFromRef(const TreePattern& refExpr = any) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangExtension<lang::PointerExtension>().getPtrFromRef(); }), refExpr);
		}

		inline TreePattern unaryOp(const TreePattern& a) {
			return callExpr(any, a);
		}

		inline TreePattern binaryOp(const TreePattern& a, const TreePattern& b) {
			return callExpr(any, a, b);
		}

		inline TreePattern pick(const ListPattern& args = anyList) {
			return callExpr(lazyAtom([](core::NodeManager& mgr) { return mgr.getLangBasic().getPick(); }), args);
		}

	} // end namespace irp
} // end namespace pattern
} // end namespace core
} // end namespace insieme
