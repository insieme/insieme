/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <string>
#include <memory>
#include <ostream>
#include <unordered_map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/pattern/generator.h"

namespace insieme {
namespace core {
namespace pattern {
namespace generator {
namespace irg {
	using std::make_shared;

	typedef std::shared_ptr<impl::MatchExpression<ptr_target>> MatchExpressionPtr;

	// -- Some Match Expression helper --------------------------------------

	MatchExpressionPtr range(int start, int end);


	// -- Some Special Connectors -------------------------------------------

	inline ListGenerator forEach(const std::string& name, int start, int end, const TreeGenerator& tree) {
		return generator::forEach(name, range(start, end), tree);
	}

	inline TreeGenerator stringValue(const string& value) {
		return treeExpr(construct([=](const Match<ptr_target>& match) -> MatchValue<ptr_target> {
			core::NodePtr res = core::StringValue::get(match.getRoot().getNodeManager(), value);
			return MatchValue<ptr_target>(res);
		}, value));
	}

	inline TreeGenerator stringValue(const TreeGenerator& tree) {
		return treeExpr(construct([=](const Match<ptr_target>& match) -> MatchValue<ptr_target> {
			core::NodePtr res = core::StringValue::get(match.getRoot().getNodeManager(), toString(tree.generate(match)));
			return MatchValue<ptr_target>(res);
		}, format("#(%s)", toString(tree))));
	}

	inline TreeGenerator freshID() {
		return treeExpr(construct([=](const Match<ptr_target>& match) -> MatchValue<ptr_target> {
			core::NodeManager& manager = match.getRoot()->getNodeManager();
			core::NodePtr res = core::UIntValue::get(manager, manager.getFreshID());
			return MatchValue<ptr_target>(res);
		}, "freshID()"));
	}

	// -- IR Specific Constructs --------------------------------------------

	inline TreeGenerator atom(const core::NodePtr& node) {
		return generator::atom(node);
	}

	inline TreeGenerator atom(core::NodeManager& manager, const string& code) {
		return atom(core::IRBuilder(manager).parseExpr(code));
	}

	inline TreeGenerator genericType(const TreeGenerator& family, const ListGenerator& subtypes = empty, const ListGenerator& typeParams = empty) {
		return node(core::NT_GenericType, family << single(node(core::NT_Parents, subtypes)) << single(node(core::NT_Types, typeParams)));
	}

	inline TreeGenerator literal(const TreeGenerator& type, const TreeGenerator& value) {
		return node(core::NT_Literal, single(type) << single(value));
	}

	inline TreeGenerator literal(const TreeGenerator& type, const core::StringValuePtr& value) {
		return literal(type, atom(value));
	}

	inline TreeGenerator literal(const TreeGenerator& type, const string& value) {
		return literal(type, stringValue(value));
	}

	inline TreeGenerator literal(const TreeGenerator& type, int value) {
		return literal(type, stringValue(toString(value)));
	}

	inline TreeGenerator typeLiteral(const TreeGenerator& type) {
		return literal(genericType(stringValue("type"), empty, single(type)), "type_literal");
	}

	inline TreeGenerator tupleType(const ListGenerator& pattern) {
		return node(core::NT_TupleType, pattern);
	}

	inline TreeGenerator variable(const TreeGenerator& type, const TreeGenerator& id = freshID()) {
		return node(core::NT_Variable, single(type) << single(id));
	}

	inline TreeGenerator callExpr(const TreeGenerator& type, const TreeGenerator& function, const ListGenerator& parameters = generator::empty) {
		return node(core::NT_CallExpr, type << single(function) << parameters);
	}

	inline TreeGenerator callExpr(const TreeGenerator& type, const NodePtr& function, const ListGenerator& parameters = generator::empty) {
		return callExpr(type, atom(function), parameters);
	}

	inline TreeGenerator callExpr(const TreeGenerator& type, const NodePtr& function, const TreeGenerator& arg0) {
		return callExpr(type, atom(function), single(arg0));
	}

	inline TreeGenerator callExpr(const TreeGenerator& type, const NodePtr& function, const TreeGenerator& arg0, const TreeGenerator& arg1) {
		return callExpr(type, atom(function), single(arg0) << single(arg1));
	}

	inline TreeGenerator bindExpr(const ListGenerator& parameters, const TreeGenerator& call) {
		return node(core::NT_BindExpr, parameters << single(call));
	}

	inline TreeGenerator tupleExpr(const ListGenerator& expressions) {
		return node(core::NT_TupleExpr, expressions);
	}

	inline TreeGenerator initExpr(const TreeGenerator& type, const TreeGenerator& memExpr, const ListGenerator& expressions) {
		return node(core::NT_InitExpr, single(type) << single(memExpr) << expressions);
	}

	inline TreeGenerator markerExpr(const TreeGenerator& subExpression, const TreeGenerator& id) {
		return node(core::NT_MarkerExpr, single(subExpression) << single(id));
	}

	inline TreeGenerator lambda(const TreeGenerator& type, const ListGenerator& parameters, const TreeGenerator& body) {
		return node(core::NT_Lambda, single(type) << single(node(core::NT_Parameters, parameters)) << body);
	}

	inline TreeGenerator lambdaDefinition(const ListGenerator& definitions) {
		return node(core::NT_LambdaDefinition, definitions);
	}

	inline TreeGenerator compoundStmt(const ListGenerator& stmts) {
		return node(core::NT_CompoundStmt, stmts);
	}
	inline TreeGenerator compoundStmt(const TreeGenerator& stmt) {
		return compoundStmt(single(stmt));
	}

	inline TreeGenerator declaration(const TreeGenerator& type, const TreeGenerator& initExpr) {
		return node(core::NT_Declaration, single(type) << single(initExpr));
	}

	inline TreeGenerator declarationStmt(const TreeGenerator& type, const TreeGenerator& variable, const TreeGenerator& initExpr) {
		return node(core::NT_DeclarationStmt, single(declaration(type, initExpr)) << single(variable));
	}

	inline TreeGenerator ifStmt(const TreeGenerator& condition, const TreeGenerator& thenBody, const TreeGenerator& elseBody) {
		return node(core::NT_IfStmt, single(condition) << thenBody << elseBody);
	}


	inline TreeGenerator forStmt(const TreeGenerator& iteratorT, const TreeGenerator& iterator, const TreeGenerator& start, const TreeGenerator& end,
				                 const TreeGenerator& step, const ListGenerator& body) {
		return node(core::NT_ForStmt, single(declarationStmt(iteratorT, iterator, start)) << single(end) << single(step) << compoundStmt(body));
	}

	inline TreeGenerator forStmt(const TreeGenerator& iteratorT, const TreeGenerator& iterator, const TreeGenerator& start, const TreeGenerator& end,
				                 const TreeGenerator& step, const TreeGenerator& body) {
		return node(core::NT_ForStmt, single(declarationStmt(iteratorT, iterator, start)) << end << step << compoundStmt(body));
	}

	inline TreeGenerator whileStmt(const TreeGenerator& condition, const TreeGenerator& body) {
		return node(core::NT_WhileStmt, single(condition) << body);
	}

	inline TreeGenerator switchStmt(const TreeGenerator& expression, const ListGenerator& cases, const TreeGenerator& defaultCase) {
		return node(core::NT_SwitchStmt, single(expression) << cases << single(defaultCase));
	}

	inline TreeGenerator returnStmt(const TreeGenerator& returnType, const TreeGenerator& returnExpression) {
		return node(core::NT_ReturnStmt, single(declaration(returnType, returnExpression)));
	}

	inline TreeGenerator markerStmt(const TreeGenerator& subExpr, const TreeGenerator& id) {
		return node(core::NT_MarkerStmt, single(subExpr) << single(id));
	}

	const TreeGenerator continueStmt = node(core::NT_ContinueStmt, empty);
	const TreeGenerator breakStmt = node(core::NT_BreakStmt, empty);


	// -- Types ------------------------------------------------------------

	inline TreeGenerator int4() {
		return treeExpr(construct([&](const Match<ptr_target>& match) {
			core::NodePtr res = match.getRoot()->getNodeManager().getLangBasic().getInt4();
			return MatchValue<ptr_target>(res);
		}, "int4"));
	}


	// -- Arithmetic Constructs --------------------------------------------

	TreeGenerator add(const TreeGenerator& a, const TreeGenerator& b);
	TreeGenerator sub(const TreeGenerator& a, const TreeGenerator& b);

	TreeGenerator mul(const TreeGenerator& a, const TreeGenerator& b);
	TreeGenerator div(const TreeGenerator& a, const TreeGenerator& b);

	TreeGenerator mod(const TreeGenerator& a, const TreeGenerator& b);

	TreeGenerator min(const TreeGenerator& a, const TreeGenerator& b);
	TreeGenerator max(const TreeGenerator& a, const TreeGenerator& b);

	/**
	 * A generator accepting a arithmetic formula being created by the given
	 * generator and collapsing it if possible.
	 */
	TreeGenerator simplify(const TreeGenerator& a);


} // end namespace irg
} // end namespace generator
} // end namespace pattern
} // end namespace core
} // end namespace insieme
