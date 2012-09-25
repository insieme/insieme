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
#include "insieme/transform/pattern/generator.h"

namespace insieme {
namespace transform {
namespace pattern {
namespace generator {
namespace irg {
	using std::make_shared;

	typedef std::shared_ptr<MatchExpression<ptr_target>> MatchExpressionPtr;

	// -- Some Match Expression helper --------------------------------------

	MatchExpressionPtr range(int start, int end);


	// -- Some Special Connectors -------------------------------------------

	inline ListGeneratorPtr forEach(const std::string& name, int start, int end, const TreeGeneratorPtr& tree) {
		return generator::forEach(name, range(start, end), tree);
	}

	inline TreeGeneratorPtr stringValue(const string& value) {
		return treeExpr(construct([=](const Match<ptr_target>& match)->MatchValue<ptr_target> {
			core::NodePtr res = core::StringValue::get(match.getRoot().getNodeManager(), value);
			return MatchValue<ptr_target>(res);
		}, value));
	}

	inline TreeGeneratorPtr freshID() {
		return treeExpr(construct([=](const Match<ptr_target>& match)->MatchValue<ptr_target> {
			core::NodeManager& manager = match.getRoot()->getNodeManager();
			core::NodePtr res = core::UIntValue::get(manager, manager.getFreshID());
			return MatchValue<ptr_target>(res);
		}, "freshID()"));
	}

	// -- IR Specific Constructs --------------------------------------------

	inline TreeGeneratorPtr atom(const core::NodePtr& node) {
		return generator::atom(node);
	}

	inline TreeGeneratorPtr atom(core::NodeManager& manager, const string& code) {
		return atom( core::IRBuilder(manager).parse(code) );
	}

	inline TreeGeneratorPtr genericType(const TreeGeneratorPtr& family, const ListGeneratorPtr& subtypes = empty, const ListGeneratorPtr& typeParams = empty) {
		return node(core::NT_GenericType, family << single(node(subtypes)) << single(node(typeParams)));
	}
	inline TreeGeneratorPtr genericType(const core::StringValuePtr& family, const ListGeneratorPtr& typeParams = empty, const ListGeneratorPtr& intParams = empty) {
		return genericType(atom(family), typeParams, intParams);
	}


	inline TreeGeneratorPtr literal(const TreeGeneratorPtr& type, const TreeGeneratorPtr& value) {
		return node(core::NT_Literal, single(type) << single(value));
	}

	inline TreeGeneratorPtr literal(const TreeGeneratorPtr& type, const core::StringValuePtr& value) {
		return literal(type, atom(value));
	}

	inline TreeGeneratorPtr literal(const TreeGeneratorPtr& type, int value) {
		return literal(type, stringValue(toString(value)));
	}

	inline TreeGeneratorPtr tupleType(const ListGeneratorPtr& pattern) {
		return node(core::NT_TupleType, pattern);
	}

	inline TreeGeneratorPtr variable(const TreeGeneratorPtr& type, const TreeGeneratorPtr& id = freshID()) {
		return node(core::NT_Variable, single(type) << single(id));
	}

	inline TreeGeneratorPtr callExpr(const TreeGeneratorPtr& type, const TreeGeneratorPtr& function, const ListGeneratorPtr& parameters = generator::empty) {
		return node(core::NT_CallExpr, type << single(function) << parameters);
	}

	inline TreeGeneratorPtr bindExpr(const ListGeneratorPtr& parameters, const TreeGeneratorPtr& call) {
		return node(core::NT_BindExpr, parameters << single(call));
	}

	inline TreeGeneratorPtr tupleExpr(const ListGeneratorPtr& expressions) {
		return node(core::NT_TupleExpr, expressions);
	}

	inline TreeGeneratorPtr vectorExpr(const ListGeneratorPtr& expressions) {
		return node(core::NT_VectorExpr, expressions);
	}

	inline TreeGeneratorPtr structExpr(const ListGeneratorPtr& members) {
		return node(core::NT_StructExpr, members);
	}

	inline TreeGeneratorPtr unionExpr(const TreeGeneratorPtr& memberName, const TreeGeneratorPtr& member) {
		return node(core::NT_UnionExpr, single(memberName) << single(member));
	}

	inline TreeGeneratorPtr markerExpr(const TreeGeneratorPtr& subExpression, const TreeGeneratorPtr& id) {
		return node(core::NT_MarkerExpr, single(subExpression) << single(id));
	}

	inline TreeGeneratorPtr lambda(const TreeGeneratorPtr& type, const ListGeneratorPtr& parameters, const TreeGeneratorPtr& body) {
		return node(core::NT_Lambda, single(type) << single(node(core::NT_Parameters, parameters)) << body);
	}

	inline TreeGeneratorPtr lambdaDefinition(const ListGeneratorPtr& definitions) {
		return node(core::NT_LambdaDefinition, definitions);
	}

	inline TreeGeneratorPtr compoundStmt(const ListGeneratorPtr& stmts) {
		return node(core::NT_CompoundStmt, stmts);
	}
	inline TreeGeneratorPtr compoundStmt(const TreeGeneratorPtr& stmt) {
		return compoundStmt(single(stmt));
	}

	inline TreeGeneratorPtr declarationStmt(const TreeGeneratorPtr& variable, const TreeGeneratorPtr& initExpr) {
		return node(core::NT_DeclarationStmt, single(variable) << single(initExpr));
	}

	inline TreeGeneratorPtr ifStmt(const TreeGeneratorPtr& condition, const TreeGeneratorPtr& thenBody, const TreeGeneratorPtr& elseBody){
		return node(core::NT_IfStmt, single(condition) << thenBody << elseBody);
	}


	inline TreeGeneratorPtr forStmt(const TreeGeneratorPtr& iterator, const TreeGeneratorPtr& start,
									  const TreeGeneratorPtr& end, const TreeGeneratorPtr& step,
									  const ListGeneratorPtr& body )
		{
			return node(core::NT_ForStmt, single(declarationStmt(iterator,start)) <<
										  single(end) << single(step) << compoundStmt(body)
					   );
		}

	inline TreeGeneratorPtr forStmt(const TreeGeneratorPtr& iterator, const TreeGeneratorPtr& start,
								  const TreeGeneratorPtr& end, const TreeGeneratorPtr& step,
								  const TreeGeneratorPtr& body )
	{
		return node(core::NT_ForStmt, single(declarationStmt(iterator,start)) << end << step << compoundStmt(body));
	}

	inline TreeGeneratorPtr whileStmt(const TreeGeneratorPtr& condition, const TreeGeneratorPtr& body){
		return node(core::NT_WhileStmt, single(condition) << body);
	}

	inline TreeGeneratorPtr switchStmt(const TreeGeneratorPtr& expression, const ListGeneratorPtr& cases, const TreeGeneratorPtr& defaultCase){
		return node(core::NT_SwitchStmt, single(expression) << cases << single(defaultCase));
	}

	inline TreeGeneratorPtr returnStmt(const TreeGeneratorPtr& returnExpression){
		return node(core::NT_ReturnStmt, single(returnExpression));
	}

	inline TreeGeneratorPtr markerStmt(const TreeGeneratorPtr& subExpr, const TreeGeneratorPtr& id){
		return node(core::NT_MarkerStmt, single(subExpr) << single(id));
	}

	const TreeGeneratorPtr continueStmt = node(core::NT_ContinueStmt, empty);
	const TreeGeneratorPtr breakStmt = node(core::NT_BreakStmt, empty);


	// -- Types ------------------------------------------------------------

	inline TreeGeneratorPtr int4() {
		return treeExpr(construct([&](const Match<ptr_target>& match) {
			core::NodePtr res = match.getRoot()->getNodeManager().getLangBasic().getInt4();
			return MatchValue<ptr_target>(res);
		}, "int4"));
	}


	// -- Arithmetic Constructs --------------------------------------------

	TreeGeneratorPtr add(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);
	TreeGeneratorPtr sub(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);

	TreeGeneratorPtr mul(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);
	TreeGeneratorPtr div(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);

	TreeGeneratorPtr mod(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);

	TreeGeneratorPtr min(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);
	TreeGeneratorPtr max(const TreeGeneratorPtr& a, const TreeGeneratorPtr& b);

	/**
	 * A generator accepting a arithmetic formula being created by the given
	 * generator and collapsing it if possible.
	 */
	TreeGeneratorPtr simplify(const TreeGeneratorPtr& a);


} // end namespace irg
} // end namespace generator
} // end namespace pattern
} // end namespace transform
} // end namespace insieme
