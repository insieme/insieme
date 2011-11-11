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
#include "insieme/transform/pattern/structure.h"
#include "insieme/transform/pattern/pattern.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {
namespace pattern {
namespace irp {
	using std::make_shared;

	inline TreePatternPtr atom(const core::NodePtr& node) {
		return atom(toTree(node));
	}

	inline TreePatternPtr atom(core::NodeManager& manager, const char* code) {
		auto a = [&manager] (const string& statementSpec) {return core::parse::parseStatement(manager, statementSpec); };
		return atom(a(string(code)));
	}

	inline TreePatternPtr atom(core::NodeManager& manager, string& code) {
		auto a = [&manager] (const string& statementSpec) {return core::parse::parseStatement(manager, statementSpec); };
		return atom(a(code));
	}

	inline TreePatternPtr tupleType(const ListPatternPtr& pattern) {
		return node(core::NT_TupleType, pattern);
	}
	inline TreePatternPtr genericType(const std::string& family, const ListPatternPtr& subtypes) {
		return node(core::NT_GenericType, atom(makeValue(family)) << single(node(subtypes)) << any);
	}
	inline TreePatternPtr genericType(const ListPatternPtr& family, const ListPatternPtr& subtypes) {
		return node(core::NT_GenericType, family << subtypes);
	}
	inline TreePatternPtr genericType(const std::string& family, const ListPatternPtr& subtypes, const ListPatternPtr& typeParams) {
		return node(core::NT_GenericType, atom(makeValue(family)) << single(node(subtypes)) << single(node(typeParams)));
	}

	inline TreePatternPtr lit(const std::string& value, const TreePatternPtr& typePattern) {
		return node(core::NT_Literal, single(typePattern) << atom(makeValue(value)));
	}
	inline TreePatternPtr lit(const TreePatternPtr& valuePattern, const TreePatternPtr& typePattern) {
		return node(core::NT_Literal, single(typePattern) << single(valuePattern));
	}
	inline TreePatternPtr call(const core::NodePtr& function, const ListPatternPtr& parameters) {
		return node(core::NT_CallExpr, atom(function) << parameters);
	}
	inline TreePatternPtr call(const TreePatternPtr& function, const ListPatternPtr& parameters) {
		return node(core::NT_CallExpr, single(function) << parameters);
	}

	inline TreePatternPtr wrap_body(const TreePatternPtr& body) {
		return body | node(core::NT_CompoundStmt, single(body));
	}

	inline TreePatternPtr compoundStmt(const ListPatternPtr& stmts) {
		return node(core::NT_CompoundStmt, stmts);
	}

	inline TreePatternPtr ifStmt(const TreePatternPtr& condition, const TreePatternPtr& thenBody, const TreePatternPtr& elseBody){
		return node(core::NT_IfStmt, single(condition) << wrap_body(thenBody) << wrap_body(elseBody));
	}


	inline TreePatternPtr returnStmt(const TreePatternPtr& returnExpression){
		return node(core::NT_ReturnStmt, single(returnExpression));
	}
}
} // end namespace pattern
} // end namespace transform
} // end namespace insieme
