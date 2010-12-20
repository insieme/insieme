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

#include "insieme/core/parser/expression_parse.h"

#include "insieme/core/parser/type_parse.h"
#include "insieme/core/expressions.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

// ----------------------- SPIRIT QI/PHOENIX survival hints
// What to do if
// - error: invalid initialization ... --> check whether ph::ref is used to pass references
//									   --> check if you're using the right placeholders (char_ counts as auto attrib, literals and plain characters do NOT)
// - error: template substitution failure --> check potential ambiguities, try supplying default parameters explicitly
// - error: some operator can not be applied (eg =) --> attribute type may be different from expected
// - error: invalid use of void expression --> trying to do a ph::ref of a reference? Placeholders are already references!
// - other: use phoenix::bind and phoenix::construct instead of plain calls
// ----------------------- - Peter

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;

VariablePtr VariableTable::get(const TypePtr& typ, const Identifier& id) {
	auto entry = table.find(id);
	if(entry != table.end()) {
		assert(entry->second->getType() == typ);
		return entry->second;
	}
	VariablePtr newVar = Variable::get(nodeMan, typ);
	table[id] = newVar;
	return newVar;
}

ExpressionGrammar::ExpressionGrammar(NodeManager& nodeMan) 
	: ExpressionGrammar::base_type(expressionRule), typeG(new TypeGrammar(nodeMan)), varTab(nodeMan) {

	auto nManRef = ph::ref(nodeMan);

	// RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

	// terminals, no skip parser

	literalString = 
		*(qi::char_ - ">")											[ qi::_val = qi::_1 ];
	
	// nonterminals, skip parser

	//// Let me tell you a little story about the folly of C++ compilers:
	//// Since there are 2 different Literal::get methods with the same number of parameters, when you directly
	//// pass it to phoenix::bind, the compiler is too stupid to figure out which one you mean. By creating this 
	//// method pointer and using it instead, we help the compiler figure out which one to choose.
	//void (Literal::*get)(NodeManager&, const TypePtr&, const string&) = &Literal::get;
	// Fixed by adding parserGet to Literal, TODO should probably just remove one of the gets in Literal

	literalExpr =
		( qi::lit("lit<") >> typeG->typeRule >> ',' 
		>> literalString >> '>' )									[ qi::_val = ph::bind(&Literal::parserGet, nManRef, qi::_1, qi::_2) ];

	variableExpr =
		( typeG->typeRule >> ':' >> typeG->identifier )				[ qi::_val = ph::bind(&VariableTable::get, &varTab, qi::_1, qi::_2) ];

	expressionRule =
		literalExpr													[ qi::_val = ph::construct<ExpressionPtr>(qi::_1) ];
}

ExpressionGrammar::~ExpressionGrammar() {
	delete typeG;
}

} // namespace parse 
} // namespace core
} // namespace insieme

