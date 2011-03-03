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

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/parser/type_parse.h"
#include "insieme/core/parser/expression_parse.h"
#include "insieme/core/parser/statement_parse.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;

IRParser::IRParser(NodeManager& nodeMan) : nodeMan(nodeMan) {

}

TypePtr IRParser::parseType(const std::string& input) {
	TypePtr result;
	TypeGrammar typeGrammar(nodeMan);
	auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = qi::phrase_parse(startIt, endIt, typeGrammar, qi::space, result);
	parse_result = parse_result && (startIt == endIt);
	if(!parse_result) throw ParseException();
	return result;
}

ExpressionPtr IRParser::parseExpression(const std::string& input) {
	ExpressionPtr result;
	ExpressionGrammar exprGrammar(nodeMan);
	auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = qi::phrase_parse(startIt, endIt, exprGrammar, qi::space, result);
	parse_result = parse_result && (startIt == endIt);
	if(!parse_result) throw ParseException();
	return result;
}

StatementPtr IRParser::parseStatement(const std::string& input) {
    StatementPtr result;
    StatementGrammar stmtGrammar(nodeMan);
    auto startIt = input.cbegin(), endIt = input.cend();
    bool parse_result = qi::phrase_parse(startIt, endIt, stmtGrammar, qi::space, result);
    parse_result = parse_result && (startIt == endIt);
    if(!parse_result) throw ParseException();
    return result;
//    return ReturnStmt::get(nodeMan, parseExpression("0"));
}

TypePtr parseType(NodeManager& nodeMan, const string& input) {
	IRParser parser(nodeMan);
	return parser.parseType(input);
}
ExpressionPtr parseExpression(NodeManager& nodeMan, const string& input) {
	IRParser parser(nodeMan);
	return parser.parseExpression(input);
}
StatementPtr parseStatement(NodeManager& nodeMan, const string& input) {
    IRParser parser(nodeMan);
    return parser.parseStatement(input);
}

} // namespace parse 
} // namespace core
} // namespace insieme
