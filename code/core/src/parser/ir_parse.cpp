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
#include "insieme/core/parser/program_parse.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>

namespace insieme {
namespace core {
namespace parse {

// variable table implementation, used by ExpressionGrammar and ExpressionGrammarPart
VariablePtr VariableTable::get(const TypePtr& typ, const IdentifierPtr& id) {
    auto entry = table.find(id);
    if(entry != table.end()) {
        assert(entry->second->getType() == typ);
        return entry->second;
    }
    VariablePtr newVar = Variable::get(nodeMan, typ);
    table[id] = newVar;

    return newVar;
}

VariablePtr VariableTable::lookup(const IdentifierPtr& id) {
    auto entry = table.find(id);

    if(entry == table.end()) {
		const string err = "FAIL at '" + id->toString() + "'\n";
        throw ParseException(err);
    }

    return entry->second;
}


namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;

IRParser::IRParser(NodeManager& nodeMan) : nodeMan(nodeMan) {

}

TypePtr IRParser::parseType(const std::string& input) {
	TypePtr result;
	TypeGrammar<TypePtr, IntTypeParamPtr, IdentifierPtr> typeGrammar(nodeMan);
	auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = false;
	try {
		parse_result = qi::phrase_parse(startIt, endIt, typeGrammar, qi::space, result);
		parse_result = parse_result && (startIt == endIt);
	} catch(ParseException& pe) {
		std::cerr << "ERROR: " << pe.what() << std::endl;
	} catch(SemanticException& se) {
		std::cerr << "Semantic ERROR: " << se.what() << std::endl;
	}
	if(!parse_result) throw ParseException("Parsing of Type FAILED");
	return result;
}

ExpressionPtr IRParser::parseExpression(const std::string& input) {
	ExpressionPtr result;
	ExpressionGrammar<ExpressionPtr, StatementPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr> exprGrammar(nodeMan);
	auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = false;
	try {
		parse_result = qi::phrase_parse(startIt, endIt, exprGrammar, qi::space, result);
		parse_result = parse_result && (startIt == endIt);
	} catch(ParseException& pe) {
		std::cerr << "ERROR: " << pe.what() << std::endl;
	} catch(SemanticException& se) {
		std::cerr << "Semantic ERROR: " << se.what() << std::endl;
	}
	if(!parse_result) throw ParseException("Parsing of Expression FAILED");
	return result;
}

StatementPtr IRParser::parseStatement(const std::string& input) {
    StatementPtr result;
    StatementGrammar<StatementPtr, ExpressionPtr, TypePtr, IntTypeParamPtr, IdentifierPtr, LambdaPtr, LambdaDefinitionPtr> stmtGrammar(nodeMan);
    auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = false;
	try {
		parse_result = qi::phrase_parse(startIt, endIt, stmtGrammar, qi::space, result);
		parse_result = parse_result && (startIt == endIt);
	} catch(ParseException& pe) {
		std::cerr << "ERROR: " << pe.what() << std::endl;
	} catch(SemanticException& se) {
		std::cerr << "Semantic ERROR: " << se.what() << std::endl;
	}
	if(!parse_result) throw ParseException("Parsing of Statement FAILED");
    return result;
}

ProgramPtr IRParser::parseProgram(const std::string& input) {
    ProgramPtr result;
    ProgramGrammar<ProgramPtr, ExpressionPtr> progGrammar(nodeMan);
    auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = false;
	try {
		parse_result = qi::phrase_parse(startIt, endIt, progGrammar, qi::space, result);
		parse_result = parse_result && (startIt == endIt);
	} catch(ParseException& pe) {
		std::cerr << "Parsing ERROR: " << pe.what() << std::endl;
	} catch(SemanticException& se) {
		std::cerr << "Semantic ERROR: " << se.what() << std::endl;
	}
	if(!parse_result) throw ParseException("Parsing of Program FAILED");
    return result;
}

NodePtr IRParser::parseIR(const std::string& input) {
    NodePtr result;
    IRGrammar<ProgramPtr, ExpressionPtr> irGrammar(nodeMan);
    auto startIt = input.cbegin(), endIt = input.cend();
	bool parse_result = false;
	try {
		parse_result = qi::phrase_parse(startIt, endIt, irGrammar, qi::space, result);
		parse_result = parse_result && (startIt == endIt);
	} catch(insieme::core::parse::ParseException& pe) {
		std::cerr << "ERROR: " << pe.what() << std::endl;
	} catch(SemanticException& se) {
		std::cerr << "Semantic ERROR: " << se.what() << std::endl;
	}
	if(!parse_result) throw ParseException("Parsing of IR Code FAILED");
    return result;
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

ProgramPtr parseProgram(NodeManager& nodeMan, const string& input) {
    IRParser parser(nodeMan);
    return parser.parseProgram(input);
}

NodePtr parseIR(NodeManager& nodeMan, const string& input) {
    IRParser parser(nodeMan);
    return parser.parseIR(input);
}


} // namespace parse 
} // namespace core
} // namespace insieme
