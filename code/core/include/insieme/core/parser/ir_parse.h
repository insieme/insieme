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

#include <exception>

#define BOOST_SPIRIT_DEBUG

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {
namespace parse {

// table holding all variables including their identifiers
class VariableTable {
    NodeManager& nodeMan;
    utils::map::PointerMap<StringValuePtr, VariablePtr> table;

public:
    VariableTable(NodeManager& nodeMan) : nodeMan(nodeMan) { }

    VariablePtr get(const TypePtr& typ, const StringValuePtr& id);
    VariablePtr lookup(const StringValuePtr& id);
};

class ParseException : public std::exception {
    string err;
public :
	const char* what() const throw() {
		return ("IR Parsing failed\n" + err).c_str();
	}

    ParseException() : err("Unspecified Error") {}

    ParseException(string errMsg) : err(errMsg) {}

    ~ParseException() throw() {}
};

class SemanticException : std::exception {
	string err;
public :
	const char* what() const throw() {
		return ("IR Generation failed\n" + err).c_str();
	}

	SemanticException() : err("") {}

	SemanticException(string errMsg) : err(errMsg) {}

	~SemanticException() throw() {}
};

/** A helper function for parsing an IR type declaration.
 ** If more than one definition should be parsed it is better to generate a parser object and call the parseType method
 ** @param nodeMan the NodeManager the generated definitions will be added to
 ** @param input the string representation of the IR definition to be parsed
 ** @return a pointer to an AST node representing the generated type
 **/
TypePtr parseType(NodeManager& nodeMan, const string& input);

/** A helper function for parsing an IR expression.
 ** If more than one expression should be parsed it is better to generate a parser object and call the parseExpression method
 ** @param nodeMan the NodeManager the generated nodes will be added to
 ** @param input the string representation of the IR expression to be parsed
 ** @return a pointer to an AST node representing the generated expression
 **/
ExpressionPtr parseExpression(NodeManager& nodeMan, const string& input);

/** A helper function for parsing an IR statement.
 ** If more than one statements should be parsed it is better to generate a parser object and call the parseStatement method
 ** @param nodeMan the NodeManager the generated nodes will be added to
 ** @param input the string representation of the IR statement to be parsed
 ** @return a pointer to an AST node representing the generated statement
 **/
StatementPtr parseStatement(NodeManager& nodeMan, const string& input);

/** A helper function for parsing an IR program.
 ** If more than one program should be parsed it is better to generate a parser object and call the parseProgram method
 ** @param nodeMan the NodeManager the generated nodes will be added to
 ** @param input the string representation of the IR program to be parsed
 ** @return a pointer to an AST node representing the generated program
 **/
ProgramPtr parseProgram(NodeManager& nodeMan, const string& input);

/** A helper function for parsing IR node.
 ** If more than one node should be parsed it is better to generate a parser object and call the parseIR method
 ** @param nodeMan the NodeManager the generated nodes will be added to
 ** @param input the string representation of the IR program to be parsed
 ** @return a pointer to an AST node representing the generated AST
 **/
NodePtr parseIR(NodeManager& nodeMan, const string& input);


namespace qi = boost::spirit::qi;
typedef std::string::const_iterator ParseIt;

class IRParser {
	NodeManager& nodeMan;

public:
	IRParser(NodeManager& nodeMan);

	TypePtr parseType(const std::string& input);
	ExpressionPtr parseExpression(const std::string& input);
	StatementPtr parseStatement(const std::string& input);
	ProgramPtr parseProgram(const std::string& input);
	NodePtr parseIR(const std::string& input);
};

} // namespace parse 
} // namespace core
} // namespace insieme
