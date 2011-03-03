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

// part of expression_parse moved to this file, just to make compilation of expresson parse faster

#include "insieme/core/parser/expression_parse.h"

#include "insieme/core/parser/type_parse.h"
#include "insieme/core/expressions.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ast_builder.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

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

CallExprPtr buildCallExpr(NodeManager& nodeMan, const ExpressionPtr& callee, ExpressionList arguments) {
    //TODO determine return type by inference (arguments may be empty!)
    ASTBuilder build(nodeMan);
    return build.callExpr(callee, arguments);
}

LiteralPtr buildCharLiteral(NodeManager& nodeMan, char val) {
    ASTBuilder build(nodeMan);
    return build.literal(nodeMan.basic.getChar(), toString(val));
}

ExpressionGrammarPart::ExpressionGrammarPart(NodeManager& nodeMan)
    : ExpressionGrammar::base_type(expressionRule), typeG(new TypeGrammar(nodeMan)) {

    auto nManRef = ph::ref(nodeMan);
    auto basicRef = ph::ref(nodeMan.basic);

    auto intTypeRef = ph::cref(nodeMan.basic.getInt4());

    // RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

    // terminals, no skip parser
    literalString =
      *(qi::char_ - ">")                                          ;

    // nonterminals, skip parser

    //// Let me tell you a little story about the folly of C++ compilers:
    //// Since there are 2 different Literal::get methods with the same number of parameters, when you directly
    //// pass it to phoenix::bind, the compiler is too stupid to figure out which one you mean. By creating this
    //// method pointer and using it instead, we help the compiler figure out which one to choose.
    //void (Literal::*get)(NodeManager&, const TypePtr&, const string&) = &Literal::get;
    // Fixed by adding parserGet to Literal, TODO should probably just remove one of the gets in Literal

    // literals --------------------------------------------------------------------------------------------------------------------------
    charLiteral =
        ( qi::lit("'") >> qi::char_ >> "'")                         [ qi::_val = ph::bind(&buildCharLiteral, nManRef, qi::_1) ];

    // -----------------------------------------------------------------------------------------------------------------------------------

    literalExpr =
      ( qi::lit("lit<") >> typeG->typeRule >> ','
        >> literalString >> '>' )                                   [ qi::_val = ph::bind(&Literal::parserGet, nManRef, qi::_1, qi::_2) ];

    opExpr =
        ( qi::lit("op<") >> literalString >> '>' )                  [ qi::_val = ph::bind(&lang::BasicGenerator::getLiteral, basicRef, qi::_1) ];

    callExpr =
        ( qi::lit("(") >> expressionRule >> '(' >> -(expressionRule [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> ')' >> ')' )                                    [ qi::_val = ph::bind(&buildCallExpr, nManRef, qi::_1, qi::_a) ];

    castExpr =
        ( qi::lit("CAST<") >> typeG->typeRule
        >> '>' >> '(' >> expressionRule >> ')' )                    [ qi::_val = ph::bind(&CastExpr::get, nManRef, qi::_1, qi::_2) ];

    memberAccessExpr =
        ( '(' >> expressionRule >> qi::lit(").")
        >> typeG->identifier )                                      [ qi::_val = ph::bind(&MemberAccessExpr::get, nManRef, qi::_1, qi::_2) ];

    tupleProjectionExpr =
        ( '(' >> expressionRule >> qi::lit(").")
        >> qi::ulong_long )                                         [ qi::_val = ph::bind(&TupleProjectionExpr::get, nManRef, qi::_1, qi::_2) ];

    markerExpr =
        ( '<' >> qi::lit("me") >> qi::lit("id") >> '='
        >> qi::ulong_long >> '>' >> expressionRule >> '<'
        >> '/' >> qi::lit("me") >> '>')                            [ qi::_val = ph::bind(&MarkerExpr::get, nManRef, qi::_2, qi::_1) ];


    // --------------------------------------------------------------------------------------
}

ExpressionGrammarPart::~ExpressionGrammarPart() {
    delete typeG;
}

} // namespace parse
} // namespace core
} // namespace insieme
