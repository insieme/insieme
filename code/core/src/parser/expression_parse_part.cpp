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

#include "insieme/core/parser/expression_parse_part.h"
#include "insieme/core/parser/expression_parse.h"
#include "insieme/core/parser/statement_parse.h"
#include "insieme/core/parser/type_parse.h"
#include "insieme/core/expressions.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ast_builder.h"

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_ascii.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

// ----------------------- SPIRIT QI/PHOENIX survival hints
// What to do if
// - error: invalid initialization ... --> check whether ph::ref is used to pass references
//                                     --> check if you're using the right placeholders (char_ counts as auto attrib, literals and plain characters do NOT)
// - error: template substitution failure --> check potential ambiguities, try supplying default parameters explicitly
// - error: some operator can not be applied (eg =) --> attribute type may be different from expected
// - error: invalid use of void expression --> trying to do a ph::ref of a reference? Placeholders are already references!
// - error: <some callable> is not a class, struct or union in ph::bind --> check argument count and type
// - other: use phoenix::bind and phoenix::construct instead of plain calls
// ----------------------- - Peter
// - error: has no member named ‘parse’ --> check your operators (>>!)
// - error: is not a class, struct, or union type --> chek function name and argument list

namespace insieme {
namespace core {
namespace parse {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace ph = boost::phoenix;
/*
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
        std::cerr << "Variable '" << id << "' not defined.\n";
        throw ParseException();
    }

    return entry->second;
}
*/
LiteralPtr buildCharLiteral(NodeManager& nodeMan, char val) {
    ASTBuilder build(nodeMan);
    return build.literal(nodeMan.basic.getChar(), toString(val));
}

// obsolete
LiteralPtr buildNat(NodeManager& nodeMan, uint64_t val) {
    ASTBuilder build(nodeMan);
    return build.literal(nodeMan.basic.getUInt8(), toString(val));
}

ExpressionGrammarPart::ExpressionGrammarPart(NodeManager& nodeMan, ExpressionGrammar* exprGram)
    : ExpressionGrammarPart::base_type(expressionPart), exprG(exprGram)/*, varTab(nodeMan)*/ {
    typeG = new TypeGrammar(nodeMan);

    auto nManRef = ph::ref(nodeMan);
    auto basicRef = ph::ref(nodeMan.basic);

    auto intTypeRef = ph::cref(nodeMan.basic.getInt4());
    // RULES ---------------------------------------------------- | ACTIONS ----------------------------------------------------------------------------------

    // terminals, no skip parser
    literalString = //exprGpart->literalString;
        *(qi::char_ - ">");

    // nonterminals, skip parser

    //// Let me tell you a little story about the folly of C++ compilers:
    //// Since there are 2 different Literal::get methods with the same number of parameters, when you directly
    //// pass it to phoenix::bind, the compiler is too stupid to figure out which one you mean. By creating this
    //// method pointer and using it instead, we help the compiler figure out which one to choose.
    //void (Literal::*get)(NodeManager&, const TypePtr&, const string&) = &Literal::get;
    // Fixed by adding parserGet to Literal, TODO should probably just remove one of the gets in Literal

    // literals --------------------------------------------------------------------------------------------------------------------------
    charLiteral = //exprGpart->charLiteral;
        ( qi::lit("'") >> qi::char_ >> "'")                         [ qi::_val = ph::bind(&buildCharLiteral, nManRef, qi::_1) ];

    // -----------------------------------------------------------------------------------------------------------------------------------

    literalExpr = //exprGpart->literalExpr;
        ( qi::lit("lit<") >> typeG->typeRule >> ','
        >> literalString >> '>' )                                   [ qi::_val = ph::bind(&Literal::parserGet, nManRef, qi::_1, qi::_2) ];

    opExpr = //exprGpart->opExpr;
        ( qi::lit("op<") >> literalString >> '>' )                  [ qi::_val = ph::bind(&lang::BasicGenerator::getLiteral, basicRef, qi::_1) ];

    variableExpr =
        ( typeG->typeRule >> ':' >> typeG->identifier )             [ qi::_val = ph::bind(&VariableTable::get, &exprG->varTab, qi::_1, qi::_2) ]
        | typeG->identifier                                         [ qi::_val = ph::bind(&VariableTable::lookup, &exprG->varTab, qi::_1) ];

    // specialized type to ensure type safety
    funVarExpr =
        ( typeG->functionType >> ':' >> typeG->identifier )         [ qi::_val = ph::bind(&VariableTable::get, &exprG->varTab, qi::_1, qi::_2) ];

    castExpr = //exprGpart->castExpr;
        ( qi::lit("CAST<") >> typeG->typeRule
        >> '>' >> '(' >> exprG->expressionRule >> ')' )             [ qi::_val = ph::bind(&CastExpr::get, nManRef, qi::_1, qi::_2) ];

    // --------------------------------------------------------------------------------------
    captureInitExpr =
        ( qi::lit("[") >> +( exprG->expressionRule                 [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> qi::lit("]") >> exprG->expressionRule)         [ qi::_val = ph::bind(&CaptureInitExpr::get, nManRef, qi::_2, qi::_a ) ];

    tupleExpr =
        ( qi::lit("tuple[") >> -(exprG->expressionRule              [ ph::push_back(qi::_a, qi::_1) ]
          % ',') >> ']' )                                           [ qi::_val = ph::bind(&TupleExpr::get, nManRef, qi::_a) ];

    vectorExpr =
        ( qi::lit("vector") >> '<' >>
        (typeG->typeRule >> ',' >> typeG->intTypeParam)             [ qi::_a = ph::bind(&VectorType::get, nManRef, qi::_1, qi::_2 ) ]
        >> '>' >> '(' >> -(exprG->expressionRule                    [ ph::push_back(qi::_b, qi::_1) ]
          % ',') >> ')' )                                           [ qi::_val = ph::bind(&VectorExpr::get, nManRef, qi::_a, qi::_b ) ];

    structExpr =
        ( qi::lit("struct") >> '{' >> -(
          (typeG->identifier >> ':' >> exprG->expressionRule )      [ ph::push_back(qi::_a, ph::bind(&makePair<IdentifierPtr, ExpressionPtr>, qi::_1, qi::_2 )) ]
          % ',') >> '}' )                                           [ qi::_val = ph::bind(&StructExpr::get, nManRef, qi::_a ) ];

    unionExpr =
        ( qi::lit("union") >> '<' >> typeG->typeRule >> '>'
        >> '{' >> typeG->identifier >> ':'
        >> exprG->expressionRule >> '}' )                           [ qi::_val = ph::bind(&UnionExpr::get, nManRef, qi::_1, qi::_2, qi::_3) ];

    memberAccessExpr = //exprGpart->memberAccessExpr;
        ( '(' >> exprG->expressionRule >> qi::lit(").")
        >> typeG->identifier )                                      [ qi::_val = ph::bind(&MemberAccessExpr::get, nManRef, qi::_1, qi::_2) ];

    tupleProjectionExpr = //exprGpart->tupleProjectionExpr;
        ( '(' >> exprG->expressionRule >> qi::lit(").")
        >> qi::ulong_long )                                         [ qi::_val = ph::bind(&TupleProjectionExpr::get, nManRef, qi::_1, qi::_2) ];

    markerExpr =
        ( '<' >> qi::lit("me") >> qi::lit("id") >> '='
        >> qi::ulong_long >> '>' >> exprG->expressionRule >> '<'
        >> '/' >> qi::lit("me") >> '>')                             [ qi::_val = ph::bind(&MarkerExpr::get, nManRef, qi::_2, qi::_1) ];

    // --------------------------------------------------------------------------------------
//    BOOST_SPIRIT_DEBUG_NODE(literalString);
//    BOOST_SPIRIT_DEBUG_NODE(charLiteral);
//    BOOST_SPIRIT_DEBUG_NODE(lambdaDef);
//    BOOST_SPIRIT_DEBUG_NODE(lambdaExpr);
//    BOOST_SPIRIT_DEBUG_NODE(captureInitExpr);
//    BOOST_SPIRIT_DEBUG_NODE(tupleExpr);
//    BOOST_SPIRIT_DEBUG_NODE(vectorExpr);
//    BOOST_SPIRIT_DEBUG_NODE(structExpr);
//    BOOST_SPIRIT_DEBUG_NODE(unionExpr);
//    BOOST_SPIRIT_DEBUG_NODE(memberAccessExpr);
//    BOOST_SPIRIT_DEBUG_NODE(tupleProjectionExpr);
//    BOOST_SPIRIT_DEBUG_NODE(markerExpr);
    // --------------------------------------------------------------------------------------
//    BOOST_SPIRIT_DEBUG_NODE(literalExpr);
//    BOOST_SPIRIT_DEBUG_NODE(opExpr);
//    BOOST_SPIRIT_DEBUG_NODE(variableExpr);
//    BOOST_SPIRIT_DEBUG_NODE(callExpr);
//    BOOST_SPIRIT_DEBUG_NODE(castExpr);
//    BOOST_SPIRIT_DEBUG_NODE(expressionRule);
}

ExpressionGrammarPart::~ExpressionGrammarPart() {
    delete typeG;
}

} // namespace parse
} // namespace core
} // namespace insieme

