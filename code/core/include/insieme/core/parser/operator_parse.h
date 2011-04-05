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

#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace parse {

// FW Declaration
template<typename T> struct ExpressionGrammar;

template <typename T>
struct OperatorGrammar : public qi::grammar<ParseIt, T(), qi::space_type> {
    ExpressionGrammar<T>* exprG;
    lang::BasicGenerator* generator;
    OperatorGrammar(NodeManager& nodeMan, ExpressionGrammar<T>* exprGram);
    ~OperatorGrammar();

    const boost::phoenix::actor<boost::phoenix::reference<insieme::core::NodeManager> >&& nManRef;
    NodeManager& nodeMan;

    qi::rule<ParseIt, T(), qi::space_type> assignment;
    qi::rule<ParseIt, T(), qi::space_type> addition;
    qi::rule<ParseIt, T(), qi::space_type> subtraction;
    qi::rule<ParseIt, T(), qi::space_type> multiplication;
    qi::rule<ParseIt, T(), qi::space_type> division;
    qi::rule<ParseIt, T(), qi::space_type> modulo;
    qi::rule<ParseIt, T(), qi::space_type> and_;
    qi::rule<ParseIt, T(), qi::space_type> or_;
    qi::rule<ParseIt, T(), qi::space_type> xor_;
    qi::rule<ParseIt, T(), qi::space_type> lShift;
    qi::rule<ParseIt, T(), qi::space_type> rShift;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, T(), qi::space_type> not_;
    qi::rule<ParseIt, T(), qi::space_type> plus;
    qi::rule<ParseIt, T(), qi::space_type> minus;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, T(), qi::space_type> preInc;
    qi::rule<ParseIt, T(), qi::space_type> postInc;
    qi::rule<ParseIt, T(), qi::space_type> preDec;
    qi::rule<ParseIt, T(), qi::space_type> postDec;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, T(), qi::space_type> lAnd;
    qi::rule<ParseIt, T(), qi::space_type> lOr;
    qi::rule<ParseIt, T(), qi::space_type> lNot;
    qi::rule<ParseIt, T(), qi::space_type> Eq;
    qi::rule<ParseIt, T(), qi::space_type> Ne;
    qi::rule<ParseIt, T(), qi::space_type> Lt;
    qi::rule<ParseIt, T(), qi::space_type> Le;
    qi::rule<ParseIt, T(), qi::space_type> Gt;
    qi::rule<ParseIt, T(), qi::space_type> Ge;

    // --------------------------------------------------------------------------------------

    qi::rule<ParseIt, T(), qi::space_type> operatorRule;

    // member functions providing the rules
    virtual qi::rule<ParseIt, T(), qi::space_type> getAssignment();
    #define get(op) virtual qi::rule<ParseIt, T(), qi::space_type> get##op ();
    get(Add)
    get(Sub)
    get(Mul)
    get(Div)
    get(Mod)
    get(And)
    get(Or)
    get(Xor)
    get(LShift)
    get(RShift)
    get(Not)
    get(Plus)
    get(Minus)
    get(PreInc)
    get(PostInc)
    get(PreDec)
    get(PostDec)
    get(LAnd)
    get(LOr)
    get(LNot)
    get(Eq)
    get(Ne)
    get(Lt)
    get(Le)
    get(Gt)
    get(Ge)
    #undef get

    // member functions creating the Objects
    virtual T getAssignmentHelper(ExpressionPtr a, ExpressionPtr b);
    virtual T getBinaryOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b);
    virtual T getInt4OpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b);
    virtual T getUnaryOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a);
    virtual T getSignOperation(const lang::BasicGenerator::Operator& op, ExpressionPtr b);
    virtual T getInplaceOperation(const lang::BasicGenerator::Operator& op, ExpressionPtr a);
    virtual T getLazyOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b);
    virtual T getBoolOpHelper(const lang::BasicGenerator::Operator& op, ExpressionPtr a, ExpressionPtr b);

};

}
}
}
