/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <vector>
#include <sstream>

// flex lexer base class
#undef yyFlexLexer
#define yyFlexLexer Trick_Lexer
# include <FlexLexer.h>

// these file is generated and the path is provided
# include "location.hh"
# include "inspire_parser.hpp"

namespace insieme{
namespace core{
namespace parser3{
namespace detail{

    class inspire_driver;

    /**
     *  the scanner wrapper is an interface to implement differen inputs for the scanner
     */
    class inspire_scanner : public Trick_Lexer
    {
        location loc;

        inspire_parser::symbol_type type_token;
        inspire_parser::symbol_type expression_token;
        inspire_parser::symbol_type statement_token;
        inspire_parser::symbol_type program_token;

        inspire_parser::symbol_type*  start_token;

    public:

        inspire_scanner(std::istream* stream)
        : Trick_Lexer(stream), 
          type_token(inspire_parser::make_TYPE_ONLY(loc)),
          expression_token(inspire_parser::make_EXPRESSION_ONLY(loc)),
          statement_token(inspire_parser::make_STMT_ONLY(loc)),
          program_token(inspire_parser::make_FULL_PROGRAM(loc))
        {
            loc.initialize();
        }

    #undef YY_DECL
    # define YY_DECL \
      inspire_parser::symbol_type inspire_scanner::yylex ( inspire_driver& driver)
        inspire_parser::symbol_type yylex (inspire_driver& driver);

        void set_start_program(){
            start_token = &program_token;
        }
        void set_start_statement(){
            start_token = &statement_token;
        }
        void set_start_expression(){
            start_token = &expression_token;
        }
        void set_start_type(){
            start_token = &type_token;
        }

        int yywrap(){ return 1; }

        virtual ~inspire_scanner(){ }
    };

    /**
     * Bison-Flex interaction in c++ mode is not very neat:
     * Bison will call a function yylex, and to keep an unique instance of the scanner, we pass it by argument 
     * this function bridges the bison flex interaction.
     */
    inspire_parser::symbol_type yylex (inspire_driver& driver, inspire_scanner& scanner);

} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

