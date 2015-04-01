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

    public:

        inspire_scanner(std::istream* stream)
        : Trick_Lexer(stream)
        {
            loc.initialize();
        }

    #undef YY_DECL
    # define YY_DECL \
      inspire_parser::symbol_type inspire_scanner::yylex ( inspire_driver& driver, inspire_parser::symbol_type** start_token)
        inspire_parser::symbol_type yylex (inspire_driver& driver, inspire_parser::symbol_type** start_token);


        int yywrap(){ return 1; }

        virtual ~inspire_scanner(){ }
    };

    /**
     * Bison-Flex interaction in c++ mode is not very neat:
     * Bison will call a function yylex, and to keep an unique instance of the scanner, we pass it by argument 
     * this function bridges the bison flex interaction.
     */
    inspire_parser::symbol_type yylex (inspire_driver& driver, inspire_parser::symbol_type** start_token, inspire_scanner& scanner);

} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

