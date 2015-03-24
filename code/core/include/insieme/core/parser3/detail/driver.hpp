#pragma once

#include <string>
#include <iostream>
#include <map>

#include "insieme/core/parser3/detail/nodes.hpp"
#include "inspire_parser.hpp"
#include "insieme/core/parser3/detail/scanner.h"


// FLex is still a primitive tool using macros:
// Tell Flex the lexer's prototype ...
# define YY_DECL \
  insieme::core::parser3::detail::inspire_parser::symbol_type yylex (insieme::core::parser3::detail::inspire_driver& driver)
// ... and declare it for the parser's sake.
YY_DECL;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace insieme{
namespace core{
namespace parser3{
namespace detail{


/**
 *  The declaration context keeps track of all the symbols available in the scope,
 *  it is useful to keep track of the let bindings
 */
class DeclarationContext{
    using ctx_map_type = std::map<std::string, NNode*>;

    std::vector<ctx_map_type> scope_stack;
    ctx_map_type global_scope;
public:

    void open_scope(){
        scope_stack.push_back(ctx_map_type());
    }
    void close_scope(){
        scope_stack.pop_back();
    }

    void add_symb(const std::string& name, NNode* node){
        if (scope_stack.empty()) global_scope.insert({name, node});
        else  scope_stack.back().insert({name, node});
    }

    NNode* find(const std::string& name){
        ctx_map_type::iterator mit;
        for (auto it = scope_stack.rbegin(); it != scope_stack.rend(); ++it){
            if ((mit = it->find(name)) != it->end()) return mit->second;
        }
        if ((mit = global_scope.find(name)) != global_scope.end()) return mit->second;
        return nullptr;
    }

};


// Conducting the whole scanning and parsing of Calc++.
class inspire_driver
{
    scanner_wrapper* scanner;    
public:
    inspire_driver (const std::string& f, NodeKeeper& nk);
    virtual ~inspire_driver ();

    NodeKeeper& nodeKeeper;
    std::string file;
    const std::string& str;       
    NNode* result;

    DeclarationContext scopes;

    int parse ();

    // Error handling.
    void error (const location& l, const std::string& m);
    void error (const std::string& m);
};

} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme
