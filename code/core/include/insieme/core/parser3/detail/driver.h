#pragma once

#include <string>
#include <iostream>
#include <map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"


#include "inspire_parser.hpp"
#include "insieme/core/parser3/detail/scanner.h"




// FLex is still a primitive tool using macros:
// Tell Flex the lexer's prototype ...
# define YY_DECL \
  insieme::core::parser3::detail::inspire_parser::symbol_type yylex (insieme::core::parser3::detail::inspire_driver& driver, \
                                                                     insieme::core::parser3::detail::inspire_parser::symbol_type** start_token)
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
    using ctx_map_type = std::map<std::string, NodePtr>;

    std::vector<ctx_map_type> scope_stack;
    ctx_map_type global_scope;

    std::vector<std::string> unfinished_symbols;
public:

    void open_scope(const std::string& msg = ""){
        for (unsigned i =0; i< scope_stack.size(); ++i) std::cout << " ";
//        std::cout << "open scope: " << msg << std::endl;
        scope_stack.push_back(ctx_map_type());
    }
    void close_scope(const std::string& msg = ""){
        scope_stack.pop_back();
        for (unsigned i =0; i< scope_stack.size(); ++i) std::cout << " ";
//        std::cout << "close scope: " << msg <<  std::endl;
    }

    bool add_symb(const std::string& name, NodePtr node){
        std::cout <<"                             " <<  "add: " << name <<  " : "<< node << std::endl;
        if (scope_stack.empty()) {
            if (global_scope.find(name) != global_scope.end()) { 
                return false;
            }
            global_scope[name] =  node;
            std::cout <<"                             " <<  name << " is "  <<  global_scope[name] << std::endl;
        }
        else  {
            if (scope_stack.back().find(name) != scope_stack.back().end()) { 
                return false;
            }
            scope_stack.back()[name] =  node;
            std::cout <<"                             " <<  name << " is "  <<  scope_stack.back()[name] << std::endl;
        }
        return true;
    }

    NodePtr find(const std::string& name) const{

//        std::cout <<"                             " <<  "find: " << name << std::endl;
//        std::cout <<"                                 " <<  global_scope << std::endl;
//        std::cout <<"                                 " <<  scope_stack << std::endl;

        ctx_map_type::const_iterator mit;
        for (auto it = scope_stack.rbegin(); it != scope_stack.rend(); ++it){
            if ((mit = it->find(name)) != it->end()) return mit->second;
        }
        if ((mit = global_scope.find(name)) != global_scope.end()) return mit->second;
        return nullptr;
    }

    void add_unfinish_symbol(const std::string& name){
        unfinished_symbols.push_back(name);
    }

    std::string get_unfinish_symbol(){
        auto x = unfinished_symbols.back();
        unfinished_symbols.pop_back();
        return x;
    }

    bool all_symb_defined(){
        return unfinished_symbols.empty();
    }

};


// Conducting the whole scanning and parsing of Calc++.
class inspire_driver
{

    struct t_error{
        location l;
        std::string msg;
        t_error(const location& l, const std::string& msg)
        :l(l), msg(msg) {}
    };
    mutable std::vector<t_error> errors;

    scanner_wrapper* scanner;    
    DeclarationContext scopes;
public:
    inspire_driver (const std::string& f, NodeManager& nk);
    virtual ~inspire_driver ();

    NodeManager& mgr;
    IRBuilder builder;
    std::string file;
    const std::string& str;       
    NodePtr result;


    location glob_loc;

    ProgramPtr parseProgram ();
    TypePtr parseType ();
    StatementPtr parseStmt ();
    ExpressionPtr parseExpression ();

    // tools
    ExpressionPtr findSymbol(const location& l, const std::string& name) const;
    TypePtr findType(const location& l, const std::string& name) const;

    ExpressionPtr getOperand(ExpressionPtr expr);
    ExpressionPtr genBinaryExpression(const location& l, const std::string& op, ExpressionPtr left, ExpressionPtr right);

    TypePtr genGenericType(const location& l, const std::string& name, const TypeList& params, const IntParamList& iparamlist);
    TypePtr genFuncType(const location& l, const TypeList& params, const TypePtr& retType, bool closure = false);

    ExpressionPtr genLambda(const location& l, const VariableList& params, StatementPtr body);
    ExpressionPtr genClosure(const location& l, const VariableList& params, StatementPtr body);
    ExpressionPtr genCall(const location& l, const ExpressionPtr& func, ExpressionList params);

    void add_symb(const location& l, const std::string& name, NodePtr ptr);
    void add_symb(const std::string& name, NodePtr ptr);

    void add_unfinish_symbol(const location& l, const std::string& name);
    void close_unfinish_symbol(const location& l, const NodePtr& node);
    void all_symb_defined(const location& l);

    void open_scope(const location& l, const std::string& );
    void close_scope(const location& l, const std::string&);

    // Error handling.
    void error (const location& l, const std::string& m)const;
    void error (const std::string& m)const;
    void print_errors(std::ostream& out = std::cout)const;
};

} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme
