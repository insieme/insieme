#pragma once

#include <string>
#include <iostream>
#include <map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"


#include "inspire_parser.hpp"

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

    std::vector<std::string>       unfinished_symbols;
    std::vector<std::string>       let_symbols;
public:

    // ~~~~~~~~~~~~~~~~~~~~~ scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

    void open_scope(const std::string& msg = "");
    void close_scope(const std::string& msg = "");

    bool add_symb(const std::string& name, NodePtr node);
    NodePtr find(const std::string& name) const;

    // ~~~~~~~~~~~~~~~~~~~~~ recursive types construction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

    void add_unfinish_symbol(const std::string& name);
    std::string get_unfinish_symbol();
    bool is_unfinished(const std::string& name)const;

    bool all_symb_defined();

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

    DeclarationContext scopes;

    std::map<std::string, NodePtr> recursive_symbols;
    std::map<std::string, std::pair<LiteralPtr, VariablePtr>> rec_function_vars;

    LiteralPtr get_tag_function(const std::string& name);
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
    ExpressionPtr findSymbol(const location& l, const std::string& name);
    TypePtr findType(const location& l, const std::string& name);

    ExpressionPtr getOperand(ExpressionPtr expr);
    ExpressionPtr genBinaryExpression(const location& l, const std::string& op, ExpressionPtr left, ExpressionPtr right);

    TypePtr genGenericType(const location& l, const std::string& name, const TypeList& params, const IntParamList& iparamlist);
    TypePtr genFuncType(const location& l, const TypeList& params, const TypePtr& retType, bool closure = false);

    ExpressionPtr genLambda(const location& l, const VariableList& params, StatementPtr body);
    ExpressionPtr genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body);
    ExpressionPtr genClosure(const location& l, const VariableList& params, StatementPtr body);
    ExpressionPtr genCall(const location& l, const ExpressionPtr& func, ExpressionList params);

    ExpressionPtr genTagExpression(const location& l, const TypePtr& type, const NamedValueList& fields);
    ExpressionPtr genTagExpression(const location& l, const NamedValueList& fields);

    VariableIntTypeParamPtr gen_type_param_var(const location& l, const std::string& name);
    VariableIntTypeParamPtr find_type_param_var(const location& l, const std::string& name);

    void add_symb(const location& l, const std::string& name, NodePtr ptr);
    void add_symb(const std::string& name, NodePtr ptr);

    void add_unfinish_symbol(const location& l, const std::string& name);
    bool close_unfinish_symbol(const location& l, const NodePtr& node);
    bool all_symb_defined(const location& l);

    void open_scope(const location& l, const std::string& );
    void close_scope(const location& l, const std::string&);

    // Error handling.
    void error (const location& l, const std::string& m)const;
    void error (const std::string& m)const;
    bool where_errors()const ;
    void print_errors(std::ostream& out = std::cout)const;
};

} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme
