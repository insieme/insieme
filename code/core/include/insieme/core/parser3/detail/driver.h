#pragma once

#include <string>
#include <sstream>
#include <iostream>
#include <map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/parser3/detail/scanner.h"

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

public:

    DeclarationContext()
    {}
    DeclarationContext(const DeclarationContext& o) 
    :global_scope(o.global_scope.begin(), o.global_scope.end())
    {
        for(const auto& scope : o.scope_stack){
            scope_stack.push_back(ctx_map_type(scope.begin(), scope.end()));
        }
    }

    // ~~~~~~~~~~~~~~~~~~~~~ scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

    void open_scope(const std::string& msg = "");
    void close_scope(const std::string& msg = "");

    bool add_symb(const std::string& name, NodePtr node);
    NodePtr find(const std::string& name) const;
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

public:
    inspire_driver (const std::string& f, NodeManager& nk, const DeclarationContext& ctx = DeclarationContext());
    virtual ~inspire_driver ();

    NodeManager& mgr;
    IRBuilder builder;
    std::string file;
    const std::string& str;       
    NodePtr result;

    location glob_loc;
    bool in_let;

private:
    std::stringstream ss;
    inspire_scanner scanner;
    inspire_parser parser;

    bool inhibit_building_flag;

    struct Lambda_let{
        TypePtr retType;
        VariableList params;
        std::string  expression;
        Lambda_let( const TypePtr& retType, const VariableList& params, const std::string& expression)
        : retType(retType), params(params.begin(), params.end()), expression(expression) {}
    };
    std::vector<Lambda_let>    lambda_lets;
    std::vector<ExpressionPtr> closure_lets;
    std::vector<TypePtr>       type_lets;
    std::vector<std::string>   let_names;


public:
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

    void add_let_lambda(const location& l, const location& bodyb, const location& bodye, const TypePtr& retType, const VariableList& params = VariableList());
    void add_let_type(const location& l, const TypePtr& type);
    void add_let_closure(const location& l, const ExpressionPtr& closure);

    void add_let_name(const location& l, const std::string& name); 
    void close_let_statement(const location& l); 

    ExpressionPtr genTagExpression(const location& l, const TypePtr& type, const NamedValueList& fields);
    ExpressionPtr genTagExpression(const location& l, const NamedValueList& fields);

    VariableIntTypeParamPtr gen_type_param_var(const location& l, const std::string& name);
    VariableIntTypeParamPtr find_type_param_var(const location& l, const std::string& name);

    void add_symb(const location& l, const std::string& name, NodePtr ptr);
    void add_symb(const std::string& name, NodePtr ptr);

    void open_scope(const location& l, const std::string& );
    void close_scope(const location& l, const std::string&);

    // syntatic parsing, no build (this can be used to jump over large ranges of code to do subscooping
    bool inhibit_building()const;
    void set_inhibit(bool flag =true);

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
