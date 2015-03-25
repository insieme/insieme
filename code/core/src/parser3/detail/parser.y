%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.0.4"
%defines
%define parser_class_name {inspire_parser}
%define api.namespace {insieme::core::parser3::detail}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%code requires
{
    # include <string>
    #include "insieme/core/ir.h"

    namespace insieme{
    namespace core{
    namespace parser3{
    namespace detail{

        class inspire_driver;
    } // detail
    } // parser3
    } // core
    } // insieme

    using namespace insieme::core;
}

// The parsing context.
%param { parser3::detail::inspire_driver& driver }
%param { symbol_type** start_token }
%locations
%initial-action
{
  // Initialize the initial location.
  @$.initialize(&driver.file);
};

// DEBUG
%define parse.trace
%define parse.error verbose
%code
{
    #include "insieme/core/parser3/detail/driver.h"
    #include "insieme/core/ir.h"

}

%define api.token.prefix {TOK_}
%token 
  END  0  "end of stream"
  ASSIGN  "="
  MINUS   "-"
  PLUS    "+"
  STAR    "*"
  SLASH   "/"

  LPAREN  "("
  RPAREN  ")"
  LCURBRACKET  "{"
  RCURBRACKET  "}"
  LBRACKET  "["
  RBRACKET  "]"
  DQUOTE  "\""
  QUOTE  "\'"

  LT        "<"     
  GT        ">"     
  LEQ       "<="    
  GEQ       ">="    
  EQ        "=="    
  NEQ       "!="    

  LNOT      "!"
                                         
  QMARK   "?"
  COLON   ":"
  NAMESPACE   "::"

  ARROW   "->"
  DARROW  "=>"

  SEMIC   ";"
  COMA    ","
  RANGE   ".."
  DOT     "."

  LET   "let"    
  AUTO   "auto"    
  IF     "if"      
  ELSE   "else"    
  FOR    "for"     
  WHILE  "while"   
  RETURN "return"  
  TRUE   "true"  
  FALSE  "false"  

  TYPE_ONLY
  EXPRESSION_ONLY
  STMT_ONLY
  FULL_PROGRAM
;

%token <std::string> STRING "stringlit"
%token <std::string> CHAR "charlit"
%token <std::string> IDENTIFIER "identifier"
%token <std::string> BOOL "bool"
%token <std::string> INT "int"
%token <std::string> UINT "uint"
%token <std::string> LONG "long"
%token <std::string> ULONG "ulong"
%token <std::string> FLOAT "float"
%token <std::string> DOUBLE "double"

%type  <std::string> "Number" 
%type  <std::string> "indentifier" 

%type  <ExpressionPtr> expression 
%type  <TypePtr> type 


%printer { yyoutput << $$; } <std::string>

%%

%start program;
program : TYPE_ONLY  type { driver.result = $2; }
        | EXPRESSION_ONLY expression { driver.result = $2; }
        ;

type :  {}
     ;


expression : 
            /* unary */
           | "*" expression {}
            /* logic */
           | expression "+" expression { }
           | expression "-" expression { }
           | expression "*" expression { }
           | expression "/" expression { }
            /* arithmetic/geometric */
           | expression "+" expression { }
           | expression "-" expression { }
           | expression "*" expression { }
           | expression "/" expression { }
            /* parent */
           | "(" expression ")" { $$ = $2;  }
            /* variables */
           | "identifier" { $$ = driver.findSymbol(@$, $1); }
            /* literals */
           | "bool"       { $$ = driver.builder.literal(driver.mgr.getLangBasic().getBool(), $1); }
           | "charlit"    { $$ = driver.builder.literal(driver.mgr.getLangBasic().getChar(), $1); }
           | "int"        { $$ = driver.builder.literal(driver.mgr.getLangBasic().getInt4(), $1); }
           | "uint"       { $$ = driver.builder.literal(driver.mgr.getLangBasic().getUInt4(), $1); }
           | "long"       { $$ = driver.builder.literal(driver.mgr.getLangBasic().getInt8(), $1); }
           | "ulong"      { $$ = driver.builder.literal(driver.mgr.getLangBasic().getUInt8(), $1); }
           | "float"      { $$ = driver.builder.literal(driver.mgr.getLangBasic().getReal4(), $1); }
           | "double"     { $$ = driver.builder.literal(driver.mgr.getLangBasic().getReal8(), $1); }
           | "stringlit"  { $$ = driver.builder.stringLit($1); }
           ;

%nonassoc "=";
%left "+" "-";
%left "*" "/";

%%
// code after the second %% is copyed verbatim at the end of the parser .cpp file

namespace insieme{
namespace core{
namespace parser3{
namespace detail{

    using namespace insieme::core;

    void inspire_parser::error (const location_type& l, const std::string& m) {
      driver.error (l, m);
    } 


} // detail
} // parser3
} // core
} // insieme


