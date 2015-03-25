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

  BAND      "&"
  BOR       "|"
  BXOR      "^"

  LAND      "&&"
  LOR       "||"
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

  INFINITE "#inf"
  LET   "let"    
  AUTO   "auto"    
  IF     "if"      
  ELSE   "else"    
  FOR    "for"     
  WHILE  "while"   
  RETURN "return"  
  TRUE   "true"  
  FALSE  "false"  
  STRUCT "struct"
  UNION  "union"

  TYPE_ONLY
  EXPRESSION_ONLY
  STMT_ONLY
  FULL_PROGRAM
;

    /* operators: they use string for convenience, instead of creating million rules */
%token <std::string> BIN_OP 


    /* Literals */
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
%token <std::string> PARAMVAR "paramvar"

%type  <std::string> "Number" 
%type  <std::string> "indentifier" 

%type <ExpressionPtr> expression cast_or_expr 
%type <std::pair<TypeList, IntParamList> > type_param_list
%type <TypePtr> type 
%type <TypeList> type_list parent_list
%type <bool> func_tok
%type <std::pair<TypeList, std::pair<TypeList, IntParamList> > > generic_type

%type <NamedTypePtr> member_field
%type <NamedTypeList> member_list
%type <std::pair<TypeList, NamedTypeList> > tag_type

%printer { yyoutput << $$; } <std::string>

%%

%start program;
program : TYPE_ONLY declarations type { driver.result = $3; }
        | EXPRESSION_ONLY declarations expression { driver.result = $3; }
        ;

declarations : /* empty */ { } 
             | "let" "indentifier" "=" type ";" {  driver.scopes.add_symb($2, $4); }
             | "let" "identifier" "=" type ";" declarations { driver.scopes.add_symb($2, $4); }

/* ~~~~~~~~~~~~~~~~~~~  TYPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

type_param_list : "#inf" { $$.second.push_back(driver.builder.infiniteIntTypeParam()); }
                | "paramvar" { $$.second.push_back(driver.builder.variableIntTypeParam($1[0])); }
                | "int" { $$.second.push_back(driver.builder.concreteIntTypeParam(utils::numeric_cast<uint32_t>($1))); }
                | type  { $$.first.push_back($1); }
                | "#inf" "," type_param_list {$3.second.push_back(driver.builder.infiniteIntTypeParam()); std::swap($$, $3); }
                | "paramvar" "," type_param_list {$3.second.push_back(driver.builder.infiniteIntTypeParam()); std::swap($$, $3); }
                | "int" "," type_param_list {$3.second.push_back(driver.builder.variableIntTypeParam($1[0])); std::swap($$, $3); }
                | type "," type_param_list {$3.first.push_back($1); std::swap($$, $3); }
                ;

type_list : type { $$.push_back($1); }
          | type "," type_list { $3.push_back($1); std::swap($$, $3); }

parent_list : type { $$.push_back($1); }
            | type ":" parent_list { $3.push_back($1); std::swap($$, $3); }

/* is a closure? */
func_tok : "->" { $$ = false; }
         | "=>" { $$ = true; }
         ;

generic_type : "<" type_param_list ">"  { std::swap($$.second, $2); } 
                        /* gen type with parents */
             | "::" parent_list "<" type_param_list ">" { std::swap($$.first, $2); std::swap($$.second, $4); }
             ;

member_field : type "identifier" { $$ = driver.builder.namedType($2, $1); }
             ;

member_list :  /* empty */  { }
            | member_field { $$.push_back($1); }
            | member_field ";" member_list { $3.push_back($1); std::swap($$, $3); }
            ;

op_semi_colon : /* empty */ | ";"

tag_type : ":" parent_list "{" member_list op_semi_colon "}" { std::swap($$.first, $2); std::swap($$.second, $4); }
         | "{" member_list op_semi_colon "}"  { std::swap($$.second, $2); }
         ;

type : "identifier" { $$ = driver.findType(@1, $1); }  // defined?
        /* gen type */
     | "identifier" generic_type { $$ = driver.genGenericType(@$, $1, $2.first, $2.second.first, $2.second.second);} 
        /* func / closure type */
     | "(" type_list ")" func_tok type 
                { $$ = driver.genFuncTypeType(@$, $2, $5, $4); }
     | "struct" "identifier" tag_type 
                { $$ = driver.builder.structType(driver.builder.stringValue($2), driver.builder.parents($3.first), $3.second); }
     | "struct" tag_type              
                { $$ = driver.builder.structType(driver.builder.stringValue(""), driver.builder.parents($2.first), $2.second); }
     | "union" tag_type   { $$ = driver.builder.unionType($2.second); }
     ;

/* ~~~~~~~~~~~~~~~~~~~  EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
 
expression : "identifier" { $$ = driver.findSymbol(@$, $1); }
            /* unary */
           | "*" expression {} %prec UDEREF
           | "-" expression {} %prec UMINUS
           | expression "=" expression {  $$ = driver.genBinaryExpression(@$, "=", $1, $3); }
            /* bitwise / logic / arithmetic / geometric */
           | expression "&" expression { $$ = driver.genBinaryExpression(@$, "&", $1, $3);  }
           | expression "|" expression { $$ = driver.genBinaryExpression(@$, "|", $1, $3);  }
           | expression "^" expression { $$ = driver.genBinaryExpression(@$, "^", $1, $3);  }
           | expression "&&" expression {$$ = driver.genBinaryExpression(@$, "&&", $1, $3);  }
           | expression "||" expression {$$ = driver.genBinaryExpression(@$, "||", $1, $3);  }
           | expression "+" expression { $$ = driver.genBinaryExpression(@$, "+", $1, $3);  }
           | expression "-" expression { $$ = driver.genBinaryExpression(@$, "-", $1, $3);  }
           | expression "*" expression { $$ = driver.genBinaryExpression(@$, "*", $1, $3);  }
           | expression "/" expression { $$ = driver.genBinaryExpression(@$, "/", $1, $3);  }
            /* cast */
           | "(" cast_or_expr { $$ = $2; }
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

cast_or_expr : type ")" expression     { $$ = $3; }
             | expression ")"          { $$ = $1; }
             ;


%nonassoc "=" ;
%nonassoc "::" ;
%left "(";
%left "+" "-";
%left "*" "/";
%left "&&" "||";
%left "&" "|" "^";
%nonassoc UMINUS;
%nonassoc UDEREF;
%nonassoc BOOL_OP;

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


