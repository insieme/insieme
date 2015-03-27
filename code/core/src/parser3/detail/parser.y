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
    /**
     * this code goes in the header
     */

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

    namespace {
        struct For_decl{
            VariablePtr it;
            ExpressionPtr low;
            ExpressionPtr up;
            ExpressionPtr step;
        };

        struct t_params_names{
            TypeList types;
            std::vector<std::string> names;
            VariableList params;
        };
    }
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
//%define parse.trace
%define parse.error verbose
%code
{
    /**
     * this code goes in the cpp file
     */

    #include "insieme/core/parser3/detail/driver.h"
    #include "insieme/core/ir.h"

    #include "insieme/core/annotations/naming.h"

    #define INSPIRE_GUARD(l,n) \
            if(!n) { driver.error(l, "unrecoverable error"); YYABORT; }
    #define INSPIRE_NOT_IMPLEMENTED(l) \
            { driver.error(l, "not supported yet "); YYABORT; }
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
  ADDRESS "$"

  
  PARENT        ".as("
  CAST          "CAST("
  INFINITE      "#inf"
  LET           "let"    
  AUTO          "auto"    
  LAMBDA        "lambda"    

  IF            "if"      
  ELSE          "else"    
  FOR           "for"     
  WHILE         "while"   
  DECL          "decl"
  TRY           "try"   
  CATCH         "catch"   
  RETURN        "return"  
  DEFAULT       "default"  
  CASE          "case"  
  SWITCH        "switch"  
  
  VAR           "var"
  NEW           "new"
  LOC           "loc"
  DELETE        "delete"
  UNDEFINED     "undefined"

  TRUE          "true"  
  FALSE         "false"  
  STRUCT        "struct"
  UNION         "union"

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
%token <std::string> TYPE_VAR   "type_var"
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

%type <ProgramPtr> program

%type <StatementPtr> statement decl_stmt
%type <StatementList> compound_stmt statement_list
%type <For_decl> for_decl
%type <VariablePtr> var_decl
%type <VariableList> variable_list 
%type <SwitchCasePtr> switch_case
%type <std::vector<SwitchCasePtr> > switch_case_list
%type <std::vector<CatchClausePtr> > catch_clause_list

%type <ExpressionPtr> expression  lambda_expression markable_expression lambda_expression_aux
%type <ExpressionList> expression_list

%type <std::pair<TypeList, IntParamList> > type_param_list
%type <TypePtr> type  type_no_func
%type <TypeList> type_list parent_list
%type <bool> func_tok

%type <NamedTypePtr> member_field
%type <NamedTypeList> member_list tag_type

%type <NodePtr> func_or_type
%type <t_params_names> params_names params_names_collector

%printer { yyoutput << $$; } <std::string>



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
%%

%start program;
program : TYPE_ONLY declarations type { driver.result = $3; }
        | STMT_ONLY declarations statement { driver.result = $3; }
        | EXPRESSION_ONLY declarations expression { driver.result = $3; }
        | FULL_PROGRAM declarations program { driver.result = $3; }
        ;

/* poison: we do not know if we deal with a function or type until we find the first variable name 
        accumulate everithing and decide later
*/
params_names_collector : type "identifier"      { $$.types.insert($$.types.begin(), $1);  $$.names.insert($$.names.begin(), $2);} 
             | type                             { $$.types.insert($$.types.begin(), $1); }
             | type "," params_names_collector  { $3.types.insert($3.types.begin(), $1); std::swap($$.types, $3.types); }
             | type "identifier" "," params_names_collector { 
                                                  $4.types.insert($4.types.begin(), $1); 
                                                  $4.names.insert($4.names.begin(), $2); 
                                                  std::swap($$.types, $4.types);std::swap($$.names, $4.names);
                                                }
             ;

params_names : params_names_collector {

                    if($1.names.size() != $1.types.size()) {
                        // typeList
                        if(!$1.names.empty()) { driver.error(@1, "is this a list of types or variables?"); YYABORT;  }
                        std::swap($$.types, $1.types);
                    }
                    else{
                        driver.scopes.open_scope("params_names");
                        assert($$.params.empty());
                        for (unsigned i=0; i < $1.types.size(); ++i){
                            auto var = driver.builder.variable($1.types[i]);
                            annotations::attachName(var, $1.names[i]);
                            driver.scopes.add_symb($1.names[i], var);

                            $$.params.push_back(var);
                        }
                        std::swap ($$.types, $1.types);
                    }
                        
                } 
             ;


func_or_type : "(" ")" "->" type ";" {
                       $$ = driver.genFuncType(@$, TypeList(), $4); 
                }
             | "(" ")" "->" type "{" compound_stmt {
                    driver.scopes.close_scope("func_or _type");
                    FunctionTypePtr funcType = driver.builder.functionType(TypeList(), $4); 
                    CompoundStmtPtr body = driver.builder.compoundStmt($6);
	    			$$ = driver.builder.lambdaExpr(funcType, VariableList(), body);
                }
             | "(" params_names ")" "->" type ";" {
                    $$ = driver.genFuncType(@$, $2.types , $5); 
                }
             | "(" params_names ")" "->" type "{" compound_stmt {
                    driver.scopes.close_scope("func_or _type2");
                    FunctionTypePtr funcType = driver.builder.functionType($2.types, $5); 
                    CompoundStmtPtr body = driver.builder.compoundStmt($7);
	    			$$ = driver.builder.lambdaExpr(funcType, $2.params, body);
                }
             | type_no_func { $$ = $1; }
             ;

let_chain : "identifier" "=" func_or_type ";" {  driver.scopes.add_symb($1, $3); }
          | "identifier" "=" func_or_type ";" "let" let_chain { driver.scopes.add_symb($1, $3);}
          ;

declarations : /* empty */ { } 
             | "let" let_chain {  }
             ;

program : type "identifier" "(" variable_list ")" "{" compound_stmt  {
                            driver.scopes.close_scope("program");
                            TypeList paramTys;
                            for (const auto& var : $4) paramTys.push_back(var.getType());
                            FunctionTypePtr funcType = driver.builder.functionType(paramTys, $1); 
						    ExpressionPtr main = driver.builder.lambdaExpr(funcType, $4, driver.builder.compoundStmt($7));
						    $$ = driver.builder.createProgram(toVector(main));
                        }
        | type "identifier" "(" ")" "{" compound_stmt {
                            driver.scopes.close_scope("program 2");
                            FunctionTypePtr funcType = driver.builder.functionType(TypeList(), $1); 
						    ExpressionPtr main = driver.builder.lambdaExpr(funcType, VariableList(), driver.builder.compoundStmt($6));
						    $$ = driver.builder.createProgram(toVector(main));
                        }
        ;

variable_list : var_decl {  $$.push_back($1); }
              | var_decl "," variable_list { $3.insert($3.begin(), $1); std::swap($$, $3); }
              ;

/* ~~~~~~~~~~~~~~~~~~~  TYPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

type_param_list : "#inf" { $$.second.insert($$.second.begin(), driver.builder.infiniteIntTypeParam()); }
                | "paramvar" { $$.second.insert($$.second.begin(), driver.builder.variableIntTypeParam($1[0])); }
                | "int" { $$.second.insert($$.second.begin(), driver.builder.concreteIntTypeParam(utils::numeric_cast<uint32_t>($1))); }
                | type  { $$.first.push_back($1); }

                | "#inf" "," type_param_list {$3.second.insert($3.second.begin(), driver.builder.infiniteIntTypeParam()); std::swap($$, $3); }
                | "paramvar" "," type_param_list {$3.second.insert($3.second.begin(), driver.builder.infiniteIntTypeParam()); std::swap($$, $3); }
                | "int" "," type_param_list {$3.second.insert($3.second.begin(), driver.builder.variableIntTypeParam($1[0])); std::swap($$, $3); }
                | type "," type_param_list {$3.first.insert($3.first.begin(), $1); std::swap($$, $3); }
                ;

type_list : type { $$.push_back($1); }
          | type "," type_list { $3.push_back($1); std::swap($$, $3); }

parent_list : type { $$.push_back($1); }
            | type ":" parent_list { $3.push_back($1); std::swap($$, $3); }
            ;

/* is a closure? */
func_tok : "->" { $$ = false; }
         | "=>" { $$ = true; }
         ;

member_field : type "identifier" { $$ = driver.builder.namedType($2, $1); }
             ;

member_list :  /* empty */  { }
            | member_field { $$.push_back($1); }
            | member_field ";" member_list { $3.insert($3.begin(),$1); std::swap($$, $3); }
            ;

//op_semi_colon : /* empty */ | ";"  // this beauty costs one conflict in grammar

tag_type : "{" member_list "}"  { std::swap($$, $2); }
         ;

type_no_func : "identifier" "<" type_param_list ">" { $$ = driver.genGenericType(@$, $1, $3.first, $3.second);  }
             | "identifier" "<" ">" {                 $$ = driver.genGenericType(@$, $1, TypeList(), IntParamList());  }
             | "identifier" {                         
                                                      $$ = driver.findType(@1, $1);
                                                      if(!$$) { driver.error(@$, format("undefined type %s", $1)); YYABORT; } 
                            }
                /* func / closure type */
             | "struct" "identifier" tag_type 
                        { $$ = driver.builder.structType(driver.builder.stringValue($2), $3); }
             | "struct" "identifier" "::" parent_list tag_type 
                        { $$ = driver.builder.structType(driver.builder.stringValue($2), driver.builder.parents($4), $5); }
             | "struct" tag_type              
                        { $$ = driver.builder.structType(driver.builder.stringValue(""), $2); }
             | "union" tag_type   { $$ = driver.builder.unionType($2); }
             | "type_var" { $$ = driver.builder.typeVariable($1);  }
             ;

type : type_no_func { $$ = $1; }
     | "(" type_list ")" func_tok type { $$ = driver.genFuncType(@$, $2, $5, $4); }
     ;

/* ~~~~~~~~~~~~~~~~~~~  STATEMENTS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

statement : ";" { $$ = driver.builder.getNoOp(); } 
          | expression ";" { $$ = $1; }

                /* compound statement */ 
          | "{" "}" { $$ = driver.builder.compoundStmt(StatementList()); }
          | "{" compound_stmt { driver.scopes.close_scope("compound end"); $$ = driver.builder.compoundStmt($2); }

                /* variable declarations */
          | "decl" decl_stmt ";" { $$ = $2;  }

                /* if */
          | "if" "(" expression ")" statement {  $$ = driver.builder.ifStmt($3, $5); }
          | "if" "(" expression ")" statement "else" statement { $$ = driver.builder.ifStmt($3, $5, $7); }

                /* loops */
          | "while" "(" expression ")" statement { $$ = driver.builder.whileStmt($3, $5); }
          | "for" for_decl statement {
                $$ = driver.builder.forStmt($2.it, $2.low, $2.up, $2.step, $3);
                driver.scopes.close_scope("for end");
            }
                /* switch */
          | "switch" "(" expression ")" "{" switch_case_list "}" {
			    $$ = driver.builder.switchStmt($3, $6, driver.builder.getNoOp());
            }
          | "switch" "(" expression ")" "{" switch_case_list "default" ":" statement "}" {
			    $$ = driver.builder.switchStmt($3, $6, $9);
            }
                /* exceptions */
          | "try" statement "catch" catch_clause_list {
                if(!$2.isa<CompoundStmtPtr>()) { driver.error(@2, "try body must be a compound"); YYABORT; }
			    $$ = driver.builder.tryCatchStmt($2.as<CompoundStmtPtr>(), $4);
            }   
          | "return" expression ";" { $$ = driver.builder.returnStmt($2); }
          ;

catch_clause_list : 
                   "catch" "(" ")" statement { 
                            if(!$4.isa<CompoundStmtPtr>()) { driver.error(@4, "catch body must be a compound"); YYABORT; }
                            $$.push_back(driver.builder.catchClause(VariablePtr(), $4.as<CompoundStmtPtr>())); 
                    } 
                  | "(" var_decl ")" statement {  
                            if(!$4.isa<CompoundStmtPtr>()) { driver.error(@4, "catch body must be a compound"); YYABORT; }
                            $$.push_back(driver.builder.catchClause($2, $4.as<CompoundStmtPtr>())); 
                    }
                  | "(" var_decl ")" statement "catch" catch_clause_list{ 
                            if(!$4.isa<CompoundStmtPtr>()) { driver.error(@4, "catch body must be a compound"); YYABORT; }
                            $6.insert($6.begin(), driver.builder.catchClause($2, $4.as<CompoundStmtPtr>()));
                            std::swap($$, $6);
                    }
                  ;

decl_stmt : var_decl "=" expression {
                $$ = driver.builder.declarationStmt($1, $3);
            }
          | "auto" "identifier" "=" expression {
		        auto var = driver.builder.variable($4.getType());
				annotations::attachName(var, $2);
                driver.scopes.add_symb($2, var);
                $$ = driver.builder.declarationStmt(var, $4);
            }
          ;

var_decl : type "identifier" { 
		        $$ = driver.builder.variable($1);
				annotations::attachName( $$, $2);
                driver.scopes.add_symb($2, $$);
            };


for_decl : "(" type "identifier" expression ".." expression ")"  {
                driver.scopes.open_scope("forDecl");
		        $$.it = driver.builder.variable($2);
				annotations::attachName( $$.it, $3);
                driver.scopes.add_symb("$3", $$.it);
                $$.low = $4;
                $$.up = $6;
                $$.step = driver.builder.literal($2, "1");
           }
         | "(" type "identifier" expression ".." expression ":" expression ")" {
                driver.scopes.open_scope("forDecl_step");
		        $$.it = driver.builder.variable($2);
				annotations::attachName( $$.it, $3);
                driver.scopes.add_symb($3, $$.it);
                $$.low = $4;
                $$.up = $6;
                $$.step = $8;
           }
         ;

switch_case_list : switch_case { $$.push_back($1); }
                 | switch_case switch_case_list { $2.insert($2.begin(), $1); std::swap($$, $2); }
                 ;


switch_case :  "case" expression ":" statement { 
                  if(!$2.isa<LiteralPtr>()) { driver.error(@2, "case value must be a literal"); YYABORT; }
                  $$ = driver.builder.switchCase($2.as<LiteralPtr>(), $4); 
               }
            ;

compound_stmt : declarations statement_list { $$ = $2; }

statement_list : statement "}" { driver.scopes.open_scope("stmt list"); $$.push_back($1); }
               | statement statement_list { $2.insert($2.begin(),$1); std::swap($$, $2); }



/* ~~~~~~~~~~~~~~~~~~~  EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

expression : markable_expression  { INSPIRE_GUARD(@1, $1); $$ = $1; } 
           | "$" markable_expression "$" {  INSPIRE_GUARD(@2, $2); $$ = $2; } 

expression_list : expression { $$.push_back($1); }
                | expression "," expression_list  { $3.insert($3.begin(), $1); std::swap($$, $3); }
 
markable_expression : "identifier" { $$ = driver.findSymbol(@$, $1); }
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
            /* call expr */
           | expression "(" ")" {  $$ = driver.builder.callExpr($1); }
           | expression "(" expression_list ")" { $$ = driver.builder.callExpr($1, $3); }
            /* parenthesis */
           | "(" expression ")"  { $$ = $2; }
            /* lambda or closure expression: callable expression */
           |  "lambda" lambda_expression  { $$ = $2; }
            /* cast */ 
           | "CAST(" type ")" expression  { $$ = driver.builder.castExpr($2, $4); }
           | expression ".as(" type ")" { $$  = driver.builder.refParent($1, $3); }
            /* ref mamagement */
           | "undefined" "(" type ")"    { $$ =  driver.builder.undefined( $3 ); }
           | "var" "(" expression ")"    { $$ =  driver.builder.refVar( $3 ); }
           | "new" "(" expression ")"    { $$ =  driver.builder.refNew( $3 ); }
           | "loc" "(" expression ")"    { $$ =  driver.builder.refLoc( $3 ); }
           | "delete" "(" expression ")" { $$ =  driver.builder.refDelete( $3 ); }
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


lambda_expression_aux : "(" ")" func_tok expression {    
                               driver.scopes.close_scope("labmda1");
                                auto funcType = driver.genFuncType(@$, TypeList(), $4.getType(), $3); 
                                CompoundStmtPtr body = driver.builder.compoundStmt(driver.builder.returnStmt($4));
                                $$ = driver.builder.lambdaExpr(funcType.as<FunctionTypePtr>(), VariableList(), body);
                        }
                      | "(" ")" func_tok "{" compound_stmt {
                                driver.scopes.close_scope("labmda2");
                                INSPIRE_NOT_IMPLEMENTED(@$);
                                // this needs a little of love: get the type from the returns in the compound
                        }
                      | "(" variable_list ")" func_tok expression {
                                driver.scopes.close_scope("labmda3");
                                TypeList paramTys;
                                for (const auto& var : $2) paramTys.push_back(var.getType());
                                auto funcType = driver.genFuncType(@$, paramTys, $5.getType(), $4); 
                                CompoundStmtPtr body = driver.builder.compoundStmt(driver.builder.returnStmt($5));
                                $$ = driver.builder.lambdaExpr(funcType.as<FunctionTypePtr>(), $2, body);
                        }
                      | "(" variable_list ")" func_tok  "{" compound_stmt  {
                                driver.scopes.close_scope("labmda4");
                                INSPIRE_NOT_IMPLEMENTED(@$);
                                // this needs a little of love: get the type from the returns in the compound
                        }
                      ;

open_scope : /* empty */  { driver.scopes.open_scope("for_lambda_expr");} 

lambda_expression : open_scope lambda_expression_aux { $$ = $2; }

%nonassoc "=" ;
%nonassoc "::" ;
%nonassoc ")";
%nonassoc "else";
%right "catch";
%left "(";
%left ".cast(";
%left ".as(";
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


