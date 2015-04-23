%skeleton "lalr1.cc" /* -*- C++ -*- */
/* glr.cc does not support type variants, 
 * there is work in process to suport it, 
 * lets wait until 2016 and then we can use the $#%& ambigous grammar again
 */
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
        class inspire_scanner;

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

        struct Gen_type{
            ParentList parents;
            TypeList typeParams;
            IntParamList intParams;
        };
    }
}

// The parsing context.
%param { parser3::detail::inspire_driver& driver }
%param { parser3::detail::inspire_scanner& scanner }
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
    #include "insieme/core/parser3/detail/scanner.h"
    #include "insieme/core/ir.h"

    #include "insieme/core/annotations/naming.h"
    #include "insieme/core/analysis/ir_utils.h"
    #include "insieme/core/encoder/lists.h"

    #define INSPIRE_MSG(l, n, msg) \
            if(!n) { driver.error(l, msg); YYABORT; }

    #define INSPIRE_TYPE(l, n, t, msg) \
            if(!n.isa<t>()) { driver.error(l, msg); YYABORT; } 

    #define INSPIRE_GUARD(l, n) \
            if(!n) { driver.error(l, "unrecoverable error"); YYABORT; }

    #define INSPIRE_NOT_IMPLEMENTED(l) \
            { driver.error(l, "not supported yet "); YYABORT; }

    #define RULE if(driver.inhibit_building()) { break; }
}

%define api.token.prefix {TOK_}
%token 
  END  0  "end of stream"
  ASSIGN  "="
  MINUS   "-"
  PLUS    "+"
  STAR    "*"
  SLASH   "/"
  PERCENT "%"

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
  NAMESPACE "::"
  FUNNY_BOY "~"

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
  USING         "using"    
  AUTO          "auto"    
  LAMBDA        "lambda"    
  CTOR          "ctor"    
  METHOD        "method"    
  EXPR          "expr"    

  IF            "if"      
  ELSE          "else"    
  FOR           "for"     
  WHILE         "while"   
  DECL          "decl"
  TRY           "try"   
  THROW         "throw"   
  CATCH         "catch"   
  RETURN        "return"  
  DEFAULT       "default"  
  CASE          "case"  
  SWITCH        "switch"  
  CONTINUE      "continue"  
  BREAK         "break"  
  
  VAR           "var"
  NEW           "new"
  LOC           "loc"
  DELETE        "delete"
  UNDEFINED     "undefined"

  TYPE_LITERAL  "type("
  LITERAL       "lit("
  PARAM         "param("

  TRUE          "true"  
  FALSE         "false"  
  STRUCT        "struct"
  UNION         "union"

  SPAWN         "spawn"
  SYNC          "sync"
  SYNCALL       "syncAll"

  JOB           "job"
  TASK          "task"

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
%token <std::string> LONGLONG "longlong"
%token <std::string> ULONGLONG "ulonglong"
%token <std::string> FLOAT "float"
%token <std::string> DOUBLE "double"
%token <std::string> PARAMVAR "paramvar"

%type  <std::string> "Number" 
%type  <std::string> "indentifier" 

%type <NodePtr> start_rule
%type <ProgramPtr> program

%type <StatementPtr> statement decl_stmt compound_stmt markable_compound_stmt statement_aux 
%type <StatementList>  statement_list
%type <For_decl> for_decl
%type <VariablePtr> var_decl
%type <VariableList> variable_list  variable_list_aux
%type <SwitchCasePtr> switch_case
%type <std::vector<SwitchCasePtr> > switch_case_list
%type <std::vector<CatchClausePtr> > catch_clause_list

%type <ExpressionPtr> expression  lambda_expression markable_expression lambda_expression_aux
%type <ExpressionList> expression_list

%type <Gen_type> type_param_list gen_type
%type <TypePtr> type tuple_or_function  struct_type named_type just_name 
%type <TypeList> type_list  type_list_aux
%type <ParentList> parent_list
%type <FunctionKind> func_tok

%type <NamedTypeList> member_list tag_def union_type 

%type <std::string> namespaced_type

%type <std::vector<std::string>> string_list

%printer { yyoutput << $$; } <std::string>

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
%%

/* ~~~~~~~~~~~~ Super rule, the lexer is prepared to return a spetial token and jump to the right rule ~~~~~~~~~~~~ */

%start start_rule;
start_rule : TYPE_ONLY declarations type             { RULE if(!driver.where_errors())  driver.result = $3; }
           | STMT_ONLY statement                     { RULE if(!driver.where_errors())  driver.result = $2; }
           | EXPRESSION_ONLY declarations expression { RULE if(!driver.where_errors())  driver.result = $3; }
           | FULL_PROGRAM  declarations { RULE driver.open_scope(@$, "program"); }  program { RULE if(!driver.where_errors()) driver.result = $4; }
           ;

/* ~~~~~~~~~~~~~~~~~~~  LET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

let_defs : "lambda" lambda_expression ";" 
         | "lambda" lambda_expression "," let_defs  
         | "expr" expression ";" { RULE driver.add_let_expression(@1, $2); }
         | "expr" expression "," let_defs { RULE driver.add_let_expression(@1, $2); }
         | type ";" { RULE driver.add_let_type(@1, $1); }
         | type "," { RULE driver.add_let_type(@1, $1); } let_defs                       
         ;

let_decl : "identifier" { if(driver.let_count==1) driver.add_let_name(@1,$1); } "=" let_defs  { }
         | "identifier" { if(driver.let_count==1) driver.add_let_name(@1,$1); } "," let_decl  { }
         ;

let_chain : let_decl { driver.close_let_statement(@$); } 
          | let_decl { driver.close_let_statement(@$); } "let" { driver.let_count++; } let_chain { }
          ;

string_list : "stringlit"  { RULE $$.push_back($1); }
            | "stringlit" "," string_list { $3.insert($3.begin(), $1); std::swap($$, $3); }
            ;

declarations : /* empty */ { } 
             | "let" { driver.let_count++; } let_chain { }
             | "using" string_list ";" declarations { RULE driver.using_scope_handle(@$, $2); }
             ;

/* ~~~~~~~~~~~~~~~~~~~  PROGRAM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

program : type "identifier" "(" variable_list markable_compound_stmt { RULE
                             INSPIRE_GUARD(@1, $1); 
                             driver.close_scope(@$, "program");
                             TypeList paramTys;
                             for (const auto& var : $4) paramTys.push_back(var.getType());
                             FunctionTypePtr funcType = driver.builder.functionType(paramTys, $1); 
						     ExpressionPtr main = driver.builder.lambdaExpr(funcType, $4, $5);
						     $$ = driver.builder.createProgram(toVector(main));
                        }
        ;


variable_list : ")" { }
              | var_decl ")" { RULE $$.push_back($1); }
              | var_decl "," variable_list_aux ")" {RULE  $3.insert($3.begin(), $1); std::swap($$, $3); } 
              ;

variable_list_aux : var_decl {  RULE $$.push_back($1); }
                  | var_decl "," variable_list_aux { RULE $3.insert($3.begin(), $1); std::swap($$, $3); }
                  ;

/* ~~~~~~~~~~~~~~~~~~~  TYPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

type_param_list : "#inf"     { RULE $$.intParams.insert($$.intParams.begin(), driver.builder.infiniteIntTypeParam()); }
                | "paramvar" { RULE $$.intParams.insert($$.intParams.begin(), driver.gen_type_param_var(@1,$1)); }
                | "int"      { RULE $$.intParams.insert($$.intParams.begin(), 
                                    driver.builder.concreteIntTypeParam(utils::numeric_cast<uint32_t>($1))); }
                | type       { RULE $$.typeParams.insert($$.typeParams.begin(), $1); }

                | "#inf" "," type_param_list { RULE
                        $3.intParams.insert($3.intParams.begin(), driver.builder.infiniteIntTypeParam()); 
                        std::swap($$.typeParams, $3.typeParams);
                        std::swap($$.intParams, $3.intParams);
                    }
                | "paramvar" "," type_param_list {RULE
                        $3.intParams.insert($3.intParams.begin(), driver.gen_type_param_var(@1,$1)); 
                        std::swap($$.typeParams, $3.typeParams);
                        std::swap($$.intParams, $3.intParams);
                    }
                | "int" "," type_param_list { RULE
                        $3.intParams.insert($3.intParams.begin(), driver.builder.variableIntTypeParam($1[1])); 
                        std::swap($$.typeParams, $3.typeParams);
                        std::swap($$.intParams, $3.intParams);
                    }
                | type "," type_param_list { RULE
                        $3.typeParams.insert($3.typeParams.begin(), $1); 
                        std::swap($$.typeParams, $3.typeParams);
                        std::swap($$.intParams, $3.intParams);
                    }
                ;

type_list_aux : type ")"           { RULE $$.insert($$.begin(), $1); }
              | type "," type_list_aux { RULE $3.insert($3.begin(), $1); std::swap($$, $3); }
              ;

type_list : ")" { }
          | type_list_aux { RULE std::swap($$, $1); }
          ;

parent_list : just_name                 { RULE  $$.insert($$.begin(), driver.builder.parent(false, $1));  }
            | just_name "," parent_list { RULE  $3.insert($3.begin(), driver.builder.parent(false, $1)); std::swap($$, $3); }
            | "virtual" just_name                 { RULE  $$.insert($$.begin(), driver.builder.parent(true, $2)); }
            | "virtual" just_name "," parent_list { RULE  $4.insert($4.begin(), driver.builder.parent(true, $2)); std::swap($$, $4); }
            ;

func_tok : "->" { RULE $$ = FK_PLAIN; }
         | "=>" { RULE $$ = FK_CLOSURE; }
         ;

member_list : "}" {}
            | type "identifier" "}" { RULE $$.insert($$.begin(), driver.builder.namedType($2, $1)); }
            | type "identifier" ";" member_list { RULE $4.insert($4.begin(), driver.builder.namedType($2, $1)); std::swap($$, $4); }
            ;

tag_def : "{" member_list   { std::swap($$, $2); }
        ;

                     /* tuple */
tuple_or_function : type_list  { RULE 
                            $$ = driver.builder.tupleType($1); 
                        }
                     /* function/closure type */
                  | type_list func_tok type { RULE
                            $$ = driver.genFuncType(@$, $1, $3, $2); 
                            INSPIRE_GUARD(@$, $$);
                        }
                  ;

gen_type : type_param_list ">" { RULE std::swap($$.typeParams, $1.typeParams); std::swap($$.intParams, $1.intParams); }
         | ">"                 { }
         ;

union_type : tag_def   { RULE std::swap($$, $1); }
           ;

struct_type : tag_def              { RULE $$ = driver.builder.structType(driver.builder.stringValue(""), $1); }
            | "identifier" tag_def { RULE $$ = driver.builder.structType(driver.builder.stringValue($1), $2); }
            | "identifier" ":" parent_list tag_def { RULE 
                        $$ = driver.builder.structType(driver.builder.stringValue($1), $3, $4); 
                    }
            | ":" parent_list tag_def { RULE 
                        $$ = driver.builder.structType($2, $3); 
                    }
            ;


type : "struct" struct_type { RULE $$ = $2; }
     | "union"  union_type  { RULE $$ = driver.builder.unionType($2); }
     | "ctor" just_name "::" "(" type_list { RULE
                            TypePtr classType = driver.builder.refType($2);
                            TypePtr retType = classType;
                            $5.insert($5.begin(), classType);
                            $$ = driver.builder.functionType($5, retType, FK_CONSTRUCTOR);
                       }
     | "method" just_name "::" "(" type_list "->" type { RULE
                            TypePtr classType = driver.builder.refType($2);
                            TypePtr retType = $7;
                            $5.insert($5.begin(), classType);
                            $$ = driver.builder.functionType($5, retType, FK_MEMBER_FUNCTION);
                       }
     | "~" just_name "::" "(" ")" { RULE
                            TypePtr classType = driver.builder.refType($2);
                            $$ = driver.builder.functionType(toVector(classType), classType, FK_DESTRUCTOR);
                       }
     | "(" tuple_or_function { RULE $$ = $2; }
     | named_type            { RULE $$ = $1; }  
            /* job is a keyword for job expressions but it could also apear as type, */
     | "job" { RULE $$ = driver.genGenericType(@$, "job", ParentList(), TypeList(), IntParamList()); }
     ;

just_name : "identifier" { RULE  $$ = driver.findType(@1, $1); 
                           INSPIRE_MSG(@$,$$, format("Type %s was not defined", $1));
                         }
          | "type_var"   { RULE  $$ = driver.builder.typeVariable($1); }
          ;

namespaced_type : "identifier"  { RULE std::swap($$, $1); }
                | "identifier" "::" namespaced_type  { RULE $$.append($1); $$.append("::"); $$.append($3); }
                ;
                
named_type : "identifier" "<" gen_type { RULE $$ = driver.genGenericType(@$, $1, $3.parents, $3.typeParams, $3.intParams); }
           | "identifier" { RULE         
                                  $$ = driver.findType(@1, $1);
                                  if(!$$) $$ = driver.genGenericType(@$, $1, ParentList(), TypeList(), IntParamList()); 
                                  if(!$$) { driver.error(@$, format("undefined type %s", $1)); YYABORT; } 
                          }
           | "identifier" ":" parent_list "<" gen_type { RULE
                            $$ = driver.genGenericType(@$, $1, $3, $5.typeParams, $5.intParams);
                        }
           | "type_var"   { RULE $$ = driver.builder.typeVariable($1); }
           | "identifier" "::" namespaced_type { RULE
                                    $1.append("::"); $1.append($3); 
                                    $$ = driver.genGenericType(@$, $1, ParentList(), TypeList(), IntParamList()); 
                                }
           | "identifier" "::" namespaced_type "<" gen_type { RULE
                                    $1.append("::"); $1.append($3); 
                                    $$ = driver.genGenericType(@$, $1, ParentList(), $5.typeParams, $5.intParams); 
                                }
           ;

/* ~~~~~~~~~~~~~~~~~~~  STATEMENTS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

statement : statement_aux         { RULE INSPIRE_GUARD(@1, $1); $$ = $1; }
          | "$" statement_aux "$" { RULE INSPIRE_GUARD(@2, $2); $$ = driver.mark_address(@2, $2); } 
          ;

statement_aux  : ";" { RULE $$ = driver.builder.getNoOp(); } 
               | expression ";" { RULE $$ = $1; }
               | "let" { driver.let_count++; } let_decl { driver.close_let_statement(@$); RULE $$ =  driver.builder.getNoOp(); }
               | "using" string_list ";" { RULE driver.using_scope_handle(@$, $2); $$ = driver.builder.getNoOp(); }
                     /* compound statement */ 
               | { RULE driver.open_scope(@$, "compound"); } compound_stmt  {RULE  
                                         driver.close_scope(@$, "compound_end"); $$ =$2; 
                      }

                     /* variable declarations */
               | "decl" decl_stmt { RULE $$ = $2;  }

                     /* if */
               | "if" "(" expression ")" statement {  RULE $$ = driver.builder.ifStmt($3, $5); }
               | "if" "(" expression ")" statement "else" statement { RULE $$ = driver.builder.ifStmt($3, $5, $7); }

                     /* loops */
               | "while" "(" expression ")" statement { RULE $$ = driver.builder.whileStmt($3, $5); }
               | "for" { RULE driver.open_scope(@$, "forDecl"); } for_decl statement { RULE
                     $$ = driver.builder.forStmt($3.it, $3.low, $3.up, $3.step, $4);
                     driver.close_scope(@$, "for end");
                 }
                     /* switch */
               | "switch" "(" expression ")" "{" switch_case_list "}" { RULE
		             $$ = driver.builder.switchStmt($3, $6, driver.builder.getNoOp());
                 }
               | "switch" "(" expression ")" "{" switch_case_list "default" ":" statement "}" { RULE
		             $$ = driver.builder.switchStmt($3, $6, $9);
                 }
                     /* exceptions */
               | "try" statement "catch" catch_clause_list { RULE
                     if(!$2.isa<CompoundStmtPtr>()) { driver.error(@2, "try body must be a compound"); YYABORT; }
		             $$ = driver.builder.tryCatchStmt($2.as<CompoundStmtPtr>(), $4);
                 }   
                    /* end of control flow */
               | "return" expression ";" { RULE $$ = driver.builder.returnStmt($2); }
               | "return" ";" {  RULE $$ = driver.builder.returnStmt(driver.builder.getLangBasic().getUnitConstant()); }
               | "continue" ";" { RULE $$ = driver.builder.continueStmt(); }
               | "break" ";" { RULE $$ = driver.builder.breakStmt(); }
               | "throw" expression ";" { RULE $$ = driver.builder.throwStmt($2); }
               ;

catch_clause_list : 
                   "catch" "(" ")" statement { RULE
                            if(!$4.isa<CompoundStmtPtr>()) { driver.error(@4, "catch body must be a compound"); YYABORT; }
                            $$.push_back(driver.builder.catchClause(VariablePtr(), $4.as<CompoundStmtPtr>())); 
                    } 
                  | "(" var_decl ")" statement {  RULE
                            if(!$4.isa<CompoundStmtPtr>()) { driver.error(@4, "catch body must be a compound"); YYABORT; }
                            $$.push_back(driver.builder.catchClause($2, $4.as<CompoundStmtPtr>())); 
                    }
                  | "(" var_decl ")" statement "catch" catch_clause_list{ RULE
                            if(!$4.isa<CompoundStmtPtr>()) { driver.error(@4, "catch body must be a compound"); YYABORT; }
                            $6.insert($6.begin(), driver.builder.catchClause($2, $4.as<CompoundStmtPtr>()));
                            std::swap($$, $6);
                    }
                  ;

decl_stmt : var_decl ";" {RULE
                auto type = $1->getType();
                ExpressionPtr value = driver.builder.undefined(type);
                if (type.isa<RefTypePtr>()) {
                    value = driver.builder.refVar(driver.builder.undefined(type.as<RefTypePtr>()->getElementType()));
                }
                $$ = driver.builder.declarationStmt($1, value);
            }
          | var_decl "=" expression ";" {RULE
                $$ = driver.builder.declarationStmt($1, $3);
            }
          | "auto" "identifier" "=" expression ";" {RULE
		        auto var = driver.builder.variable($4.getType());
				annotations::attachName(var, $2);
                driver.add_symb(@$, $2, var);
                $$ = driver.builder.declarationStmt(var, $4);
            }
          ;

var_decl : type "identifier" { RULE
		        $$ = driver.builder.variable($1);
				annotations::attachName( $$, $2);
                driver.add_symb(@$, $2, $$);
            };


for_decl : "(" type "identifier" "=" expression ".." expression ")"  {RULE
		        $$.it = driver.builder.variable($2);
				annotations::attachName( $$.it, $3);
                driver.add_symb(@$, $3, $$.it);
                $$.low = $5;
                $$.up = $7;
                $$.step = driver.builder.literal($2, "1");
           }
         | "(" type "identifier" "=" expression ".." expression ":" expression ")" {RULE
		        $$.it = driver.builder.variable($2);
				annotations::attachName( $$.it, $3);
                driver.add_symb(@$, $3, $$.it);
                $$.low = $5;
                $$.up = $7;
                $$.step = $9;
           }
         ;

switch_case_list : switch_case { RULE $$.push_back($1); }
                 | switch_case switch_case_list { RULE $2.insert($2.begin(), $1); std::swap($$, $2); }
                 ;


switch_case :  "case" expression ":" statement { RULE
                  if(!$2.isa<LiteralPtr>()) { driver.error(@2, "case value must be a literal"); YYABORT; }
                  $$ = driver.builder.switchCase($2.as<LiteralPtr>(), $4); 
               }
            ;

compound_stmt : "{" "}" { RULE $$ = driver.builder.compoundStmt(); }
                  | "{" statement_list "}"{ RULE $$ = driver.builder.compoundStmt($2); }
                  ;

markable_compound_stmt :  compound_stmt         { RULE INSPIRE_GUARD(@1, $1); $$ = $1; } 
                       | "$" compound_stmt "$" {  RULE INSPIRE_GUARD(@2, $2); $$ = driver.mark_address(@2, $2); } 
                       ;

statement_list : statement { RULE     
                        if($1 != driver.builder.getNoOp())  $$.push_back($1); 
                    }
               | statement statement_list { RULE 
                        if($1 != driver.builder.getNoOp())  $2.insert($2.begin(),$1); 
                        std::swap($$, $2); 
                    }
               ;

/* ~~~~~~~~~~~~~~~~~~~  EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

expression : markable_expression         { RULE INSPIRE_GUARD(@1, $1); $$ = $1; } 
           | "$" markable_expression "$" {  RULE INSPIRE_GUARD(@2, $2); $$ = driver.mark_address(@2, $2); } 

expression_list : expression                      { RULE $$.push_back($1); }
                | expression "," expression_list  { RULE $3.insert($3.begin(), $1); std::swap($$, $3); }
 
markable_expression : "identifier" { RULE $$ = driver.findSymbol(@$, $1); }

            /* unary */
           | "*" expression { RULE  
                                          INSPIRE_TYPE(@2, $2->getType(), RefTypePtr, "cannot deref non ref type");
                                          $$ = driver.builder.deref($2); } %prec UDEREF
           | "-" expression { RULE 
                                          auto tmp = driver.builder.tryDeref($2);
                                          $$ = driver.builder.minus(tmp); 
                            } %prec UMINUS
           | "!" expression { RULE 
                                          auto tmp = driver.builder.tryDeref($2);
                                          $$ = driver.builder.logicNeg(tmp); 
                            }%prec UNOT
           | expression "=" expression  { RULE $$ = driver.genBinaryExpression(@$, "=", $1, $3); }

            /* bitwise / logic / arithmetic / geometric */
           | expression "&" expression  { RULE $$ = driver.genBinaryExpression(@$, "&", $1, $3);  }
           | expression "|" expression  { RULE $$ = driver.genBinaryExpression(@$, "|", $1, $3);  }
           | expression "^" expression  { RULE $$ = driver.genBinaryExpression(@$, "^", $1, $3);  }
           | expression "&&" expression { RULE $$ = driver.genBinaryExpression(@$, "&&", $1, $3);  }
           | expression "||" expression { RULE $$ = driver.genBinaryExpression(@$, "||", $1, $3);  }
           | expression "+" expression  { RULE $$ = driver.genBinaryExpression(@$, "+", $1, $3);  }
           | expression "-" expression  { RULE $$ = driver.genBinaryExpression(@$, "-", $1, $3);  }
           | expression "*" expression  { RULE $$ = driver.genBinaryExpression(@$, "*", $1, $3);  }
           | expression "/" expression  { RULE $$ = driver.genBinaryExpression(@$, "/", $1, $3);  }
           | expression "%" expression  { RULE $$ = driver.genBinaryExpression(@$, "%", $1, $3);  }
           | expression "==" expression { RULE $$ = driver.genBinaryExpression(@$, "==", $1, $3);  }
           | expression "!=" expression { RULE $$ = driver.genBinaryExpression(@$, "!=", $1, $3);  }
           | expression "<" expression  { RULE $$ = driver.genBinaryExpression(@$, "<", $1, $3);  }
           | expression ">" expression  { RULE $$ = driver.genBinaryExpression(@$, ">", $1, $3);  }
           | expression "<=" expression { RULE $$ = driver.genBinaryExpression(@$, "<=", $1, $3);  }
           | expression ">=" expression { RULE $$ = driver.genBinaryExpression(@$, ">=", $1, $3);  }
            /* data access */
           | expression "[" expression "]" { RULE $$ = driver.genBinaryExpression(@$, "[", $1, $3); }
           | expression "." "identifier" { RULE $$ = driver.genFieldAccess(@1, $1, $3); }
           | expression "." "int"        { RULE $$ = driver.genTupleAccess(@1, $1, $3); }
                
            /* ternary operator */
           | expression "?" expression ":" expression { RULE
						$$ =  driver.builder.ite($1, driver.builder.wrapLazy($3), driver.builder.wrapLazy($5));
                    }

            /* call expr */
           | expression "->" "identifier" "(" ")"{ RULE 
                                        auto mf = driver.findSymbol(@3, $3);
                                        INSPIRE_GUARD(@1, mf);
                                        INSPIRE_TYPE(@1, mf->getType(), FunctionTypePtr, "non callable expr"); 
                                        $$ = driver.genCall(@$, mf, toVector($1));  
                                }
           | expression "->" "identifier" "(" expression_list ")"{ RULE 
                                        auto mf = driver.findSymbol(@3, $3);
                                        INSPIRE_TYPE(@1, mf->getType(), FunctionTypePtr, "non callable expr"); 
                                        $5.insert($5.begin(), $1);
                                        $$ = driver.genCall(@$, mf, $5);  
                                }
           | expression "(" ")" { RULE 
                                        INSPIRE_TYPE(@1, $1->getType(), FunctionTypePtr, "non callable expr"); 
                                        $$ = driver.genCall(@$, $1, ExpressionList());  
                                }
           | expression "(" expression_list ")" { RULE 
                                        INSPIRE_TYPE(@1, $1->getType(), FunctionTypePtr, "non callable expr"); 
                                        $$ = driver.genCall(@$, $1, $3); 
                                }

            /* parenthesis : tuple or expression */
           | "(" expression_list ")"  { RULE  
                                                  if ($2.size() == 1) $$ = $2[0];
                                                  else $$ = driver.builder.tupleExpr($2);
             }
            /* lambda or closure expression: callable expression */
           |  "lambda" lambda_expression  { RULE $$ = $2; }
            /* cast */ 
           | "CAST(" type ")" expression  { RULE $$ = driver.builder.castExpr($2, $4); }
           | expression ".as(" type ")"   { RULE 
                              INSPIRE_MSG(@1, $1.getType().isa<RefTypePtr>(), "can not get parent-reference of non referenced expression");
                              $$  = driver.builder.refParent($1, $3); 
                          }
            /* ref mamagement */
           | "undefined" "(" type ")"     { RULE $$ =  driver.builder.undefined( $3 ); }
           | "var" "(" expression ")"     { RULE $$ =  driver.builder.refVar( $3 ); }
           | "new" "(" expression ")"     { RULE $$ =  driver.builder.refNew( $3 ); }
           | "loc" "(" expression ")"     { RULE $$ =  driver.builder.refLoc( $3 ); }
           | "delete" "(" expression ")"  { RULE $$ =  driver.builder.refDelete( $3 ); }
            /* literals */
           | "bool"       { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getBool(), $1); }
           | "charlit"    { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getChar(), $1); }
           | "int"        { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getInt4(), $1); }
           | "uint"       { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getUInt4(), $1); }
           | "long"       { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getInt8(), $1); }
           | "ulong"      { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getUInt8(), $1); }
           | "longlong"   { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getInt16(), $1); }
           | "ulonglong"  { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getUInt16(), $1); }
           | "float"      { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getReal4(), $1); }
           | "double"     { RULE $$ = driver.builder.literal(driver.mgr.getLangBasic().getReal8(), $1); }
           | "stringlit"  { RULE $$ = driver.builder.stringLit($1); }
            /* constructed literals */
           | "type(" type ")"         { RULE $$ = driver.builder.getTypeLiteral($2); }
           | "param(" "paramvar" ")"  { RULE $$ = driver.builder.getIntTypeParamLiteral(driver.find_type_param_var(@2, $2)); }
           | "param(" "int" ")"       { RULE $$ = driver.builder.getIntTypeParamLiteral(driver.builder.concreteIntTypeParam(utils::numeric_cast<uint32_t>($2))); }
           | "lit(" "stringlit" ")"          { RULE 
                                                    $2.replace(0,1,"");
                                                    $2.replace($2.size()-1,1,"");
                                                    $$ = driver.builder.getIdentifierLiteral($2); }
           | "lit(" "stringlit" ":" type ")" { RULE 
                                                    $2.replace(0,1,"");
                                                    $2.replace($2.size()-1,1,"");
                                                    $$ = driver.builder.literal($4, $2); }
           | "lit(" type ")"                 { RULE $$ = driver.builder.getTypeLiteral($2); }
            /* list expression */
           | "[" expression_list "]"         { RULE 
                            $$ = encoder::toIR<ExpressionList, encoder::DirectExprListConverter>(driver.mgr, $2); }
            /* struct / union expressions */
           | "struct" type "{" expression_list "}" { RULE $$ = driver.genTagExpression(@$, $2, $4); }
            /* async */
           | "spawn" expression { RULE 
                        $$ = driver.builder.parallel($2, 1);  
                }
           | "spawn" markable_compound_stmt { RULE
                        $$ = driver.builder.parallel($2, 1);
                }
           | "sync" expression  { RULE 
                        $$ = driver.builder.callExpr(driver.builder.getLangBasic().getUnit(), 
                                                     driver.builder.getLangBasic().getMerge(), $2);
                }
           | "syncAll" { RULE 
                    $$ = driver.builder.callExpr(driver.builder.getLangBasic().getUnit(), driver.builder.getLangBasic().getMergeAll()); 
                }
            /* job expressions */
           | "job" "(" "[" expression ":" expression "]" "," expression ")" { RULE
                    INSPIRE_TYPE(@9, $9, CallExprPtr, "expresion in job must be a call expression");
                    auto bind = driver.builder.bindExpr(VariableList(), $9.as<CallExprPtr>());
                    $$ = driver.builder.jobExpr($4, $6, bind);
             }
           | "task" "{" statement "}" { RULE 
                    $$ = driver.builder.jobExpr($3, 1);
             }
           | "job" "{" statement "}" { RULE 
                    // builds a job for more than a sigle thread
                    $$ = driver.builder.jobExpr($3, -1);
             }
           ;

lambda_expression_aux : 
                            /* closures */
                       "(" variable_list "=>" expression { RULE

                            if (driver.let_count == 1) {
                                driver.add_let_expression(@$, driver.genClosure(@1, $2, $4));
                            }
                            else {
                                $$ = driver.genClosure(@$, $2, $4);
                            }
                        } %prec LAMBDA

                      | "(" variable_list "=>" markable_compound_stmt  { RULE
                            
                            if (driver.let_count == 1) {
                                driver.add_let_expression(@$, driver.genClosure(@1, $2, $4));
                            }
                            else{
                                 $$ = driver.genClosure(@$, $2, $4);
                            }
                        } %prec LAMBDA
                            /* lambdas */
                      | "(" variable_list "->" type { driver.set_inhibit(driver.let_count);} markable_compound_stmt { 

                            if (driver.let_count ==1 ){
                                driver.add_let_lambda(@$, @1, @6, $4, $2);
                            }
                            else{
                                  RULE $$ = driver.genLambda(@$, $2, $4, $6); 
                            }
                            driver.set_inhibit(false);
                        } %prec LAMBDA

                            /* member functions */
                      | just_name "::" { driver.add_this(@1, $1); } "(" variable_list "->" type 
                                        { driver.set_inhibit(driver.let_count);} markable_compound_stmt { 

                            auto thisvar = driver.findSymbol(@1, "this");
                            $5.insert($5.begin(), thisvar.as<VariablePtr>());
                            if (driver.let_count ==1 ){
                                driver.add_let_lambda(@$, @1, @9, $7, $5, FK_MEMBER_FUNCTION);
                            }
                            else{
                                RULE $$ = driver.genLambda(@$, $5, $7, $9, FK_MEMBER_FUNCTION); 
                            }
                            driver.set_inhibit(false);
                        } %prec LAMBDA

                      | "~" just_name "::" { driver.add_this(@1, $2); } "(" variable_list 
                                            { driver.set_inhibit(driver.let_count);} markable_compound_stmt { 

                            auto thisvar = driver.findSymbol(@1, "this");
                            $6.insert($6.begin(),  thisvar.as<VariablePtr>());
                            if (driver.let_count ==1 ){
                                driver.add_let_lambda(@$, @1, @8, thisvar->getType(), $6, FK_DESTRUCTOR);
                            }
                            else{
                                RULE $$ = driver.genLambda(@$, $6, thisvar->getType(), $8, FK_DESTRUCTOR); 
                            }
                            driver.set_inhibit(false);
                        } %prec LAMBDA

                      | "ctor" just_name "::" { driver.add_this(@2, $2); } "(" variable_list 
                                               { driver.set_inhibit(driver.let_count);} markable_compound_stmt { 

                            auto thisvar = driver.findSymbol(@2, "this");
                            $6.insert($6.begin(), thisvar.as<VariablePtr>());
                            if (driver.let_count ==1 ){
                                driver.add_let_lambda(@$, @1, @8, thisvar->getType(), $6, FK_CONSTRUCTOR);
                            }
                            else{
                                RULE $$ = driver.genLambda(@$, $6, thisvar->getType(), $8, FK_CONSTRUCTOR); 
                            }
                            driver.set_inhibit(false);
                        }  %prec LAMBDA
                     ;


lambda_expression : { driver.open_scope(@$,"lambda expr"); } lambda_expression_aux 
                    { driver.close_scope(@$, "lambda expr"); $$ = $2; }
                  ;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// the lowest in list, the highest precedence

%nonassoc "::" ;
%nonassoc ")";
%nonassoc "else";
%right "=>";
%left "spawn" "sync" "syncAll";
%right "?";
%right "catch";
%left "=";
%left "->";
%nonassoc ":";
%left LAMBDA;
%left "&&" "||";
%left "<" "==" "!=" "<=" ">" ">=";
%left "%";
%left "+" "-";
%left "*" "/";
%left "&" "|" "^";
%nonassoc UDEREF;
%nonassoc UMINUS;
%nonassoc BOOL_OP;
%nonassoc UNOT;
%right "[";
%left ".as(";
%nonassoc ".";
%right "(";

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


