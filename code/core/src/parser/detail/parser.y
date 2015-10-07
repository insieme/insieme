%skeleton "lalr1.cc" /* -*- C++ -*- */
/* glr.cc does not support type variants, 
 * there is work in process to suport it, 
 * lets wait until 2016 and then we can use the $#%& ambigous grammar again
 */
%require "3.0.0"
%defines
%define parser_class_name {InspireParser}
%define api.namespace {insieme::core::parser::detail}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%code requires
{
	/**
	 * this code goes in the header
	 */

	#include <utility>
	#include <string>
	#include <iostream>
	#include "insieme/core/ir.h"

	namespace insieme {
	namespace core {
	namespace parser {
	namespace detail {

		class InspireDriver;
		class InspireScanner;

	} // detail
	} // parser
	} // core
	} // insieme

	using namespace insieme::core;
}

// The parsing context.
%param { parser::detail::InspireDriver& driver }
%param { parser::detail::InspireScanner& scanner }
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

	#include "insieme/core/parser/detail/driver.h"
	#include "insieme/core/parser/detail/scanner.h"
	#include "insieme/core/ir.h"

	#include "insieme/core/annotations/naming.h"
	#include "insieme/core/analysis/ir_utils.h"
	#include "insieme/core/encoder/lists.h"

	#include "insieme/core/lang/parallel.h"
}

%define api.token.prefix {TOK_}
%token
	END  0       "end of stream"
	ASSIGN       "="
	MINUS        "-"
	PLUS         "+"
	STAR         "*"
	SLASH        "/"
	PERCENT      "%"
	HASH         "#"

	LPAREN       "("
	RPAREN       ")"
	LCURBRACKET  "{"
	RCURBRACKET  "}"
	LBRACKET     "["
	RBRACKET     "]"

	LT           "<"
	GT           ">"
	LEQ          "<="
	GEQ          ">="
	EQ           "=="
	NEQ          "!="

	BAND         "&"
	BOR          "|"
	BXOR         "^"

	LAND         "&&"
	LOR          "||"
	LNOT         "!"

	QMARK        "?"
	COLON        ":"
	NAMESPACE    "::"
	FUNNY_BOY    "~"

	ARROW        "->"
	DARROW       "=>"
	TARROW       "~>"

	SEMIC        ";"
	COMA         ","
	RANGE        ".."
	DOT          "."
	ADDRESS      "$"

	USING        "using"
	ALIAS        "alias"

	DECL         "decl"
	DEF          "def"

	VIRTUAL      "virtual"
	CONST        "const"
	VOLATILE     "volatile"
	PRIVATE      "private"
	PUBLIC       "public"
	PROTECTED    "protected"

	FUNCTION     "function"
	LAMBDA       "lambda"
	CTOR         "ctor"
	TYPE_LIT     "type_lit"
	STRUCT       "struct"
	UNION        "union"

	LET          "let"
	IN           "in"

	PARENT       "as"
	THIS         "this"

	TRUE         "true"
	FALSE        "false"

	CAST         "CAST"
	AUTO         "auto"
	UNDEFINED    "undefined"

	VAR          "var"
	IF           "if"
	ELSE         "else"
	FOR          "for"
	WHILE        "while"
	TRY          "try"
	THROW        "throw"
	CATCH        "catch"
	RETURN       "return"
	CONTINUE     "continue"
	BREAK        "break"
	SWITCH       "switch"
	CASE         "case"
	DEFAULT      "default"


	SPAWN        "spawn"
	SYNC         "sync"
	SYNCALL      "sync_all"

	JOB          "job"

	TYPE_ONLY    "initial-type-parser-marker"
	EXPR_ONLY    "initial-expression-parser-marker"
	STMT_ONLY    "initial-statement-parser-marker"
	FULL_PROG    "initial-program-parser-marker"
;



    /* Literals */
%token <std::string> STRING            "string"
%token <std::string> IDENTIFIER        "identifier"
%token <std::string> TYPE_VAR          "type_var"
%token <std::string> TAG_REF           "tag_ref"
%token <std::string> INT               "int"
%token <std::string> UINT              "uint"
%token <std::string> LONG              "long"
%token <std::string> ULONG             "ulong"
%token <std::string> LONGLONG          "longlong"
%token <std::string> ULONGLONG         "ulonglong"
%token <std::string> FLOAT             "float"
%token <std::string> DOUBLE            "double"
%token <std::string> LIT               "lit"


%type <TypePtr>                        type plain_type let_type
%type <TypeList>                       types non_empty_types type_param_list
%type <ExpressionPtr>                  expression plain_expression let_expression
%type <ExpressionList>                 expressions non_empty_expressions
%type <StatementPtr>                   statement plain_statement let_statement
%type <ProgramPtr>                     main
%type <NodePtr>                        definition

%type <TagTypePtr>                     record_definition
%type <NodePtr>                        function_definition

%type <FieldPtr>                       field
%type <FieldList>                      fields
%type <LambdaExprPtr>                  constructor
%type <LambdaExprList>                 constructors
%type <LambdaExprPtr>                  destructor
%type <MemberFunctionPtr>              member_function
%type <MemberFunctionList>             member_functions
%type <PureVirtualMemberFunctionPtr>   pure_virtual_member_function
%type <PureVirtualMemberFunctionList>  pure_virtual_member_functions

%type <TypePtr>                        object_type qual_object_type generic_type parallel_type
%type <TypeVariablePtr>                type_variable
%type <GenericTypePtr>                 abstract_type
%type <FunctionTypePtr>                function_type pure_function_type closure_type constructor_type destructor_type member_function_type virtual_function_type
%type <NumericTypePtr>                 numeric_type
%type <TupleTypePtr>                   tuple_type
%type <TagTypeReferencePtr>            tag_type_reference

%type <ParentPtr>                      parent
%type <ParentList>                     parent_spec parents non_empty_parents

%type <AccessSpecifier>                access_specifier
%type <NodeType>                       struct_or_union
%type <bool>                           virtual_flag lambda_or_function
%type <pair<bool,bool>>                cv_flags


%type <ExpressionPtr>                  variable
%type <LiteralPtr>                     literal
%type <ExpressionPtr>                  call
%type <LambdaExprPtr>                  lambda
%type <BindExprPtr>                    bind
%type <ExpressionPtr>                  undefined_expression parallel_expression list_expression initializer unary_op binary_op ternary_op this_expression

%type <VariablePtr>                    parameter
%type <VariableList>                   parameters non_empty_parameters


%type <CompoundStmtPtr>                compound_statement
%type <DeclarationStmtPtr>             variable_declaration
%type <IfStmtPtr>                      if_statement
%type <SwitchStmtPtr>                  switch_statement
%type <WhileStmtPtr>                   while_statement
%type <ForStmtPtr>                     for_statement
%type <BreakStmtPtr>                   break
%type <ContinueStmtPtr>                continue
%type <ReturnStmtPtr>                  return

%type <StatementList>                  statement_list
%type <SwitchCaseList>                 switch_cases
%type <SwitchCasePtr>                  switch_case
%type <CompoundStmtPtr>                default_case

%printer { yyoutput << $$; } <std::string>


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
%%


%start top_level;


//    -- top_level -------------------------------------


top_level : TYPE_ONLY top_level_elements type                               { driver.result = driver.tu.resolve($3); }
          | EXPR_ONLY top_level_elements expression                         { driver.result = driver.tu.resolve($3); }
          | STMT_ONLY top_level_elements statement                          { driver.result = driver.tu.resolve($3); }
          | FULL_PROG top_level_elements main                               { driver.result = driver.tu.resolve($3); }
          ;

top_level_elements : top_level_element top_level_elements
                   | ";"
                   |
                   ;

top_level_element : using | alias | declaration | definition ;

using : "using" "identifier" ";" ;

alias : "alias" abstract_type "=" type ";"                                  { driver.addTypeAlias($2,$4); }
      ;

declaration : "decl" struct_or_union "identifier" ";"                       { driver.addType($3, driver.builder.tagTypeReference($3)); }
            | "decl" "identifier" ":" type ";"                              { driver.addSymb($2, driver.builder.literal($2, $4)); }
            | "decl" "identifier" "::" "identifier" ":" type ";"
            ;

definition : "def" record_definition                                        { $$ = $2; }
           | "def" function_definition                                      { $$ = $2; }
           ;

main : type "identifier" "(" parameters ")" compound_statement              { $$ = driver.builder.createProgram({driver.genLambda(@$, $4, $1, $6)}); }
     ;

//    -- record_declarations -------------------------------------

record_definition : struct_or_union "identifier" parent_spec "{"            { driver.addType(@$, $2, driver.builder.genericType($2)); }
                                    fields constructors destructor member_functions pure_virtual_member_functions "}"
                                                                            { $$ = driver.genRecordType(@$, $1, $2, $3, $6, $7, $8, $9, $10); }
                  | struct_or_union "{" fields "}"                          { $$ = driver.builder.structType($3); }
                  ;

struct_or_union : "struct"                                                  { $$ = NT_Struct; }
                | "union"                                                   { $$ = NT_Union; }
                ;

fields : fields field                                                       { $1.push_back($2); $$ = $1; }
       | fields ";"                                                         { $$ = $1; }
       |                                                                    { $$ = FieldList(); }
       ;

field : "identifier" ":" type                                               { $$ = driver.builder.field($1, $3); }
      ;

constructors : constructors constructor                                     { $1.push_back($2); $$ = $1; }
             |                                                              { $$ = LambdaExprList(); }
             ;

constructor : "ctor" "(" parameters ")" compound_statement                  { $$ = driver.genConstructor(@$, $3, $5); }
            ;

destructor : "dtor" "(" ")" compound_statement                              { $$ = driver.genDestructor(@$, $4); }
           |                                                                { $$ = LambdaExprPtr(); }
           ;

member_functions : member_functions member_function                         { $1.push_back($2); $$ = $1; }
                 |                                                          { $$ = MemberFunctionList(); }
                 ;

member_function : virtual_flag lambda_or_function "identifier" "=" lambda
                                                                            { $$ = driver.genMemberFunction(@$, $1, false, false, $3, $5, $2); }
                | virtual_flag "const" lambda_or_function "identifier" "=" lambda
                                                                            { $$ = driver.genMemberFunction(@$, $1, true, false, $4, $6, $3); }
                | virtual_flag "volatile" lambda_or_function "identifier" "=" lambda
                                                                            { $$ = driver.genMemberFunction(@$, $1, false, true, $4, $6, $3); }
                | virtual_flag "const" "volatile" lambda_or_function "identifier" "=" lambda
                                                                            { $$ = driver.genMemberFunction(@$, $1, true, true, $5, $7, $4); }
                | virtual_flag "volatile" "const" lambda_or_function "identifier" "=" lambda
                                                                            { $$ = driver.genMemberFunction(@$, $1, true, true, $5, $7, $4); }
                ;

virtual_flag : "virtual"                                                    { $$ = true; }
             |                                                              { $$ = false; }
             ;

cv_flags :                                                                  { $$ = {false, false}; }
         | "const"                                                          { $$ = {true, false}; }
         | "volatile"                                                       { $$ = {false, true}; }
         | "const" "volatile"                                               { $$ = {true, true}; }
         | "volatile" "const"                                               { $$ = {true, true}; }
         ;

pure_virtual_member_functions : pure_virtual_member_functions pure_virtual_member_function  { $1.push_back($2); $$ = $1; }
                              |                                                             { $$ = PureVirtualMemberFunctionList(); }
                              ;

pure_virtual_member_function : "pure" "virtual" cv_flags "identifier" ":" pure_function_type    { $$ = PureVirtualMemberFunctionPtr(); }
                             ;


//    -- function_declarations -------------------------------------

function_definition : lambda_or_function "identifier" "=" lambda          {
                                                                            $$ = $4;
                                                                            driver.addSymb($2, $4);
                                                                            driver.tu.addFunction(driver.builder.literal($2, $4->getType()), $4);
                                                                          }
                    ;

lambda_or_function : "lambda"                                             { $$ = true; }
                   | "function"                                           { $$ = false; }
                   ;


//    -- types -------------------------------------------


type : plain_type                                                         { $$ = $1; }
     | let_type                                                           { $$ = $1; }
     ;

plain_type : object_type                                                  { $$ = $1; }
           | function_type                                                { $$ = $1; }
           | numeric_type                                                 { $$ = $1; }
           | tuple_type                                                   { $$ = $1; }
           | parallel_type                                                { $$ = $1; }
           ;


object_type : type_variable                                               { $$ = $1; }
            | generic_type                                                { $$ = $1; }
            | tag_type_reference                                          { $$ = $1; }
            | record_definition                                           { $$ = $1; }
            ;

types : non_empty_types                                                   { $$ = $1; }
      |                                                                   { $$ = TypeList(); }
      ;

non_empty_types : non_empty_types "," type                                { $1.push_back($3); $$ = $1; }
                | type                                                    { $$ = toVector<TypePtr>($1); }
                ;


// -- type variable --

type_variable : "type_var"                                                { $$ = driver.builder.typeVariable($1); }
              ;

// -- abstract type --

abstract_type : "identifier" parent_spec type_param_list                  { $$ = driver.builder.genericType($1, $2, $3); }
              ;

generic_type : abstract_type                                              { $$ = driver.resolveTypeAliases(@$, $1); }
             ;

type_param_list :                                                         { $$ = TypeList(); }
                | "<" types ">"                                           { $$ = $2; }
                ;

parent_spec :                                                             { $$ = ParentList(); }
            | ":" "[" parents "]"                                         { $$ = $3; }
            ;

parents :                                                                 { $$ = ParentList(); }
        | non_empty_parents                                               { $$ = $1; }
        ;

non_empty_parents : non_empty_parents "," parent                          { $1.push_back($3); $$ = $1; }
                  | parent                                                { $$ = toVector($1); }
                  ;

parent : virtual_flag access_specifier object_type                        { $$ = driver.builder.parent($1, $2, $3); }
       ;

access_specifier : "public"                                               { $$ =  AS_PUBLIC; }
                 | "protected"                                            { $$ =  AS_PROTECTED; }
                 | "private"                                              { $$ =  AS_PRIVATE; }
                 |                                                        { $$ =  AS_PUBLIC; }
                 ;


// -- function type --

function_type : pure_function_type                                        { $$ = $1; }
              | closure_type                                              { $$ = $1; }
              | constructor_type                                          { $$ = $1; }
              | destructor_type                                           { $$ = $1; }
              | member_function_type                                      { $$ = $1; }
              | virtual_function_type                                     { $$ = $1; }
              ;

pure_function_type : "(" ")" "->" type                                    { $$ = driver.builder.functionType(TypeList(), $4, FK_PLAIN); }
                   | "(" non_empty_types ")" "->" type                    { $$ = driver.builder.functionType($2, $5, FK_PLAIN); }
                   ;

closure_type : "(" ")" "=>" type                                          { $$ = driver.builder.functionType(TypeList(), $4, FK_CLOSURE); }
             | "(" non_empty_types ")" "=>" type                          { $$ = driver.builder.functionType($2, $5, FK_CLOSURE); }
             ;

constructor_type : qual_object_type "::" "(" types ")"                    { $4.insert($4.begin(), $1); $$ = driver.builder.functionType($4, $1, FK_CONSTRUCTOR); }
                 ;

destructor_type : "~" qual_object_type "::" "(" ")"                       { $$ = driver.builder.functionType(toVector($2), $2, FK_DESTRUCTOR); }
                ;

member_function_type : qual_object_type "::" "(" types ")" "->" type      { $4.insert($4.begin(), $1); $$ = driver.builder.functionType($4, $7, FK_MEMBER_FUNCTION); }
                     ;

virtual_function_type : qual_object_type "::" "(" types ")" "~>" type     { $4.insert($4.begin(), $1); $$ = driver.builder.functionType($4, $7, FK_VIRTUAL_MEMBER_FUNCTION); }
                      ;

qual_object_type : object_type                                            { $$ = lang::ReferenceType::create($1); }
                 | "const" object_type                                    { $$ = lang::ReferenceType::create($2, true,  false); }
                 | "volatile" object_type                                 { $$ = lang::ReferenceType::create($2, false, true); }
                 | "const" "volatile" object_type                         { $$ = lang::ReferenceType::create($3, true,  true); }
                 | "volatile" "const" object_type                         { $$ = lang::ReferenceType::create($3, true,  true); }
                 ;


// -- numeric type --

numeric_type : "int"                                                      { $$ = driver.genNumericType(@$, $1); }
             | "#" "identifier"                                           { $$ = driver.genNumericType(@$, $2); }
             ;

// -- tuple types --

tuple_type : "(" ")"                                                      { $$ = driver.builder.tupleType(); }
           | "(" non_empty_types ")"                                      { $$ = driver.builder.tupleType($2); }
           ;

// -- parallel types --

parallel_type : "job"                                                     { $$ = driver.genGenericType(@$, "job", ParentList(), TypeList()); }
              ;

// -- tag reference --

tag_type_reference : "tag_ref"                                            { $$ = driver.builder.tagTypeReference($1); }
                   ;

// -- let --

let_type : "let" "identifier" "="  type                                   { driver.openScope(); driver.addType($2, $4); }
                              "in" type                                   { $$ = $7; driver.closeScope(); }
         ;


//    -- expressions -------------------------------------

expression : plain_expression                                             { $$ = $1; }
           | "$" plain_expression "$"                                     { $$ = driver.markAddress(@2, $2); }
           | let_expression                                               { $$ = $1; }
           ;

expressions : non_empty_expressions                                       { $$ = $1; }
            |                                                             { $$ = ExpressionList(); }
            ;

non_empty_expressions : non_empty_expressions "," expression              { $1.push_back($3); $$ = $1; }
                      | expression                                        { $$ = toVector($1); }
                      ;

plain_expression : variable                                               { $$ = $1; }
                 | literal                                                { $$ = $1; }
                 | call                                                   { $$ = $1; }
                 | lambda                                                 { $$ = $1; }
                 | bind                                                   { $$ = $1; }
                 | undefined_expression                                   { $$ = $1; }
                 | parallel_expression                                    { $$ = $1; }
                 | list_expression                                        { $$ = $1; }
                 | initializer                                            { $$ = $1; }
                 | unary_op                                               { $$ = $1; }
                 | binary_op                                              { $$ = $1; }
                 | ternary_op                                             { $$ = $1; }
                 | this_expression                                        { $$ = $1; }
                 ;


// -- variable --

variable : "identifier"                                                   { $$ = driver.findSymbol(@$, $1); }
         ;


// -- literal --

literal : "true"                                                          { $$ = driver.builder.boolLit(true); }
        | "false"                                                         { $$ = driver.builder.boolLit(false); }
        | "int"                                                           { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getInt4(), $1); }
        | "uint"                                                          { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getUInt4(), $1); }
        | "long"                                                          { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getInt8(), $1); }
        | "ulong"                                                         { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getUInt8(), $1); }
        | "longlong"                                                      { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getInt16(), $1); }
        | "ulonglong"                                                     { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getUInt16(), $1); }
        | "float"                                                         { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getReal4(), $1); }
        | "double"                                                        { $$ = driver.genNumericLiteral(@$, driver.mgr.getLangBasic().getReal8(), $1); }
        | "string"                                                        { $$ = driver.builder.stringLit($1); }
        | "lit" "(" "string" ":" type ")"                                 { $$ = driver.builder.literal($5, $3.substr(1, $3.size() - 1)); }
        | "type_lit" "(" type ")"                                         { $$ = driver.builder.getTypeLiteral($3); }
        ;


// -- call --

call : expression "(" expressions ")"                                     { $$ = driver.genCall(@$, $1, $3); }
     ;


// -- lambda --

lambda : "(" ")" "->" type compound_statement                             { $$ = driver.genLambda(@$, VariableList(), $4, $5); }
       | "(" non_empty_parameters ")" "->" type compound_statement        { $$ = driver.genLambda(@$, $2, $5, $6); }
       ;

parameters : non_empty_parameters                                         { $$ = $1; }
           |                                                              { $$ = VariableList(); }
           ;

non_empty_parameters : non_empty_parameters "," parameter                 { $1.push_back($3); $$ = $1; }
                     | parameter                                          { $$ = toVector($1); }
                     ;

parameter : "identifier" ":" type                                         { $$ = driver.genParameter(@$, $1, $3); }
          ;


// -- bind --

bind : "(" ")" "=>" expression                                            { $$ = driver.genClosure(@$, VariableList(), driver.getScalar($4)); }
     | "(" non_empty_parameters ")" "=>" expression                       { $$ = driver.genClosure(@$, $2, driver.getScalar($5)); }
     ;


// -- let --

let_expression : "let" "identifier" "=" expression                        { driver.openScope(); driver.addSymb($2, $4); }
                                    "in" expression                       { $$ = $7; driver.closeScope(); }
               ;

// -- reference expressions --

undefined_expression : "undefined" "(" type ")"                           { $$ = driver.builder.undefined($3); }
                     ;

// -- parallel expressions --

parallel_expression : "job" "[" expression ".." expression "]" "=>" expression  { $$ = driver.genJobExpr(@$, $3, $5, $8); }
                    | "job" "[" "]" "=>" expression                             { $$ = driver.genJobExpr(@$, $5); }
                    | "spawn" expression                                        { $$ = driver.builder.parallel(driver.getScalar($2), 1); }
                    | "sync" expression                                         { $$ = driver.genSync(@$, $2); }
                    | "sync_all"                                                { $$ = driver.genSyncAll(@$); }
                    ;


list_expression : "[" non_empty_expressions "]"                           { $$ = ExpressionPtr(); }
                | "[" expressions ":" type "]"                            { $$ = ExpressionPtr(); }
                ;


// -- initializer --

initializer : "<" type ">" "{" expressions "}"                            { $$ = driver.getInitiaizerExpr(@$, $2, $5); }
            | "(" ")"                                                     { $$ = driver.builder.tupleExpr(); }
            | "(" non_empty_expressions ")"                               { $$ = driver.builder.tupleExpr($2); }
            ;

unary_op : "-" expression                                                 { $$ = driver.builder.minus(driver.getScalar($2)); }       %prec UMINUS
         | "*" expression                                                 { $$ = driver.genDerefExpr(@1, $2); }                      %prec UDEREF
         | "!" expression                                                 { $$ = driver.builder.logicNeg(driver.getScalar($2)); }    %prec UNOT
         | expression "." "identifier"                                    { $$ = driver.genFieldAccess(@1, $1, $3); }
         | expression "." "int"                                           { $$ = driver.genTupleAccess(@1, $1, $3); }
         | expression "->" "identifier"                                   { $$ = driver.genFieldAccess(@1, $1, $3); }
         | expression "->" "int"                                          { $$ = driver.genTupleAccess(@1, $1, $3); }
         | "CAST" "(" type ")" expression                                 { $$ = driver.builder.castExpr($3, $5); }
         | expression "." "as" "(" type ")"                               { $$ = driver.genAsExpr(@1, $1, $5); }
         ;

binary_op : expression "="  expression                                    { $$ = driver.genBinaryExpression(@$, "=",  $1, $3); }
          | expression "+"  expression                                    { $$ = driver.genBinaryExpression(@$, "+",  $1, $3); }
          | expression "-"  expression                                    { $$ = driver.genBinaryExpression(@$, "-",  $1, $3); }
          | expression "*"  expression                                    { $$ = driver.genBinaryExpression(@$, "*",  $1, $3); }
          | expression "/"  expression                                    { $$ = driver.genBinaryExpression(@$, "/",  $1, $3); }
          | expression "%"  expression                                    { $$ = driver.genBinaryExpression(@$, "%",  $1, $3); }
          | expression "&&" expression                                    { $$ = driver.genBinaryExpression(@$, "&&", $1, $3); }
          | expression "||" expression                                    { $$ = driver.genBinaryExpression(@$, "||", $1, $3); }
          | expression "&"  expression                                    { $$ = driver.genBinaryExpression(@$, "&",  $1, $3); }
          | expression "|"  expression                                    { $$ = driver.genBinaryExpression(@$, "|",  $1, $3); }
          | expression "^"  expression                                    { $$ = driver.genBinaryExpression(@$, "^",  $1, $3); }
          | expression "==" expression                                    { $$ = driver.genBinaryExpression(@$, "==", $1, $3); }
          | expression "!=" expression                                    { $$ = driver.genBinaryExpression(@$, "!=", $1, $3); }
          | expression "<"  expression                                    { $$ = driver.genBinaryExpression(@$, "<",  $1, $3); }
          | expression "<=" expression                                    { $$ = driver.genBinaryExpression(@$, "<=", $1, $3); }
          | expression ">=" expression                                    { $$ = driver.genBinaryExpression(@$, ">=", $1, $3); }
          | expression ">"  expression                                    { $$ = driver.genBinaryExpression(@$, ">",  $1, $3); }
          | expression "["  expression "]"                                { $$ = driver.genBinaryExpression(@$, "[",  $1, $3); }
          ;

ternary_op : expression "?" expression ":" expression                     { $$ = driver.builder.ite(driver.getScalar($1), driver.builder.wrapLazy($3), driver.builder.wrapLazy($5)); }
           ;

this_expression : "this"                                                  { $$ = driver.genThis(@$); }
                ;


//    -- statements --------------------------------------


statement : plain_statement                                               { $$ = $1; }
          | "$" plain_statement "$"                                       { $$ = driver.markAddress(@2, $2); }
          | let_statement                                                 { $$ = $1; }
          ;

plain_statement : expression ";"                                          { $$ = $1; }
                | compound_statement                                      { $$ = $1; }
                | variable_declaration                                    { $$ = $1; }
                | if_statement                                            { $$ = $1; }
                | switch_statement                                        { $$ = $1; }
                | while_statement                                         { $$ = $1; }
                | for_statement                                           { $$ = $1; }
                | break                                                   { $$ = $1; }
                | continue                                                { $$ = $1; }
                | return                                                  { $$ = $1; }
                ;


// -- compound statement --

compound_statement : { driver.openScope(); } "{" statement_list "}"      { $$ = driver.builder.compoundStmt($3); driver.closeScope(); }
                   ;

statement_list :                                                          { $$ = StatementList(); }
               | statement_list statement                                 { $1.push_back($2); $$ = $1; }
               | statement_list ";"                                       { $$ = $1; }
               ;


// -- variable declaration --

variable_declaration : "var" type "identifier" "=" expression ";"         { $$ = driver.genVariableDeclaration(@$, $2, $3, $5); }
                     | "auto" "identifier" "=" expression ";"             { $$ = driver.genVariableDeclaration(@$, driver.getScalar($4)->getType(), $2, driver.getScalar($4)); }
                     ;


// -- if --

if_statement : "if" "(" expression ")" compound_statement                            { $$ = driver.builder.ifStmt(driver.getScalar($3), $5); }
             | "if" "(" expression ")" compound_statement "else" compound_statement  { $$ = driver.builder.ifStmt(driver.getScalar($3), $5, $7); }
             ;


// -- switch --

switch_statement : "switch" "(" expression ")" "{" switch_cases default_case "}"  { $$ = driver.builder.switchStmt(driver.getScalar($3), $6, $7); }
                 ;

switch_cases : switch_cases switch_case                                   { $1.push_back($2); $$ = $1; }
             |                                                            { $$ = SwitchCaseList(); }
             ;

switch_case : "case" literal ":" compound_statement                       { $$ = driver.builder.switchCase($2, $4); }
            ;

default_case : "default" ":" compound_statement                           { $$ = $3; }
             |                                                            { $$ = driver.builder.getNoOp(); }
             ;

// -- while --

while_statement : "while" "(" expression ")" compound_statement           { $$ = driver.builder.whileStmt(driver.getScalar($3), $5); }
                ;


// -- for --

for_statement : "for" "(" type "identifier" "=" expression ".." expression ")"
                                                                          { driver.openScope(); driver.genVariableDeclaration(@6, $3, $4, $6); }
                               compound_statement                         { $$ = driver.genForStmt(@$, $3, $4, $6, $8, driver.builder.literal($3, "1"), $11); driver.closeScope(); }
              | "for" "(" type "identifier" "=" expression ".." expression ":" expression ")"
                                                                          { driver.openScope(); driver.genVariableDeclaration(@6, $3, $4, $6); }
                               compound_statement                         { $$ = driver.genForStmt(@$, $3, $4, $6, $8, $10, $13); driver.closeScope(); }
              ;


// -- break --

break : "break" ";"                                                       { $$ = driver.builder.breakStmt(); }
      ;

// -- continue --

continue : "continue" ";"                                                 { $$ = driver.builder.continueStmt(); }
         ;

// -- return --

return : "return" expression ";"                                          { $$ = driver.builder.returnStmt(driver.getScalar($2)); }
       ;


// -- let --

let_statement : "let" "identifier" "=" expression ";"                     {  driver.addSymb(@$, $2, $4); $$ = $4; }
              ;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// the lowest in list, the highest precedence


%nonassoc "::" ;
%left     ":";
%nonassoc ")";

%nonassoc UDEREF;
%nonassoc UMINUS;
%nonassoc UNOT;
%nonassoc POST_INC POST_DEC;
%nonassoc PRE_INC PRE_DEC;
%nonassoc ".";
%nonassoc "->";

%nonassoc "else";
%right    "=>";
%left     "spawn" "sync" "sync_all";
%right    "?";
%right    "catch";
%left     "=";
%left     LAMBDA;
%left     "||";
%left     "&&";
%left     "|";
%left     "^";
%left     "&";
%left     "==" "!=";
%left     "<" "<=" ">" ">=";
%left     "+" "-";
%left     "*" "/" "%";
%right    "[";
%right    "(";
%right    "in";

%%
// code after the second %% is copyed verbatim at the end of the parser .cpp file

namespace insieme{
namespace core{
namespace parser{
namespace detail{

	using namespace insieme::core;

	void InspireParser::error(const location_type& l, const std::string& m) {
		driver.error(l, m);
	}


} // detail
} // parser
} // core
} // insieme


