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
	#include "insieme/core/parser/detail/typed_expression.h"

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

	#include "insieme/core/encoder/lists.h"

	#define INSPIRE_GUARD(l, n) \
		if(!n) { driver.error(l, "unrecoverable error"); YYABORT; }
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
	LSHIFT       "<<"

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
	ELLIPSIS     "..."
	RANGE        ".."
	DOT          "."
	ADDRESS      "$"

	USING        "using"
	ALIAS        "alias"

	DECL         "decl"
	DEF          "def"

	VIRTUAL      "virtual"
	PURE         "pure"
	CONST        "const"
	VOLATILE     "volatile"
	PRIVATE      "private"
	PUBLIC       "public"
	PROTECTED    "protected"

	FUNCTION     "function"
	LAMBDA       "lambda"
	CTOR         "ctor"
	DTOR         "dtor"
	LIT          "lit"
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

	VAR          "var"
	IF           "if"
	ELSE         "else"
	FOR          "for"
	WHILE        "while"
	RETURN       "return"
	CONTINUE     "continue"
	BREAK        "break"
	SWITCH       "switch"
	CASE         "case"
	DEFAULT      "default"
	DELETE       "delete"
	TRY          "try"
	CATCH        "catch"
	THROW        "throw"

	MATERIALIZE  "materialize"

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
%token <std::string> CHAR              "char"
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


%type <TypePtr>                        type plain_type let_type
%type <TypeList>                       types non_empty_types type_param_list
%type <ExpressionPtr>                  expression plain_expression let_expression
%type <ExpressionList>                 expressions non_empty_expressions
%type <ParserTypedExpression>          typed_expression
%type <ParserTypedExpressionList>      typed_expressions non_empty_typed_expressions
%type <StatementPtr>                   statement plain_statement let_statement
%type <ProgramPtr>                     main
%type <NodePtr>                        definition free_member_definition

%type <TypePtr>                        record_definition
%type <NodePtr>                        function_definition

%type <FieldPtr>                       field
%type <FieldList>                      fields
%type <ExpressionPtr>                  constructor
%type <ExpressionList>                 constructors
%type <pair<ExpressionPtr,bool>>       destructor
%type <MemberFunctionPtr>              member_function
%type <MemberFunctionList>             member_functions
%type <PureVirtualMemberFunctionPtr>   pure_virtual_member_function
%type <PureVirtualMemberFunctionList>  pure_virtual_member_functions

%type <TypePtr>                        object_type qual_object_type generic_type abstract_type parallel_type type_variable
%type <FunctionTypePtr>                instantiated_function_type function_type pure_function_type closure_type constructor_type destructor_type member_function_type virtual_function_type
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
%type <ExpressionPtr>                  call actual_call
%type <LambdaExprPtr>                  lambda constructor_lambda
%type <BindExprPtr>                    bind
%type <ExpressionPtr>                  parallel_expression list_expression initializer unary_op binary_op ternary_op this_expression mem_lambda_reference

%type <VariablePtr>                    parameter
%type <VariableList>                   parameters non_empty_parameters non_empty_bind_parameters


%type <CompoundStmtPtr>                compound_statement compound_statement_no_scope compound_no_scope_default_delete
%type <DeclarationStmtPtr>             variable_definition typed_variable_definition
%type <IfStmtPtr>                      if_statement
%type <SwitchStmtPtr>                  switch_statement
%type <WhileStmtPtr>                   while_statement
%type <ForStmtPtr>                     for_statement
%type <TryCatchStmtPtr>                try_catch_statement
%type <CatchClauseList>                catch_clauses
%type <CatchClausePtr>                 catch_clause
%type <ThrowStmtPtr>                   throw_statement
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


top_level : TYPE_ONLY top_level_elements type                               { driver.computeResult($3); }
          | EXPR_ONLY top_level_elements expression                         { driver.computeResult($3); }
          | STMT_ONLY top_level_elements statement                          { driver.computeResult($3); }
          | FULL_PROG top_level_elements main                               { driver.computeResult($3); }
          ;

top_level_elements : top_level_element ";" top_level_elements
                   |
                   ;

top_level_element : using | alias | declaration | definition;

using : "using" "string"                                                    { driver.importExtension(@$, $2); }
      ;

alias : "alias" abstract_type "=" type                                      { driver.addTypeAlias($2,$4); }
      ;

declaration : "decl" struct_or_union "identifier"                           { driver.declareRecordType(@3, $3); }
            | "decl" "ctor" ":" constructor_type                            { driver.genDeclaration(@2, "ctor", $4); }
            | "decl" "dtor" ":" destructor_type                             { driver.genDeclaration(@2, "dtor", $4); }
            | "decl" "identifier" ":" type                                  { driver.genDeclaration(@2, $2, $4); }
            | "decl" "identifier" "::" "identifier" ":" type                { driver.registerField(@4, $2, $4, $6); }
            ;

definition : "def" record_definition                                        { $$ = $2; }
           | "def" function_definition                                      { $$ = $2; }
           | "def" free_member_definition                                   { $$ = $2; }
           ;

main : type "identifier" "(" parameters                                     { driver.openScope(); driver.registerParameters(@4, $4); }
                                        ")" compound_statement              { $$ = driver.builder.createProgram({driver.genFunctionDefinition(@$, $2, driver.genLambda(@$, $4, $1, $7))}); driver.unregisterParameters(); driver.closeScope(); }
     | type "function" "identifier" "(" parameters                          { driver.inLambda = false; driver.openScope(); driver.registerParameters(@5, $5); }
                                        ")" compound_statement              { $$ = driver.builder.createProgram({driver.genFunctionDefinition(@$, $3, driver.genLambda(@$, $5, $1, $8))}); driver.unregisterParameters(); driver.closeScope(); driver.inLambda = true; }
     ;

//    -- record_declarations -------------------------------------

record_definition : struct_or_union "identifier" parent_spec "{"            { driver.beginRecord(@$, $2); }
                                    fields                                  { driver.registerFields(@$, $6); }
                                           constructors destructor member_functions pure_virtual_member_functions "}"
                                                                            { $$ = driver.genRecordType(@$, $1, $2, $3, $6, $8, $9.first, $9.second, $10, $11); driver.endRecord(); }
                  | struct_or_union "{" fields "}"                          { $$ = driver.genSimpleStructOrUnionType(@$, $1, $3); }
                  ;

struct_or_union : "struct"                                                  { $$ = NT_Struct; }
                | "union"                                                   { $$ = NT_Union; }
                ;

fields : fields field                                                       { $1.push_back($2); $$ = $1; }
       |                                                                    { $$ = FieldList(); }
       ;

field : "identifier" ":" type ";"                                           { $$ = driver.builder.field($1, $3); }
      ;

constructors : constructors constructor                                     { INSPIRE_GUARD(@2, $2) $1.push_back($2); $$ = $1; }
             |                                                              { $$ = ExpressionList(); }
             ;

constructor : "ctor" constructor_lambda                                     { $$ = driver.genConstructor(@$, $2); }
            ;

compound_no_scope_default_delete : compound_statement_no_scope              { $$ = $1; }
                                 | "=" "default" ";"                        { $$ = driver.builder.getDefaultedBodyPreTUMarker(); }
                                 | "=" "delete"  ";"                        { $$ = driver.builder.getDeletedBodyPreTUMarker(); }
                                 ;

constructor_lambda : "(" parameters                                         { driver.openScope(); driver.registerParameters(@2, $2); }
                         ")" compound_no_scope_default_delete               { $$ = driver.genConstructorLambda(@$, $2, $5); driver.unregisterParameters(); driver.closeScope(); }
                   | "function" "(" parameters                              { driver.inLambda = false; driver.openScope(); driver.registerParameters(@2, $3); }
                         ")" compound_no_scope_default_delete               { $$ = driver.genConstructorLambda(@$, $3, $6); driver.unregisterParameters(); driver.closeScope(); driver.inLambda = true; }
                   ;

destructor : "dtor" virtual_flag                                            { driver.openScope(); driver.registerParameters(@2, {}); }
                                   "(" ")" compound_no_scope_default_delete { $$ = std::make_pair(driver.genDestructor(@$, $6), $2); driver.unregisterParameters(); driver.closeScope(); }
           | "dtor" virtual_flag "function"                                 { driver.inLambda = false; driver.openScope(); driver.registerParameters(@2, {}); }
                                   "(" ")" compound_no_scope_default_delete { $$ = std::make_pair(driver.genDestructor(@$, $7), $2); driver.unregisterParameters(); driver.closeScope(); driver.inLambda = true; }
           |                                                                { $$ = std::make_pair(LambdaExprPtr(), false); }
           ;

member_functions : member_functions member_function                         { INSPIRE_GUARD(@2, $2) $1.push_back($2); $$ = $1; }
                 |                                                          { $$ = MemberFunctionList(); }
                 ;

member_function : virtual_flag cv_flags lambda_or_function "identifier" "=" "(" ")"     { driver.inLambda = $3; driver.openScope(); driver.registerParameters(@2, {}, $2.first, $2.second); }
                      "->" type compound_no_scope_default_delete            { $$ = driver.genMemberFunction(@$, $1, $2.first, $2.second, $4, VariableList(), $10, $11); driver.unregisterParameters(); driver.closeScope(); driver.inLambda = true; }
                | virtual_flag cv_flags lambda_or_function "identifier" "=" "(" non_empty_parameters
                                                                                        { driver.inLambda = $3; driver.openScope(); driver.registerParameters(@7, $7, $2.first, $2.second); }
                      ")" "->" type compound_no_scope_default_delete        { $$ = driver.genMemberFunction(@$, $1, $2.first, $2.second, $4, $7, $11, $12); driver.unregisterParameters(); driver.closeScope(); driver.inLambda = true; }
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

pure_virtual_member_function : "pure" "virtual" cv_flags "identifier" ":" pure_function_type    { $$ = driver.genPureVirtualMemberFunction(@$, $3.first, $3.second, $4, $6); }
                             ;


//    -- free members -------------------------------------

free_member_definition : "identifier" "::" "identifier" "=" "ctor"          { driver.beginRecord(@$, $1); }
                                           constructor_lambda               { $$ = driver.genFreeConstructor(@$, $3, $7); INSPIRE_GUARD(@$, $$) driver.endRecord(); }
                       | "identifier" "::"                                  { driver.beginRecord(@$, $1); }
                                           member_function                  { INSPIRE_GUARD(@4, $4) $$ = $4; driver.endRecord(); }
                       ;


//    -- function_declarations -------------------------------------

function_definition : "identifier" "=" lambda                             { $$ = driver.genFunctionDefinition(@$, $1, $3); }
                    ;

lambda_or_function : "lambda"                                             { $$ = true; }
                   | "function"                                           { $$ = false; }
                   ;


//    -- types -------------------------------------------


type : plain_type                                                         { $$ = $1; }
     | let_type                                                           { $$ = $1; }
     ;

plain_type : object_type                                                  { $$ = $1; }
           | instantiated_function_type                                   { $$ = $1; }
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
              | "type_var" "<" types ">"                                  { $$ = driver.builder.genericTypeVariable($1, $3); }
              | "type_var" "..."                                          { $$ = driver.builder.variadicTypeVariable($1); }
              | "type_var" "..." "<" types ">"                            { $$ = driver.builder.variadicGenericTypeVariable($1, $4); }
              ;

// -- abstract type --

abstract_type : "identifier" parent_spec type_param_list                  { $$ = driver.findOrGenAbstractType(@$, $1, $2, $3); }
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

instantiated_function_type : "<" types ">" function_type                  { $$ = driver.builder.functionType($4->getParameterTypes(), $4->getReturnType(), $4->getKind(), driver.builder.types($2)); }
                           | function_type                                { $$ = $1; }

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
             | "-" "int"                                                  { $$ = driver.genNegativeNumericType(@$, $2); }
             | "uint"                                                     { $$ = driver.genUnsignedNumericType(@$, $1); }
             | "#" variable                                               { $$ = driver.genNumericType(@$, $2); }
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

let_type : "let" "identifier" "="  type                                   { driver.openScope(); driver.declareType(@2, $2, $4); }
                              "in" type                                   { $$ = $7; driver.closeScope(); }
         ;


//    -- expressions -------------------------------------

expression : plain_expression                                             { INSPIRE_GUARD(@1, $1); $$ = $1; }
           | "$" plain_expression "$"                                     { INSPIRE_GUARD(@2, $2); $$ = driver.markAddress(@2, $2); }
           | let_expression                                               { INSPIRE_GUARD(@1, $1); $$ = $1; }
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
                 | parallel_expression                                    { $$ = $1; }
                 | list_expression                                        { $$ = $1; }
                 | initializer                                            { $$ = $1; }
                 | unary_op                                               { $$ = $1; }
                 | binary_op                                              { $$ = $1; }
                 | ternary_op                                             { $$ = $1; }
                 | this_expression                                        { $$ = $1; }
                 | mem_lambda_reference                                   { $$ = $1; }
                 ;

typed_expressions : non_empty_typed_expressions                           { $$ = $1; }
                  |                                                       { $$ = ParserTypedExpressionList(); }
                  ;

non_empty_typed_expressions : non_empty_typed_expressions "," typed_expression    { $1.push_back($3); $$ = $1; }
                            | typed_expression                                    { $$ = toVector($1); }
                            ;

typed_expression : expression ":" type                                    { $$ = driver.genTypedExpression(@$, $1, $3); INSPIRE_GUARD(@$, $$.expression); }
                 | expression                                             { $$ = {$1, $1->getType()}; INSPIRE_GUARD(@$, $$.expression); }
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
        | "char"                                                          { $$ = driver.builder.literal(driver.mgr.getLangBasic().getChar(), $1); }
        | "string"                                                        { $$ = driver.builder.stringLit($1); }
        | "lit" "(" "string" ")"                                          { $$ = driver.builder.getIdentifierLiteral($3.substr(1, $3.size() - 2)); }
        | "lit" "(" "string" ":" type ")"                                 { $$ = driver.builder.literal($5, $3.substr(1, $3.size() - 2)); }
        | "type_lit" "(" type ")"                                         { $$ = driver.builder.getTypeLiteral($3); }
        ;


// -- call --

call : actual_call                                                        { $$ = $1; }
     | actual_call "materialize"                                          { $$ = driver.materializeCall(@$, $1); }
     ;

actual_call : expression "(" typed_expressions ")"                        { $$ = driver.genCall(@$, $1, $3); }
            | "identifier" "::" "(" non_empty_typed_expressions ")"       { $$ = driver.genConstructorCall(@$, $1, $4); }
            | "identifier" "::" "~" "(" expression ")"                    { $$ = driver.genDestructorCall(@$, $1, $5); }
            ;


// -- lambda --

lambda : "(" ")" "->" type compound_statement                                  { $$ = driver.genLambda(@$, VariableList(), $4, $5); }
       | "function" "(" ")"                                                    { driver.inLambda = false; }
                            "->" type compound_statement                       { $$ = driver.genLambda(@$, VariableList(), $6, $7); driver.inLambda = true; }
       | "(" non_empty_parameters ")"                                          { driver.openScope(); driver.registerParameters(@2, $2); }
                                      "->" type compound_statement_no_scope    { $$ = driver.genLambda(@$, $2, $6, $7); driver.unregisterParameters(); driver.closeScope(); }
       | "function" "(" non_empty_parameters ")"                               { driver.openScope(); driver.registerParameters(@3, $3); driver.inLambda = false; }
                                      "->" type compound_statement_no_scope    { $$ = driver.genLambda(@$, $3, $7, $8); driver.unregisterParameters(); driver.closeScope(); driver.inLambda = true; }
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
     | "(" ")" "=>" compound_statement                                    { $$ = driver.genClosure(@$, VariableList(), $4); }
     | non_empty_bind_parameters "=>" expression                          { $$ = driver.genClosure(@$, $1, driver.getScalar($3)); driver.closeScope(); }
     | non_empty_bind_parameters "=>" compound_statement                  { $$ = driver.genClosure(@$, $1, $3); driver.closeScope(); }
     ;

non_empty_bind_parameters : "(" non_empty_parameters ")"                  { driver.openScope(); driver.registerParameters(@2, $2); $$ = $2; }
                          ;

// -- let --

let_expression : "let" "identifier" "=" expression                        { driver.openScope(); driver.declareSymbol(@2, $2, $4); }
                                    "in" expression                       { $$ = $7; driver.closeScope(); }
               ;

// -- parallel expressions --

parallel_expression : "job" "[" expression ".." expression "]" "=>" expression  { $$ = driver.genJobExpr(@$, $3, $5, $8); }
                    | "job" "[" expression ".." expression ":" expression "]" "=>" expression  { $$ = driver.genJobExpr(@$, $3, $5, $7, $10); }
                    | "job" "[" expression "..." "]" "=>" expression            { $$ = driver.genJobExpr(@$, $3, $7); }
                    | "job" "[" "]" "=>" expression                             { $$ = driver.genJobExpr(@$, $5); }
                    | "job" compound_statement                                  { $$ = driver.builder.jobExpr($2, -1); }
                    | "spawn" expression                                        { $$ = driver.builder.parallel(driver.getScalar($2), 1); }
                    | "sync" expression                                         { $$ = driver.genSync(@$, $2); }
                    | "sync_all"                                                { $$ = driver.genSyncAll(@$); }
                    ;


list_expression : "[" non_empty_expressions "]"                           { $$ = encoder::toIR<ExpressionList, encoder::DirectExprListConverter>(driver.mgr, $2); }
                | "[" expressions ":" type "]"                            { assert_not_implemented(); }
                ;


// -- initializer --

initializer : "<" type ">" "{" expressions "}"                            { $$ = driver.genInitializerExprTemp(@$, $2, $5); }
            | "<" type ">" "(" expression ")" "{" expressions "}"         { $$ = driver.genInitializerExpr(@$, $2, $5, $8); }
            | "(" ")"                                                     { $$ = driver.builder.tupleExpr(); }
            | "(" non_empty_expressions ")"                               { $$ = driver.builder.tupleExpr($2); }
            ;

unary_op : "-" expression                                                 { $$ = driver.builder.minus(driver.getScalar(driver.getOperand($2))); }       %prec UMINUS
         | "*" expression                                                 { $$ = driver.genDerefExpr(@1, $2); }                                         %prec UDEREF
         | "!" expression                                                 { $$ = driver.builder.logicNeg(driver.getScalar(driver.getOperand($2))); }    %prec UNOT
         | expression "." "identifier"                                    { $$ = driver.genMemberAccess(@3, $1, $3); }
         | expression "." "int"                                           { $$ = driver.genTupleAccess(@3, $1, $3); }
         | expression "->" "identifier"                                   { $$ = driver.genMemberAccess(@3, $1, $3); }
         | expression "->" "int"                                          { $$ = driver.genTupleAccess(@3, $1, $3); }
         | "CAST" "(" type ")" expression                                 { $$ = driver.builder.castExpr($3, $5); }                                     %prec CAST
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
          | expression "<<" expression                                    { $$ = driver.genBinaryExpression(@$, "<<", $1, $3); }
          | expression ">" ">" expression                                 { $$ = driver.genBinaryExpression(@$, ">>", $1, $4); }
          ;

ternary_op : expression "?" expression ":" expression                     { $$ = driver.builder.ite(driver.getScalar($1), driver.builder.wrapLazy(driver.getScalar($3)), driver.builder.wrapLazy(driver.getScalar($5))); }
           ;

this_expression : "this"                                                  { $$ = driver.genThis(@$); }
                ;

mem_lambda_reference : "identifier" "::" "identifier"                     { $$ = driver.genMemLambdaReference(@$, $1, $3); }
                     ;


//    -- statements --------------------------------------


statement : plain_statement                                               { INSPIRE_GUARD(@1, $1); $$ = $1; }
          | "$" plain_statement "$"                                       { INSPIRE_GUARD(@2, $2); $$ = driver.markAddress(@2, $2); }
          | let_statement                                                 { INSPIRE_GUARD(@1, $1); }
          ;

plain_statement : expression ";"                                          { $$ = $1; }
                | compound_statement                                      { $$ = $1; }
                | variable_definition                                     { $$ = $1; }
                | if_statement                                            { $$ = $1; }
                | switch_statement                                        { $$ = $1; }
                | while_statement                                         { $$ = $1; }
                | for_statement                                           { $$ = $1; }
                | try_catch_statement                                     { $$ = $1; }
                | throw_statement                                         { $$ = $1; }
                | break                                                   { $$ = $1; }
                | continue                                                { $$ = $1; }
                | return                                                  { $$ = $1; }
                ;


// -- compound statement --

compound_statement : { driver.openScope(); } "{" statement_list "}"       { $$ = driver.builder.compoundStmt($3); driver.closeScope(); }
                   ;

compound_statement_no_scope : "{" statement_list "}"                      { $$ = driver.builder.compoundStmt($2); }
                            ;

statement_list :                                                          { $$ = StatementList(); }
               | statement_list statement                                 { if ($2) $1.push_back($2); $$ = $1; }
               | statement_list ";"                                       { $$ = $1; }
               ;


// -- variable declaration --

variable_definition : typed_variable_definition                           { $$ = $1; }
                    | "auto" "identifier" "=" expression ";"              { $$ = driver.genVariableDefinition(@$, driver.getScalar($4)->getType(), $2, $4); }
                    ;

typed_variable_definition : "var" type "identifier" "="                   { INSPIRE_GUARD(@3, driver.genVariableDeclaration(@$, $2, $3)); }
                                                        expression ";"    { $$ = driver.genDeclarationStmt(@$, $3, $6); }
                          | "var" type "identifier" ";"                   { $$ = driver.genUndefinedDeclarationStmt(@$, $2, $3); }

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
                                                                          { driver.openScope(); driver.genVariableDefinition(@6, $3, $4, $6); }
                               compound_statement                         { $$ = driver.genForStmt(@$, $3, $4, $6, $8, driver.builder.literal($3, "1"), $11); driver.closeScope(); }
              | "for" "(" type "identifier" "=" expression ".." expression ":" expression ")"
                                                                          { driver.openScope(); driver.genVariableDefinition(@6, $3, $4, $6); }
                               compound_statement                         { $$ = driver.genForStmt(@$, $3, $4, $6, $8, $10, $13); driver.closeScope(); }
              ;


// -- try / catch --

try_catch_statement : "try" compound_statement catch_clauses              { $$ = driver.builder.tryCatchStmt($2, $3); }
                    ;

catch_clauses : catch_clauses catch_clause                                { $1.push_back($2); $$ = $1; }
              |                                                           { $$ = CatchClauseList(); }
              ;

catch_clause : "catch" "(" ")" compound_statement                         { $$ = driver.builder.catchClause(VariablePtr(), $4); }
             | "catch" "(" parameter ")"                                  { driver.openScope(); driver.registerParameters(@3, toVector($3)); }
                                         compound_statement_no_scope      { $$ = driver.builder.catchClause($3, $6); driver.unregisterParameters(); driver.closeScope(); }
             ;

throw_statement : "throw" expression ";"                                  { $$ = driver.builder.throwStmt($2); }
                ;

// -- break --

break : "break" ";"                                                       { $$ = driver.builder.breakStmt(); }
      ;

// -- continue --

continue : "continue" ";"                                                 { $$ = driver.builder.continueStmt(); }
         ;

// -- return --

return : "return" expression ";"                                          { $$ = driver.builder.returnStmt(driver.getScalar($2)); }
       | "return" expression "in" type ";"                                { $$ = driver.builder.returnStmt(driver.getScalar($2), $4); }
       | "return" ";"                                                     { $$ = driver.builder.returnStmt(); }
       ;


// -- let --

let_statement : "let" "identifier" "=" expression ";"                     {  driver.declareSymbol(@2, $2, $4); $$ = $4; }
              ;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// the lower in this list, the higher the precedence


%right    "in";
%left     ":";
%nonassoc ")";

%right    "=>";
%left     "spawn" "sync" "sync_all";

%right    "=";
%right    "?";
%left     "||";
%left     "&&";
%left     "|";
%left     "^";
%left     "&";
%left     "==" "!=";
%left     "<" "<=" ">" ">=";
%left     "<<";
%left     "+" "-";
%left     "*" "/" "%";
%right    UDEREF UNOT UMINUS CAST;
%left     "->" "." "[" "(";

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


