%skeleton "lalr1.cc" /* -*- C++ -*- */
/* glr.cc does not support type variants, 
 * there is work in process to suport it, 
 * lets wait until 2016 and then we can use the $#%& ambigous grammar again
 */
%require "3.0.0"
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

    #include <utility> 
    #include <string>
    #include <iostream>
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
	
	#include "insieme/core/lang/parallel.h"

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
  HASH 	  "#"

  LPAREN       "("
  RPAREN       ")"
  LCURBRACKET  "{"
  RCURBRACKET  "}"
  LBRACKET     "["
  RBRACKET     "]"
  DQUOTE       "\""
  QUOTE  	   "\'"

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
                                         
  QMARK      "?"
  COLON      ":"
  NAMESPACE  "::"
  FUNNY_BOY  "~"

  ARROW   "->"
  DARROW  "=>"
  TARROW  "~>"

  SEMIC   ";"
  COMA    ","
  RANGE   ".."
  DOT     "."
  ADDRESS "$"
  
  PARENT        "as"
  CAST          "CAST"
  INFINITE      "#inf"
  USING         "using"    
  AUTO          "auto"
  FUN           "fun"
  FUNCTION      "function"
  LAMBDA        "lambda"
  CTOR          "ctor"    
  METHOD        "method"    
  TYPE			"type"
  
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
  
  UNDEFINED     "undefined"

  TRUE          "true"  
  FALSE         "false"  
  STRUCT        "struct"
  UNION         "union"

  VIRTUAL       "virtual"
  PRIVATE       "private"
  PUBLIC        "public"
  PROTECTED     "protected"

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
%token <std::string> STRING 			"string"
%token <std::string> IDENTIFIER 		"identifier"
%token <std::string> TYPE_VAR   		"type_var"
%token <std::string> TAG_REF   			"tag_ref"
%token <std::string> BOOL 				"bool"
%token <std::string> INT 				"int"
%token <std::string> UINT 				"uint"
%token <std::string> LONG 				"long"
%token <std::string> ULONG 				"ulong"
%token <std::string> LONGLONG 			"longlong"
%token <std::string> ULONGLONG 			"ulonglong"
%token <std::string> FLOAT 				"float"
%token <std::string> DOUBLE 			"double"

%type  <std::string> "Number" 
%type  <std::string> "indentifier" 

%type <NodeList> top_level

%type <TypePtr>                        type plain_type let_type
%type <TypeList>                       types non_empty_types abstract_param_list
%type <ExpressionPtr>                  expression plain_expression let_expression
%type <ExpressionList>                 expressions non_empty_expressions
%type <StatementPtr>                   statement plain_statement let_statement
%type <NodePtr>                        definition
%type <NodeList>                       definitions
                                       
%type <NodePtr>                        record_definition
%type <NodePtr>                        function_definition
                                       
%type <FieldPtr>		               field
%type <FieldList>		               fields
%type <LambdaExprPtr>	               constructor
%type <LambdaExprList>	               constructors
%type <LambdaExprPtr>	               destructor
%type <MemberFunctionPtr>	           member_function
%type <MemberFunctionList>	           member_functions
%type <PureVirtualMemberFunctionPtr>   pure_virtual_member_function
%type <PureVirtualMemberFunctionList>  pure_virtual_member_functions

%type <TypePtr>						   object_type
%type <TypeVariablePtr>                type_variable
%type <GenericTypePtr>                 abstract_type
%type <FunctionTypePtr>                function_type pure_function_type closure_type constructor_type destructor_type member_function_type virtual_function_type
%type <NumericTypePtr>                 numeric_type
%type <TupleTypePtr>                   tuple_type
%type <TagTypeReferencePtr>            tag_type_reference

%type <ParentPtr>					   parent
%type <ParentList>					   parent_spec parents non_empty_parents

%type <AccessSpecifier>                access_specifier
%type <NodeType>                       struct_or_union
%type <bool>			               virtual_flag lambda_or_function


%type <VariablePtr>					   variable
%type <LiteralPtr>					   literal
%type <CallExprPtr>					   call
%type <LambdaExprPtr>                  lambda
%type <BindExprPtr>                    bind
%type <ExpressionPtr>                  undefined_expression parallel_expression list_expression initializer unary_op binary_op ternary_op

%type <VariablePtr>					   parameter
%type <VariableList>				   parameters non_empty_parameters


%type<CompoundStmtPtr>                 compound_statement    
%type<DeclarationStmtPtr>              variable_declaration  
%type<IfStmtPtr>                       if_statement          
%type<SwitchStmtPtr>                   switch_statement      
%type<WhileStmtPtr>                    while_statement       
%type<ForStmtPtr>                      for_statement         
%type<BreakStmtPtr>                    break                 
%type<ContinueStmtPtr>                 continue              
%type<ReturnStmtPtr>                   return 
		
%type <StatementList>				   statement_list
%type <SwitchCasePtr>				   switch_case
%type <SwitchCaseList>				   switch_cases
%type <StatementPtr>				   default_case

%printer { yyoutput << $$; } <std::string>


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
%%


%start top_level;

           
//	-- top_level -------------------------------------


top_level : aliases declarations definitions                              { $$ = $3; };
           
top_level_seperator : ";" | ;

aliases : alias top_level_seperator aliases 
        | 
		;

alias : "alias" type "=" type 
      ;

declarations : declaration top_level_seperator declarations
			 | 															
			 ;

declaration : "decl" struct_or_union "identifier"
			| "decl" "identifier" ":" type
			| "decl" "identifier" "::" "identifier" ":" type
			;


definitions : definition top_level_seperator definitions				  { $1.push_back($3); $$ = $1; } 
			|															  { $$ = NodeList(); } 
			;

definition : record_definition		 
		   | function_definition 
		   ;


//	-- record_declarations -------------------------------------

record_definition : struct_or_union "identifier" parent_spec "{" fields constructors destructor member_functions pure_virtual_member_functions "}"
																		  { $$ = NodePtr(); }
				  ;                                                       
                                                                          
struct_or_union : "struct"												  { $$ = NT_Struct; } 
				| "union" 												  { $$ = NT_Union; }
				;                                                         
                                                                          
fields : fields field 													  { $1.push_back($2); $$ = $1; } 
	   | 																  { $$ = FieldList(); }
	   ;                                                                  
                                                                          
field : type "identifier"												  { $$ = driver.field($1, $2); } 
	  ;                                                                   
                                                                          
constructors : constructors constructor									  { $1.push_back($2); $$ = $1; } 
			 | 															  { $$ = LambdaExprList(); } 
			 ;                                                            
                                                                          
constructor : "ctor" "(" parameters ")" compound_statement				  { $$ = LambdaExprPtr(); }
			;                                                             
                                                                          
destructor : "dtor" "(" ")" compound_statement							  { $$ = LambdaExprPtr(); }
		   ;                                                              
                                                                          
member_functions : member_functions member_function						  { $1.push_back($2); $$ = $1; } 
				 |  													  { $$ = MemberFunctionList(); }
				 ;                                                        
                                                                          
member_function : virtual_flag function_definition						  { $$ = MemberFunctionPtr(); } 
				;                                                         
                                                                          
virtual_flag : "virtual" 												  { $$ = true; }
			 | 															  { $$ = false; }
			 ;

pure_virtual_member_functions : pure_virtual_member_functions pure_virtual_member_function		{ $1.push_back($2); $$=$1; } 
							  |  																{ $$ = PureVirtualMemberFunctionList(); }
							  ;

pure_virtual_member_function : "pure" "virtual" "identifier" ":" pure_function_type 			{ $$ = PureVirtualMemberFunctionPtr(); }
							 ;


//	-- function_declarations -------------------------------------

function_definition : lambda_or_function "identifier" "=" lambda		  { $$ = $4; } 
					;                                                     
                                                                          
lambda_or_function : "lambda"											  { $$ = true; } 
				   | "function" 										  { $$ = false; }
				   ;                                                      
                                                                          
                                                                          
//	-- types -------------------------------------------                  
                                                                          
type : plain_type                                                         { $$ = $1; }
	 | "$" plain_type "$"                                                 { $$ = $2; }
	 | let_type													          { $$ = $1; }
	 ;                                                                    
				                                                          
plain_type : object_type        									      { $$ = $1; }
	       | function_type        									      { $$ = $1; }
	       | numeric_type         									      { $$ = $1; }
	       | tuple_type           									      { $$ = $1; }
	       ;	                                                          
                                                                          
                                                                          
object_type : type_variable                                               { $$ = $1; }
		    | abstract_type                                               { $$ = $1; }
		    | tag_type_reference                                          { $$ = $1; }
	 	    ;                                                             
		                                                                  
types : non_empty_types                                                   { $$ = $1; } 
	  | 				                                                  { $$ = TypeList(); }
	  ;                                                                   
                                                                          
non_empty_types : non_empty_types "," type								  { $1.push_back($3); $$ = $1; } 
				| type 													  { $$ = toVector<TypePtr>($1); }
				;                                                         
	                                                                      
                                                                          
// -- type variable --                                                    
                                                                          
type_variable : "type_var"												  { $$ = driver.builder.typeVariable($1); } 
			  ;                                                           
                                                                          
// -- abstract type --                                                    
                                                                          
abstract_type : "identifier" parent_spec abstract_param_list			  { $$ = driver.builder.genericType($1,$2,$3); }
			  ;                                                           
                                                                          
abstract_param_list : 						                              { $$ = TypeList(); }
					| "<" types ">"  		                              { $$ = $2; } 
					;                                                     
                                                                          
parent_spec : 								                              { $$ = ParentList(); }
			| ":" "[" parents "]" 			                              { $$ = $3; }
			;                                                             
			                                                              
parents :																  { $$ = ParentList(); } 
		| non_empty_parents 											  { $$ = $1; }
		;                                                                 
                                                                          
non_empty_parents : non_empty_parents "," parent						  { $1.push_back($3); $$ = $1; } 
				  | parent 												  { $$ = toVector($1); }
				  ;                                                       
                                                                          
parent : virtual_flag access_specifier abstract_type					  { $$ = ParentPtr(); } 
	   ;                                                                  
					                                                      
access_specifier : "public"												  { $$ =  AS_PUBLIC; }
				 | "protected" 											  { $$ =  AS_PROTECTED; }
				 | "private" 											  { $$ =  AS_PRIVATE; }
				 | 														  { $$ =  AS_PUBLIC; }
				 ;                                                        
                                                                          
                                                                          
// -- function type --                                                    
                                                                          
function_type : pure_function_type                                        
			  | closure_type                                              
			  | constructor_type                                          
			  | destructor_type                                           
			  | member_function_type                                      
			  | virtual_function_type                                     
			  ;                                                           
                                                                          
pure_function_type : "(" ")" "->" type									  { $$ = FunctionTypePtr(); }
				   | "(" non_empty_types ")" "->" type					  { $$ = FunctionTypePtr(); } 
				   ;                                                      
                                                                          
closure_type : "(" ")" "=>" type									      { $$ = FunctionTypePtr(); }
			 | "(" non_empty_types ")" "=>" type						  { $$ = FunctionTypePtr(); } 
			 ;                                                            
                                                                          
constructor_type : object_type "::" "(" types ")"						  { $$ = FunctionTypePtr(); }
				 ;                                                        
                                                                          
destructor_type : "~" object_type "::" "(" ")" 							  { $$ = FunctionTypePtr(); } 
				;                                                         
	                                                                      
member_function_type : object_type "::" "(" types ")" "->" type 		  { $$ = FunctionTypePtr(); } 
					 ;                                                    
                                                                          
virtual_function_type : object_type "::" "(" types ")" "~>" type 		  { $$ = FunctionTypePtr(); } 
					  ;                                                   
                                                                          
// -- numeric type --                                                     
                                                                          
numeric_type : "int"													  { $$ = NumericTypePtr(); } 
			 | "#" "identifier" 										  { $$ = NumericTypePtr(); }
			 ;                                                            
                                                                          
// -- tuple types --                                                      
                                                                          
tuple_type : "(" ")"													  { $$ = TupleTypePtr(); }
		   | "(" non_empty_types ")"									  { $$ = TupleTypePtr(); } 
		   ;                                                              
                                                                          
// -- tag reference --                                                    
                                                                          
tag_type_reference : "tag_ref"											  { $$ = TagTypeReferencePtr(); } 
				   ;                                                      
                                                                          
// -- let --                                                              
                                                                          
let_type : "let" "identifier" "=" type "in" type						  { $$ = $6; }
         ;                                                                
                                                                          
                                                                          
//	-- expressions -------------------------------------                  
                                                                          
expression : plain_expression											  { $$ = $1; } 
		   | "$" plain_expression "$"									  { $$ = $2; }
		   | let_expression										          { $$ = $1; }
		   ;                                                              
                                                                          
expressions : non_empty_expressions										  { $$ = $1; }
			|  															  { $$ = ExpressionList(); }
			;                                                             
                                                                          
non_empty_expressions : non_empty_expressions "," expression			  { $1.push_back($3); $$ = $1; } 
					  | expression 										  { $$ = toVector($1); }
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
				 ;                                                        
                                                                          
				                                                          
// -- variable --                                                         
                                                                          
variable : "identifier"													  { $$ = VariablePtr(); } 
		 ;                                                                
                                                                          
                                                                          
// -- literal --                                                          
                                                                          
literal : "true"                                                          { $$ = driver.builder.boolLiteral(true); }
		| "false"                                                         { $$ = driver.builder.boolLiteral(false); }
		| "int"                                                           { $$ = LiteralPtr(); }
		| "uint"                                                          { $$ = LiteralPtr(); }
		| "long"                                                          { $$ = LiteralPtr(); }
		| "ulong"                                                         { $$ = LiteralPtr(); }
		| "longlong"                                                      { $$ = LiteralPtr(); }
		| "ulonglong"                                                     { $$ = LiteralPtr(); }
		| "float"                                                         { $$ = LiteralPtr(); }
		| "double"                                                        { $$ = LiteralPtr(); }
		| "string"                                                        { $$ = LiteralPtr(); }
		| "lit" "(" "string" ":" type ")"                                 { $$ = LiteralPtr(); }
		| "type" "(" type ")"                                             { $$ = LiteralPtr(); }
		;                                                                 
	                                                                      
		                                                                  
// -- call --                                                             
		                                                                  
call : expression "(" expressions ")"									  { $$ = driver.builder.callExpr($1,$3); } 
	 ;                                                                    
                                                                          
                                                                          
// -- lambda --                                                           
                                                                          
lambda : "(" ")" "->" type compound_statement							  { $$ = LambdaExprPtr(); }
	   | "(" non_empty_parameters ")" "->" type compound_statement		  { $$ = LambdaExprPtr(); } 
	   ;                                                                  
                                                                          
parameters : non_empty_parameters										  { $$ = $1; }
		   |  															  { $$ = VariableList(); }
		   ;                                                              
                                                                          
non_empty_parameters : non_empty_parameters "," parameter				  { $1.push_back($3); $$ = $1; } 
					 | parameter 										  { $$ = toVector($1); }
					 ;                                                    
                                                                          
parameter : "identifier" ":" type 										  { $$ = VariablePtr(); } 
		  ;                                                               
                                                                          
                                                                          
// -- bind --                                                             
                                                                          
bind : "(" ")" "=>" expression											  { $$ = BindExprPtr(); }
     | "(" non_empty_parameters ")" "=>" expression						  { $$ = BindExprPtr(); } 
	 ;                                                                    
                                                                          
                                                                          
// -- let --                                                              
                                                                          
let_expression : "let" "identifier" "=" expression "in" expression		  { $$ = $6; }
               ;                                                          
                                                                          
// -- reference expressions --                                            
                                                                          
undefined_expression : "undefined" "(" type ")"                           { $$ = ExpressionPtr(); }
					 ;
					 
// -- parallel expressions --

parallel_expression : "job" "[" expression "-" expression "]" "=>" expression        
																	      { $$ = ExpressionPtr(); }
					| "job" "[" "]" "=>" expression         		      { $$ = ExpressionPtr(); }
					| "spawn" expression                                  { $$ = ExpressionPtr(); }
					| "sync" expression                                   { $$ = ExpressionPtr(); }
					| "sync_all"                                          { $$ = ExpressionPtr(); }
					;                                                     
                                                                          
                                                                          
list_expression : "[" non_empty_expressions "]"     	                  { $$ = ExpressionPtr(); }
				| "[" expressions ":" type "]"                            { $$ = ExpressionPtr(); }
				;                                                         
                                                                          
                                                                          
// -- initializer --                                                      
                                                                          
initializer : "<" type ">" "{" expressions "}"   	           		      { $$ = ExpressionPtr(); }  
			| "(" ")"												      { $$ = ExpressionPtr(); }
		    | "(" non_empty_expressions ")"                               { $$ = ExpressionPtr(); }   
			;                                                             
                                                                          
unary_op : "-" expression                                                 { $$ = ExpressionPtr(); }    %prec UMINUS
	     | "*" expression                                                 { $$ = ExpressionPtr(); }  	%prec UDEREF
		 | "!" expression                                                 { $$ = ExpressionPtr(); }    %prec UNOT
//		 | "++" expression                                                { $$ = ExpressionPtr(); }    %prec PRE_INC
//		 | "--" expression                                                { $$ = ExpressionPtr(); }    %prec PRE_DEC
//		 | expression "++"                                                { $$ = ExpressionPtr(); }    %prec POST_INC
//		 | expression "--"                                                { $$ = ExpressionPtr(); }    %prec POST_DEC
		 | expression "." "identifier"                                    { $$ = ExpressionPtr(); }    
		 | expression "." "int"                                           { $$ = ExpressionPtr(); }    
		 | expression "->" "identifier"                                   { $$ = ExpressionPtr(); }    
		 | expression "->" "int"                                          { $$ = ExpressionPtr(); }    
		 | "CAST" "(" type ")" expression                                 { $$ = ExpressionPtr(); }
		 | expression "." "as" "(" type ")"                               { $$ = ExpressionPtr(); }
		 ;                                                                
		                                                                  
binary_op : expression "=" expression                                     { $$ = ExpressionPtr(); }
		  | expression "+" expression                                     { $$ = ExpressionPtr(); }
		  | expression "-" expression                                     { $$ = ExpressionPtr(); }
		  | expression "*" expression                                     { $$ = ExpressionPtr(); }
		  | expression "/" expression                                     { $$ = ExpressionPtr(); }
		  | expression "%" expression                                     { $$ = ExpressionPtr(); }
		  | expression "&&" expression                                    { $$ = ExpressionPtr(); }
		  | expression "||" expression                                    { $$ = ExpressionPtr(); }
		  | expression "&" expression                                     { $$ = ExpressionPtr(); }
		  | expression "|" expression                                     { $$ = ExpressionPtr(); }
		  | expression "^" expression                                     { $$ = ExpressionPtr(); }
		  | expression "==" expression                                    { $$ = ExpressionPtr(); }
		  | expression "!=" expression                                    { $$ = ExpressionPtr(); }
		  | expression "<" expression                                     { $$ = ExpressionPtr(); }
		  | expression "<=" expression                                    { $$ = ExpressionPtr(); }
		  | expression ">=" expression                                    { $$ = ExpressionPtr(); }
		  | expression ">" expression                                     { $$ = ExpressionPtr(); }
		  | expression "[" expression "]"                                 { $$ = ExpressionPtr(); }
		  ;                                                               
		  		                                                          
ternary_op : expression "?" expression ":" expression                     { $$ = ExpressionPtr(); }
		   ;	

	 
//	-- statements --------------------------------------


statement : plain_statement											      { $$ = $1; } 
  	  	  | "$" plain_statement "$" 								      { $$ = $2; }
  	  	  | let_statement                                                 { $$ = $1; }
		  | ";"													          { $$ = StatementPtr(); }
		  ;                                                               
                                                                          
plain_statement : expression ";"									      { $$ = $1; }
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
                                                                          
compound_statement : "{" statement_list "}"							      { $$ = CompoundStmtPtr(); } 
				   ;                                                      
                                                                          
statement_list : 													      { $$ = StatementList(); }
			   | statement_list statement 							      { $1.push_back($2); $$ = $1; }
			   ;                                                          
                                                                          
                                                                          
// -- variable declaration --                                             
                                                                          
variable_declaration : "var" type "identifier" "=" expression ";" 		  { $$ = DeclarationStmtPtr(); }
					 | "auto" "identifier" "=" expression ";"			  { $$ = DeclarationStmtPtr(); } 
					 ;                                                    
                                                                          
                                                                          
// -- if --                                                               
                                                                          
if_statement : "if" "(" expression ")" compound_statement 			      { $$ = IfStmtPtr(); }
			 | "if" "(" expression ")" compound_statement "else" compound_statement
			 	 	 	 	 	 	 	 	 	 	 	 	 	 	      { $$ = IfStmtPtr(); }
			 ;
           

// -- switch --

switch_statement : "switch" "(" expression ")" "{" switch_cases default_case "}"   
																	      { $$ = SwitchStmtPtr(); } 
				 ;                                                        
                                                                          
switch_cases : switch_cases switch_case								      { $1.push_back($2); $$ = $1; } 
			 | 														      { $$ = SwitchCaseList(); }
			 ;                                                            
                                                                          
switch_case : "case" literal ":" statement							      { $$ = SwitchCasePtr(); } 
			;                                                             
                                                                          
default_case : "default" ":" statement								      { $$ = $3; }
			 ;                                                            
                                                                          
// -- while --                                                            
                                                                          
while_statement : "while" "(" expression ")" compound_statement           { $$ = WhileStmtPtr(); } 
				;


// -- for --

for_statement : "for" "(" type "identifier" "=" expression ".." expression ")" compound_statement
																	      { $$ = ForStmtPtr(); }
			  | "for" "(" type "identifier" "=" expression ".." expression ":" expression ")" compound_statement
				 	 	 	 	 	 	 	 	 	 	 	 	 	      { $$ = ForStmtPtr(); }			  
			  ;                                                           
                                                                          
                                                                          
// -- break --                                                            
                                                                          
break : "break" ";"							                              { $$ = BreakStmtPtr(); }
	  ;                                                                   
                                                                          
// -- continue --                                                         
                                                                          
continue : "continue" ";"					                              { $$ = ContinueStmtPtr(); }
		 ;                                                                
                                                                          
// -- return --                                                           
                                                                          
return : "return" expression ";"			                              { $$ = ReturnStmtPtr(); }
	   ;


// -- let --                                                           
                                                                          
let_statement : "let" "identifier" "=" expression ";"			          { $$ = StatementPtr(); }
	          ;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Precedence list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// the lowest in list, the highest precedence


%nonassoc "::" ;
%left ":";
%nonassoc ")";

%nonassoc UDEREF;
%nonassoc UMINUS;
%nonassoc UNOT;
%nonassoc POST_INC POST_DEC;
%nonassoc PRE_INC PRE_DEC; 
%nonassoc ".";
%nonassoc "->";   

%nonassoc "else";
%right "=>";
%left "spawn" "sync" "syncAll";
%right "?";
%right "catch";
%left "=";
%left LAMBDA;
%left "||";
%left "&&";
%left "|";
%left "^";
%left "&";
%left "==" "!=";
%left "<" "<=" ">" ">=";
%left "+" "-";
%left "*" "/" "%";
%nonassoc BOOL_OP;
%right "[";
%right "(";
%right "in";

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


