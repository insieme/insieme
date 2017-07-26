%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>

#include "insieme/core/parser/detail/driver.h"
#include "insieme/core/parser/detail/scanner.h"
#include "insieme/core/ir.h"

// this file is generated and the path is provided
# include "inspire_parser.hpp"

// Work around an incompatibility in flex (at least versions
// 2.5.31 through 2.5.33): it generates code that does
// not conform to C89.  See Debian bug 333231
// <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.
//# undef yywrap
//# define yywrap() 1


#ifdef __clang__
# pragma clang diagnostic ignored "-Wdeprecated-register"
#endif

#undef YY_DECL
#define YY_DECL InspireParser::symbol_type InspireScanner::yylex(insieme::core::parser::detail::InspireDriver& driver)
%}


%option noyywrap c++ batch nounput
%option yyclass="InspireScanner"
%option noyywrap
/*%option prefix="inspire_"*/
/*nounput batch debug noinput */
/*reentrant bison-bridge noyywrap */

id            [a-zA-Z_][a-zA-Z0-9_]*
type_var      [\'][a-zA-Z0-9_]*
tag_ref       [\^][a-zA-Z0-9_]*

/* Note: the original string regex was: \"(\\.|[^"])*\"
 * We had to create an alternative one which puts the string in double quotes on each side, in order to enable the parsing of
 * string literals (which are real strings - not real values or so) and also pass on the enclosing quotes to the builder.
 *
 * Some corner cases for float and double literals are not covered here, as they result in wrong parsing combined with the
 * member access usage of '.'
 */
string        \"(\\.|[^"])*\"|\"(\"(\\.|[^"])*\")\"
char          [\'][\\a-zA-Z0-9_][a-zA-Z0-9_]?[\']
float         ((0|[1-9][0-9]*)f)|((0|[1-9][0-9]*)"."[0-9]+([eE][+\-]?[0-9]+)?f)
double        ((0|[1-9][0-9]*)d)|((0|[1-9][0-9]*)"."[0-9]+([eE][+\-]?[0-9]+)?d?)
ulonglong     (0|([1-9][0-9]*))ull
longlong      (0|([1-9][0-9]*))ll
ulong         (0|([1-9][0-9]*))ul
long          (0|([1-9][0-9]*))l
uint          (0|([1-9][0-9]*))u
int           (0|([1-9][0-9]*))
blank         [ \t]


%{
	// Code run each time a pattern is matched.
	# define YY_USER_ACTION  loc.columns (yyleng);
	using namespace insieme::core::parser::detail;
%}

%%

%{
	// Code run each time yylex is called.

	// this allows the use of DUMMY start symbols to parse subtrees of the grammar without conflicst
	//  http://www.gnu.org/software/bison/manual/html_node/Multiple-start_002dsymbols.html
	//  There is somehow something different, start_token is passed to the lexer from the parser and is never modified,
	//  I  use a double ptr to alter the original value
	if (startToken){
		auto t = startToken;
		startToken = nullptr;
		return *t;
	}

	// update location;
	loc.step ();
%}

"//"+.*       loc.step ();							// skip single-line comments
"/*"([^*]|(\*+[^*/]))*\*+\/      loc.step ();		// skip multi-line comments
{blank}+      loc.step ();
[\n]+         loc.lines (yyleng); loc.step ();
"-"           return InspireParser::make_MINUS(loc);
"+"           return InspireParser::make_PLUS(loc);
"*"           return InspireParser::make_STAR(loc);
"/"           return InspireParser::make_SLASH(loc);
"%"           return InspireParser::make_PERCENT(loc);
"#"           return InspireParser::make_HASH(loc);
"<<"          return InspireParser::make_LSHIFT(loc);

"("           return InspireParser::make_LPAREN(loc);
")"           return InspireParser::make_RPAREN(loc);
"{"           return InspireParser::make_LCURBRACKET(loc);
"}"           return InspireParser::make_RCURBRACKET(loc);
"["           return InspireParser::make_LBRACKET(loc);
"]"           return InspireParser::make_RBRACKET(loc);

"&"           return InspireParser::make_BAND(loc);
"|"           return InspireParser::make_BOR(loc);
"^"           return InspireParser::make_BXOR(loc);

"&&"          return InspireParser::make_LAND(loc);
"||"          return InspireParser::make_LOR(loc);
"!"           return InspireParser::make_LNOT(loc);

"<"           return InspireParser::make_LT(loc);
">"           return InspireParser::make_GT(loc);
"<="          return InspireParser::make_LEQ(loc);
">="          return InspireParser::make_GEQ(loc);
"=="          return InspireParser::make_EQ(loc);
"!="          return InspireParser::make_NEQ(loc);


"="           return InspireParser::make_ASSIGN(loc);
"?"           return InspireParser::make_QMARK(loc);
"::"          return InspireParser::make_NAMESPACE(loc);
"~"           return InspireParser::make_FUNNY_BOY(loc);
":"           return InspireParser::make_COLON(loc);

"->"          return InspireParser::make_ARROW(loc);
"=>"          return InspireParser::make_DARROW(loc);
"~>"          return InspireParser::make_TARROW(loc);

";"           return InspireParser::make_SEMIC(loc);
","           return InspireParser::make_COMA(loc);
"..."         return InspireParser::make_ELLIPSIS(loc);
".."          return InspireParser::make_RANGE(loc);
"."           return InspireParser::make_DOT(loc);
"$"           return InspireParser::make_ADDRESS(loc);

"CAST"        return InspireParser::make_CAST(loc);
"as"          return InspireParser::make_PARENT(loc);
"let"         return InspireParser::make_LET(loc);
"in"          return InspireParser::make_IN(loc);
"using"       return InspireParser::make_USING(loc);
"alias"       return InspireParser::make_ALIAS(loc);
"auto"        return InspireParser::make_AUTO(loc);
"function"    return InspireParser::make_FUNCTION(loc);
"lambda"      return InspireParser::make_LAMBDA(loc);
"ctor"        return InspireParser::make_CTOR(loc);
"dtor"        return InspireParser::make_DTOR(loc);
"lit"         return InspireParser::make_LIT(loc);
"type_lit"    return InspireParser::make_TYPE_LIT(loc);
"this"        return InspireParser::make_THIS(loc);

"if"          return InspireParser::make_IF(loc);
"else"        return InspireParser::make_ELSE(loc);
"for"         return InspireParser::make_FOR(loc);
"while"       return InspireParser::make_WHILE(loc);
"decl"        return InspireParser::make_DECL(loc);
"def"         return InspireParser::make_DEF(loc);
"return"      return InspireParser::make_RETURN(loc);
"switch"      return InspireParser::make_SWITCH(loc);
"case"        return InspireParser::make_CASE(loc);
"try"         return InspireParser::make_TRY(loc);
"catch"       return InspireParser::make_CATCH(loc);
"throw"       return InspireParser::make_THROW(loc);
"default"     return InspireParser::make_DEFAULT(loc);
"delete"      return InspireParser::make_DELETE(loc);
"continue"    return InspireParser::make_CONTINUE(loc);
"break"       return InspireParser::make_BREAK(loc);

"var"         return InspireParser::make_VAR(loc);
"free_var"    return InspireParser::make_FREE_VAR(loc);

"true"        return InspireParser::make_TRUE(loc);
"false"       return InspireParser::make_FALSE(loc);
"struct"      return InspireParser::make_STRUCT(loc);
"union"       return InspireParser::make_UNION(loc);

"lambda_name" return InspireParser::make_LAMBDA_NAME(loc);

"virtual"     return InspireParser::make_VIRTUAL(loc);
"pure"        return InspireParser::make_PURE(loc);
"const"       return InspireParser::make_CONST(loc);
"volatile"    return InspireParser::make_VOLATILE(loc);
"private"     return InspireParser::make_PRIVATE(loc);
"public"      return InspireParser::make_PUBLIC(loc);
"protected"   return InspireParser::make_PROTECTED(loc);

"materialize" return InspireParser::make_MATERIALIZE(loc);

"spawn"       return InspireParser::make_SPAWN(loc);
"sync"        return InspireParser::make_SYNC(loc);
"sync_all"    return InspireParser::make_SYNCALL(loc);
"job"         return InspireParser::make_JOB(loc);

{string} {
	std::string tmp(yytext);
	return InspireParser::make_STRING(tmp, loc);
}
{char}        return InspireParser::make_CHAR(yytext, loc);
{int}         return InspireParser::make_INT(yytext, loc);
{uint}        return InspireParser::make_UINT(yytext, loc);
{long}        return InspireParser::make_LONG(yytext, loc);
{ulong}       return InspireParser::make_ULONG(yytext, loc);
{longlong}    return InspireParser::make_LONGLONG(yytext, loc);
{ulonglong}   return InspireParser::make_ULONGLONG(yytext, loc);
{float}       return InspireParser::make_FLOAT(yytext, loc);
{double}      return InspireParser::make_DOUBLE(yytext, loc);

{id}          return InspireParser::make_IDENTIFIER(yytext, loc);
{type_var}    return InspireParser::make_TYPE_VAR(yytext+1, loc);
{tag_ref}     return InspireParser::make_TAG_REF(yytext+1, loc);
.             driver.error (loc, "invalid character");
<<EOF>>       return InspireParser::make_END(loc);

%%

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace insieme{
namespace core{
namespace parser{
namespace detail{


	InspireParser::symbol_type yylex (InspireDriver& driver, InspireScanner& scanner){
		return scanner.yylex(driver);
	}

} // detail namespace
} // parser namespace
} // core namespace
} // insieme namespace


/* ~~~~~~~~~~~~~~~~~ UNUSED BUT REQUIRED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
int Trick_Lexer::yywrap(){
	std::cout << "this should not be called" << std::endl;
	abort();
	return 1;
}
int Trick_Lexer::yylex(){
	std::cout << "this is overloaded and should not be called" << std::endl;
	abort();
	return 0;
}
