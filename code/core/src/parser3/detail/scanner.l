%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/parser3/detail/scanner.h"
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
%}


%option noyywrap c++ batch nounput
%option yyclass="inspire_scanner"
%option noyywrap
/*%option prefix="inspire_"*/
/*nounput batch debug noinput */
/*reentrant bison-bridge noyywrap */

id       [a-zA-Z_][a-zA-Z0-9_]*
type_var [\'][a-zA-Z0-9_]*

/* Note: the original stringlit regex was: \"(\\.|[^"])*\"
 * We had to create an alternative one which puts the strin in double quotes on each side, in order to enable the parsing of
 * string literals (which are real strings - not real values or so) and also pass on the enclosing quotes to the builder.
 */
stringlit \"(\\.|[^"])*\"|\"(\"(\\.|[^"])*\")\"
charlit   \'(\\.|[^"])\'
float   [0-9]+"."[0-9]+f
double  [0-9]+"."[0-9]+
ulong   [0-9]+ul
long    [0-9]+l
longlong [0-9]+ll
ulonglong [0-9]+ull
uint    [0-9]+u
int     [0-9]+
blank   [ \t]


%{
  // Code run each time a pattern is matched.
  # define YY_USER_ACTION  loc.columns (yyleng);
  using namespace insieme::core::parser3::detail;
%}

%%

%{
  // Code run each time yylex is called.

  // this allows the use of DUMMY start symbols to parse subtrees of the grammar without conflicst  
  //  http://www.gnu.org/software/bison/manual/html_node/Multiple-start_002dsymbols.html
  //  There is somehow something different, start_token is passed to the lexer from the parser and is never modified,
  //  I  use a double ptr to alter the original value
  if (start_token){
      auto t = start_token;
      start_token = nullptr;
      return *t;
  }

  // update location;
  loc.step ();
%}

"//"+.*    loc.step ();
{blank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();
"-"      return inspire_parser::make_MINUS(loc);
"+"      return inspire_parser::make_PLUS(loc);
"*"      return inspire_parser::make_STAR(loc);
"/"      return inspire_parser::make_SLASH(loc);
"%"      return inspire_parser::make_PERCENT(loc);
"#"      return inspire_parser::make_HASH(loc);

"("      return inspire_parser::make_LPAREN(loc);
")"      return inspire_parser::make_RPAREN(loc);
"{"      return inspire_parser::make_LCURBRACKET(loc);
"}"      return inspire_parser::make_RCURBRACKET(loc);
"["      return inspire_parser::make_LBRACKET(loc);
"]"      return inspire_parser::make_RBRACKET(loc);
"\""     return inspire_parser::make_DQUOTE(loc);
"\'"     return inspire_parser::make_QUOTE(loc);

"&"     return inspire_parser::make_BAND(loc);
"|"     return inspire_parser::make_BOR(loc);
"^"     return inspire_parser::make_BXOR(loc);

"&&"     return inspire_parser::make_LAND(loc);
"||"     return inspire_parser::make_LOR(loc);
"!"      return inspire_parser::make_LNOT(loc);

"<"      return inspire_parser::make_LT(loc);
">"      return inspire_parser::make_GT(loc);
"<="     return inspire_parser::make_LEQ(loc);
">="     return inspire_parser::make_GEQ(loc);
"=="     return inspire_parser::make_EQ(loc);
"!="     return inspire_parser::make_NEQ(loc);


"="      return inspire_parser::make_ASSIGN(loc);
"?"      return inspire_parser::make_QMARK(loc);
"::"     return inspire_parser::make_NAMESPACE(loc);
"~"      return inspire_parser::make_FUNNY_BOY(loc);
":"      return inspire_parser::make_COLON(loc);

"->"     return inspire_parser::make_ARROW(loc);
"=>"     return inspire_parser::make_DARROW(loc);

";"      return inspire_parser::make_SEMIC(loc);
","      return inspire_parser::make_COMA(loc);
".."     return inspire_parser::make_RANGE(loc);
"."      return inspire_parser::make_DOT(loc);
"$"      return inspire_parser::make_ADDRESS(loc);

"CAST("     return inspire_parser::make_CAST(loc);
".as("      return inspire_parser::make_PARENT(loc);
"#inf"      return inspire_parser::make_INFINITE(loc);
"let"       return inspire_parser::make_LET(loc);
"in"        return inspire_parser::make_IN(loc);
"using"     return inspire_parser::make_USING(loc);
"auto"      return inspire_parser::make_AUTO(loc);
"lambda"    return inspire_parser::make_LAMBDA(loc);
"function"  return inspire_parser::make_FUNCTION(loc);
"ctor"      return inspire_parser::make_CTOR(loc);
"method"    return inspire_parser::make_METHOD(loc);
"expr"      return inspire_parser::make_EXPR(loc);
"recFunc"   return inspire_parser::make_RECFUNC(loc);

"if"      return inspire_parser::make_IF(loc);
"else"    return inspire_parser::make_ELSE(loc);
"for"     return inspire_parser::make_FOR(loc);
"while"   return inspire_parser::make_WHILE(loc);
"decl"    return inspire_parser::make_DECL(loc);
"try"     return inspire_parser::make_TRY(loc);
"throw"   return inspire_parser::make_THROW(loc);
"catch"   return inspire_parser::make_CATCH(loc);
"return"  return inspire_parser::make_RETURN(loc);
"switch"  return inspire_parser::make_SWITCH(loc);
"case"    return inspire_parser::make_CASE(loc);
"default" return inspire_parser::make_DEFAULT(loc);
"continue" return inspire_parser::make_CONTINUE(loc);
"break"   return inspire_parser::make_BREAK(loc);

"var"     return inspire_parser::make_VAR(loc);
"new"     return inspire_parser::make_NEW(loc);
"loc"     return inspire_parser::make_LOC(loc);
"delete"     return inspire_parser::make_DELETE(loc);
"undefined"  return inspire_parser::make_UNDEFINED(loc);

"type_lit("    return inspire_parser::make_TYPE_LITERAL(loc);
"lit("     return inspire_parser::make_LITERAL(loc);
"param("   return inspire_parser::make_PARAM(loc);

"true"    return inspire_parser::make_BOOL(yytext, loc);
"false"   return inspire_parser::make_BOOL(yytext, loc);
"struct"  return inspire_parser::make_STRUCT(loc);
"union"   return inspire_parser::make_UNION(loc);

"spawn"   return inspire_parser::make_SPAWN(loc);
"sync"    return inspire_parser::make_SYNC(loc);
"syncAll" return inspire_parser::make_SYNCALL(loc);
"job"     return inspire_parser::make_JOB(loc);
"task"     return inspire_parser::make_TASK(loc);

{stringlit}      {
    std::string tmp(yytext);
    return inspire_parser::make_STRING(tmp, loc);
}
{charlit}      {
  return inspire_parser::make_CHAR(yytext, loc);
}
{int}      {
  return inspire_parser::make_INT(yytext, loc);
}
{uint}      {
  return inspire_parser::make_UINT(yytext, loc);
}
{long}      {
  return inspire_parser::make_LONG(yytext, loc);
}
{ulong}      {
  return inspire_parser::make_ULONG(yytext, loc);
}
{longlong}      {
  return inspire_parser::make_LONGLONG(yytext, loc);
}
{ulonglong}      {
  return inspire_parser::make_ULONGLONG(yytext, loc);
}
{float}      {
  return inspire_parser::make_FLOAT(yytext, loc);
}
{double}      {
  return inspire_parser::make_DOUBLE(yytext, loc);
}

{id}       return inspire_parser::make_IDENTIFIER(yytext, loc);
{type_var}       return inspire_parser::make_TYPE_VAR(yytext+1, loc);
.          driver.error (loc, "invalid character");
<<EOF>>    return inspire_parser::make_END(loc);

%%

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace insieme{
namespace core{
namespace parser3{
namespace detail{


    inspire_parser::symbol_type yylex (inspire_driver& driver, inspire_scanner& scanner){
        return scanner.yylex(driver);
    }

} // detail namespace
} // parser3 namespace
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
