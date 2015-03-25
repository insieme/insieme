%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"

// this file is generated and the path is provided
# include "inspire_parser.hpp"

// Work around an incompatibility in flex (at least versions
// 2.5.31 through 2.5.33): it generates code that does
// not conform to C89.  See Debian bug 333231
// <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.
# undef yywrap
# define yywrap() 1

// The location of the current token.
static insieme::core::parser3::detail::location loc;
%}


%option noyywrap nounput batch debug noinput


id      [a-zA-Z_][a-zA-Z0-9_]*

stringlit \"(\\.|[^"])*\"
charlit   \'(\\.|[^"])\'
float   [0-9]*"."[0-9]+f
double  [0-9]*"."[0-9]+
ulong   [0-9]+ul
long    [0-9]+l
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
  if (*start_token){
      auto t = *start_token;
      *start_token = nullptr;
      return *t;
  }

  // update location;
  loc.step ();
%}

{blank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();
"-"      return inspire_parser::make_MINUS(loc);
"+"      return inspire_parser::make_PLUS(loc);
"*"      return inspire_parser::make_STAR(loc);
"/"      return inspire_parser::make_SLASH(loc);

"("      return inspire_parser::make_LPAREN(loc);
")"      return inspire_parser::make_RPAREN(loc);
"{"      return inspire_parser::make_LCURBRACKET(loc);
"}"      return inspire_parser::make_RCURBRACKET(loc);
"["      return inspire_parser::make_LBRACKET(loc);
"]"      return inspire_parser::make_RBRACKET(loc);
"\""     return inspire_parser::make_DQUOTE(loc);
"\'"     return inspire_parser::make_QUOTE(loc);

"<"      return inspire_parser::make_LT(loc);
">"      return inspire_parser::make_GT(loc);
"<="     return inspire_parser::make_LEQ(loc);
">="     return inspire_parser::make_GEQ(loc);
"=="     return inspire_parser::make_EQ(loc);
"!="     return inspire_parser::make_NEQ(loc);

"!"      return inspire_parser::make_LNOT(loc);

"="      return inspire_parser::make_ASSIGN(loc);
"?"      return inspire_parser::make_QMARK(loc);
"::"     return inspire_parser::make_NAMESPACE(loc);
":"      return inspire_parser::make_COLON(loc);

"->"     return inspire_parser::make_ARROW(loc);
"=>"     return inspire_parser::make_DARROW(loc);

";"      return inspire_parser::make_SEMIC(loc);
","      return inspire_parser::make_COMA(loc);
".."     return inspire_parser::make_RANGE(loc);
"."      return inspire_parser::make_DOT(loc);

"let"    return inspire_parser::make_LET(loc);
"auto"   return inspire_parser::make_AUTO(loc);
"if"     return inspire_parser::make_IF(loc);
"else"   return inspire_parser::make_ELSE(loc);
"for"    return inspire_parser::make_FOR(loc);
"while"  return inspire_parser::make_WHILE(loc);
"return" return inspire_parser::make_RETURN(loc);
"true"   return inspire_parser::make_BOOL(yytext, loc);
"false"   return inspire_parser::make_BOOL(yytext, loc);



{stringlit}      {
  return inspire_parser::make_STRING(yytext, loc);
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

{float}      {
  return inspire_parser::make_FLOAT(yytext, loc);
}
{double}      {
  return inspire_parser::make_DOUBLE(yytext, loc);
}

{id}       return inspire_parser::make_IDENTIFIER(yytext, loc);
.          driver.error (loc, "invalid character");
<<EOF>>    return inspire_parser::make_END(loc);

%%

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace insieme{
namespace core{
namespace parser3{
namespace detail{

void scanner_stdin::scan_begin(){
    yyin=stdin;
}
void scanner_stdin::scan_end(){
}

void scanner_string::scan_begin(){
    yy_flex_debug = 0;
    yy_scan_string(str.c_str());
}
void scanner_string::scan_end(){
    yylex_destroy();
    loc.initialize();
}

void scanner_file::scan_begin(){
    yy_flex_debug = 0;
    if (!(yyin = fopen (file.c_str (), "r")))
    {
      driver->error ("cannot open " + file + ": " + strerror(errno));
      exit (EXIT_FAILURE);
    }
    loc.initialize();
}

void scanner_file::scan_end(){
    fclose (yyin);
    loc.initialize();
}

} // detail namespace
} // parser3 namespace
} // core namespace
} // insieme namespace
