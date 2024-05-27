%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError

#define SAFE_STOI(base) {                         \
  try {                                           \
    yylval.i = std::stoi(yytext, nullptr, base);  \
    return tINTEGER;                              \
  } catch (const std::out_of_range&) {            \
    yyerror("Integer overflow");                  \
  }                                               \
}

#define SAFE_STOD {                      \
  try {                                  \
    yylval.d = std::stod(yytext);        \
    return tDOUBLE;                      \
  } catch (const std::out_of_range&) {   \
    yyerror("Floating point overflow");  \
  }                                      \
}

%}

%x X_COMMENT X_HEX_INT X_STRING X_STRING_IGN X_BACKSLASH

%%
  yydebug=1; set_debug(1);

  /* ====================================================================== */
  /* ====[                    3.1 - WHITESPACE                        ]==== */
  /* ====================================================================== */

[ \t\n\r]+ ;

  /* ====================================================================== */
  /* ====[                    3.2 - COMMENTS                          ]==== */
  /* ====================================================================== */

";".*             ;
"/*"              yy_push_state(X_COMMENT);
<X_COMMENT>"/*"   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"   yy_pop_state();
<X_COMMENT>.|"\n" ;

  /* ====================================================================== */
  /* ====[                    3.3 - KEYWORDS                          ]==== */
  /* ====================================================================== */

"external" return tEXTERNAL;
"forward"  return tFORWARD;
"public"   return tPUBLIC;
"var"      return tVAR;

"block"    return tBLOCK;
"if"       return tIF;
"loop"     return tLOOP;
"stop"     return tSTOP;
"next"     return tNEXT;
"return"   return tRETURN;
"print"    return tPRINT;
"println"  return tPRINTLN;

"read"     return tREAD;
"null"     return tNULL;
"set"      return tSET;
"index"    return tINDEX;
"objects"  return tOBJECTS;
"sizeof"   return tSIZEOF;
"function" return tFUNCTION;

"program"  return tPROGRAM;

  /* ====================================================================== */
  /* ====[                    3.4 - TYPES                             ]==== */
  /* ====================================================================== */

"int"    return tTYPE_INT;
"double" return tTYPE_DOUBLE;
"string" return tTYPE_STRING;
"void"   return tTYPE_VOID;

  /* ====================================================================== */
  /* ====[                    FUNCTIONAL                              ]==== */
  /* ====================================================================== */

@ return *yytext;

  /* ====================================================================== */
  /* ====[                    3.5 - OPERATORS                         ]==== */
  /* ====================================================================== */

">="         return tGE;
"<="         return tLE;
"=="         return tEQ;
"!="         return tNE;
"&&"         return tAND;
"||"         return tOR;
[+?!*/%<>~-] return *yytext;

  /* ====================================================================== */
  /* ====[                    3.6 - DELIMITERS AND TERMINATORS        ]==== */
  /* ====================================================================== */

[()] return *yytext;

  /* ====================================================================== */
  /* ====[                    3.7 - IDENTIFIERS                       ]==== */
  /* ====================================================================== */

[A-Za-z][A-Za-z0-9]* yylval.s = new std::string(yytext); return tIDENTIFIER;

  /* ====================================================================== */
  /* ====[                    3.8.1 - INTEGERS                        ]==== */
  /* ====================================================================== */

"0x"                                                yy_push_state(X_HEX_INT);
<X_HEX_INT>0+                                       yyerror("Invalid hexadecimal number");
<X_HEX_INT>[[:xdigit:]]+                            yy_pop_state(); SAFE_STOI(16);
<X_HEX_INT>[[:xdigit:]]*[G-Zg-z]+[[:xdigit:]]*|.|\n yyerror("Invalid hexadecimal number");

0|[1-9][0-9]* SAFE_STOI(10);
0[0-9]+       yyerror("Invalid decimal number");

  /* ====================================================================== */
  /* ====[                    3.8.2 - FLOATING POINT NUMBERS          ]==== */
  /* ====================================================================== */

[0-9]+[eE][+-]?[0-9]+                             SAFE_STOD;
([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? SAFE_STOD;

  /* ====================================================================== */
  /* ====[                    3.8.3 - STRINGS                         ]==== */
  /* ====================================================================== */

\"                           yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"                 yy_pop_state(); return tSTRING;
<X_STRING>\\                 yy_push_state(X_BACKSLASH);
<X_STRING>\0                 yyerror("Null byte in string");
<X_STRING>\n                 yyerror("Newline in string");
<X_STRING>.                  *yylval.s += yytext;

<X_STRING_IGN>\"             yy_pop_state(); yy_pop_state(); return tSTRING;
<X_STRING_IGN>\0             yyerror("Null byte in string");
<X_STRING_IGN>\n             yyerror("Newline in string");
<X_STRING_IGN>.|\\\"|\\\\    ;

<X_BACKSLASH>n               yy_pop_state(); *yylval.s += '\n';
<X_BACKSLASH>r               yy_pop_state(); *yylval.s += '\r';
<X_BACKSLASH>t               yy_pop_state(); *yylval.s += '\t';
<X_BACKSLASH>0{1,3}          yy_pop_state(); yy_push_state(X_STRING_IGN);
<X_BACKSLASH>[0-7]{1,3}      {
                               int i = std::stoi(yytext, NULL, 8);
                               if (i > 255) yyerror("Special character overflow"); /* max is 255 */
                               *yylval.s += (char) i;
                               yy_pop_state();
                             }
<X_BACKSLASH>.               yy_pop_state(); *yylval.s += yytext;

  /* ====================================================================== */
  /* ====[                    EVERYTHING ELSE                         ]==== */
  /* ====================================================================== */

. yyerror("Unknown character");

%%
