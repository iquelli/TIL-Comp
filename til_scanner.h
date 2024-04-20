#ifndef __TILSCANNER_H__
#define __TILSCANNER_H__

#undef yyFlexLexer
#define yyFlexLexer til_scanner_FlexLexer
#include <FlexLexer.h>

typedef til_scanner_FlexLexer til_scanner;

#endif
