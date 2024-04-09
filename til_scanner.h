#ifndef __SIMPLESCANNER_H__
#define __SIMPLESCANNER_H__

#undef yyFlexLexer
#define yyFlexLexer til_scanner_FlexLexer
#include <FlexLexer.h>

typedef til_scanner_FlexLexer til_scanner;

#endif
