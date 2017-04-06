%{
#include <stdio.h>
#include <string.h>
#include "parseStructs.h"
#include "cmgparse.tab.h"

  int lineNumber = 1;

  /* function to perform houskeeping when we get to a new line */
  void newLine();

  /*rule for floats, put here because lex doesn't understand comments */
/*-?(([0-9]+)|([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?)     yylval.doubleVal=atof(yytext); return NUMBER; */

%}

%%

seed                    return SEEDTOK;
sms                     return SMSTOK;
blk                     return BLKTOK;
on|off                  yylval.intVal=!strcmp(yytext,"on"); return STATE;
numzones                return NUMZONESTOK;
sub                     return SUBTOK;
tag                     return TAGTOK;
node|edge|face|zone     yylval.strVal=strdup((const char *)yytext); return MESHTAGTYPE;
material                return MATERIALTOK;
nb|fb                   yylval.intVal=!strcmp(yytext,"nb"); return INDEXMODE;
hex|pyr|pri             yylval.intVal=((!strcmp(yytext,"hex")) ? 0 : ((!strcmp(yytext,"pyr")) ? 1 : 2)); return SUBTYPE;
[0-9]+                  yylval.intVal=atoi(yytext); return NUMBER;


[a-zA-Z][a-zA-Z0-9]*    yylval.strVal=strdup(yytext); return WORD;
\"[a-zA-Z0-9_]+\"       { yylval.strVal=strdup(yytext+1); /*Remove open quote*/ 
                          if(yylval.strVal[yyleng-2] != '"') 
			    yyerror("Unterminated name");
			  else 
			    yylval.strVal[yyleng-2] = '\0'; /* Remove close quote*/ 
			  return NAME; }
#.*\n                   yylval.strVal=strdup(yytext); newLine(); return COMMENT;
\n                      newLine();/* ignore EOL */;
[ \t]+                  /* ignore whitespace */;


.                       return yytext[0];/* And now, a catch-all rule to handle 
					    single characters, like commas,
					    quotes, etc. */
%%
int yyerror(const char *msg)
{
  fprintf(stderr,"Line #%d: \"%s\" at '%s'\n",lineNumber, msg, yytext);
  return 1;
}


void newLine(void) {
  lineNumber++;
  /* printf("CMG > "); */
}
