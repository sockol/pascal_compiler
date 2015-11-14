%{    /* lex2.l -- partial lex specification for Pascal    22 Jul 13 */

/* Copyright (c) 2013 Gordon S. Novak Jr. and The University of Texas at Austin
   */

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>. */

/* To use:
        make lex2
        lex2
        x < 3.14            */

/* This file will work as given on a simple input such as:
   if i<j then x := 3.14

   Copy this file to lexan.l and expand it for your assignment.
   Use:
        make lexer
        lexer <graph1.pas
   */
/*   
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
//                                LEXICAL ANALYSER                                         //
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
 
Semur Nabiev
CSID: snabiev
UTID: sn9755
CS COmpilers
Spring 2015
*/


#include <ctype.h>
#include "token.h"
#include "lexan.h" 

int maketoken(int type, int which);
int install_id();
int install_fnum();
int yywrap();

/* If the lex yytext buffer turns out to be too small to hold comments,
   add the following lines:
 #undef YYLMAX
 #define YYLMAX 256
 */
 

TOKEN yylval;     /* This is the type of the lexical analyzer result */

%}

/* regular definitions */

delim    [ \t\n]
ws       {delim}+
letter   [A-Za-z]
digit    [0-9]
id       {letter}({letter}|{digit})*
fnumber  {digit}+(\.{digit}+)?((e|E)[+\-]?{digit}+)? 
number   {digit}+?
 

comment_line   (\{[^}]*\})   
string         \'([^\n]*){0,14}\'  
 

%%

{ws}        { /* no action and no return */ }
if          {return(maketoken(RESERVED, IF));}
then        {return(maketoken(RESERVED, THEN));}
else        {return(maketoken(RESERVED, ELSE));}
begin       {return(maketoken(RESERVED, BEGINBEGIN));}
end         {return(maketoken(RESERVED, END));}
array       {return(maketoken(RESERVED, ARRAY));}  
vase        {return(maketoken(RESERVED, CASE));}
const       {return(maketoken(RESERVED, CONST));}
file        {return(maketoken(RESERVED, FILEFILE));}
do          {return(maketoken(RESERVED, DO));}
downto      {return(maketoken(RESERVED, DOWNTO));}
for         {return(maketoken(RESERVED, FOR));}
function    {return(maketoken(RESERVED, FUNCTION));}
goto        {return(maketoken(RESERVED, GOTO));}
label       {return(maketoken(RESERVED, LABEL));}
mil         {return(maketoken(RESERVED, NIL));}
of          {return(maketoken(RESERVED, OF));}
packed      {return(maketoken(RESERVED, PACKED));}
procedure   {return(maketoken(RESERVED, PROCEDURE));}
program     {return(maketoken(RESERVED, PROGRAM));}
record      {return(maketoken(RESERVED, RECORD));}
repeat      {return(maketoken(RESERVED, REPEAT));}
set         {return(maketoken(RESERVED, SET));}
to          {return(maketoken(RESERVED, TO));}
type        {return(maketoken(RESERVED, TYPE));}
until       {return(maketoken(RESERVED, UNTIL));}
var         {return(maketoken(RESERVED, VAR));}
while       {return(maketoken(RESERVED, WHILE));} 
with        {return(maketoken(RESERVED, WITH));}
in          {return(maketoken(OPERATOR, IN));}

    



{id}        {return(install_id());}
{number}     {return(install_num());}
{fnumber}   {return(install_fnum());}


"<"       {return(maketoken(OPERATOR, LT));}
"<="      {return(maketoken(OPERATOR, LE));}
"="       {return(maketoken(OPERATOR, EQ));}
"<>"      {return(maketoken(OPERATOR, NE));}
">"       {return(maketoken(OPERATOR, GT));}
">="      {return(maketoken(OPERATOR, GE));}
":="      {return(maketoken(OPERATOR, ASSIGN));}
"."       {
            if(yyleng != 2)
              return(maketoken(OPERATOR, DOT));
          }
"+"      {return(maketoken(OPERATOR, PLUS));}
"-"      {return(maketoken(OPERATOR, MINUS));}
"*"      {return(maketoken(OPERATOR, TIMES));}
"/"      {return(maketoken(OPERATOR, DIVIDE));}
 


";"       {return(maketoken(DELIMITER, SEMICOLON));}
","       {return(maketoken(DELIMITER, COMMA));}
":"       {return(maketoken(DELIMITER, COLON));}
"["       {return(maketoken(DELIMITER, LBRACKET));}
"]"       {return(maketoken(DELIMITER, RBRACKET));}
"("       {return(maketoken(DELIMITER, LPAREN));}
")"       {return(maketoken(DELIMITER, RPAREN));}
".."      {
            if(yyleng == 2)
              return(maketoken(DELIMITER, DOTDOT));
          }
 


"(*"      {       
            loop_block:
            while (input() != '*');
            switch (input()){
                      case ')': break;
                      case '*': unput('*');
                      default: goto loop_block;
                    }
            }



{comment_line} /* do nothing */ 
{string}    {return(install_string());}



%%

/* Note that lex always returns two values:
   1. "What is it?": a small integer such as NUMBER that indicates the
      kind of item that was found; this is the return() value.
   2. The value itself.  This is yylval, which points to a TOKEN.   */

 


/* Make simple tokens. */
int maketoken(int type, int which){

  yylval = (TOKEN) talloc();  /* Make new token, set yylval to point to it */
  yylval->tokentype = type;
  if (type == RESERVED)
    yylval->whichval = which - RESERVED_BIAS;

  else if (type == OPERATOR)
    yylval->whichval = which - OPERATOR_BIAS;

  else if (type == DELIMITER)
    yylval->whichval = which - DELIMITER_BIAS;
 
  else if (type == STRINGTOK)
    yylval->whichval = which; 
  
  else yylval->whichval = which;
    return(which);

 
}
/*  parse string */
int install_string(){ 
  
  int i, c, f, n;
  yylval = (TOKEN) talloc();  /* Make new token, set yylval to point to it */
  yylval->tokentype = STRINGTOK;

  n = yyleng;            /* number of characters */
  if ( n > 17 ) 
    n = 17;
   
  int counter = 0; 
  
  //increment counter all the time
  i = 1;//start by skipping first ' sign
  while(i < n-1){   

    c = yytext[i];
    f = yytext[i+1];//check for IOF. font check for this if end of string
      
    yylval->stringval[counter] = c;//insert into the string, ignoring sedond '
    counter++; 

    int isDoublequote = (c == '\'' && f == '\'');
    if(isDoublequote)     //skip over next '
      i = i+2;
    else
      i = i+1; 
  };  

  yylval->stringval[n] = '\0';
  /* printf("id  %10s   n = %4d\n", yytext, n); */
  return(STRING);
}


int install_id(){ 

  int i, c, n;
  yylval = (TOKEN) talloc();  /* Make new token, set yylval to point to it */
  yylval->tokentype = IDENTIFIERTOK;

  n = yyleng;            /* number of characters */
  if ( n > 15 ) 
    n = 15;

  for (i = 0; i < n ; i++){ 
    c = yytext[i];
    /* if (islower(c)) c = toupper(c); */ /* to raise to upper case */
    yylval->stringval[i] = c;
  };

  yylval->stringval[n] = '\0';
  /* printf("id  %10s   n = %4d\n", yytext, n); */
  return(IDENTIFIER);
 }

/*
No need to be checking for overflow on Floats because:
https://piazza.com/class/i58fnbemchi2jf?cid=81
But will check anyway
*/
int install_fnum() {  /* Note -- you may use sscanf for this assignment. */
   
  float fnum;
  long double numLong;
  long double intLimitMin = 1.175495E-38;
  long double intLimitMax = 3.402823E+38;

  yylval = (TOKEN) talloc();   /* Make new token, set yylval to point to it */
  yylval->tokentype = NUMBERTOK;
  yylval->datatype = REAL;
   
  sscanf(yytext, "%f", &fnum);
  sscanf(yytext, "%LG", &numLong);
   
  if((fnum >= intLimitMax || fnum <= intLimitMin)){// && !isinf(fnum)

    printf("Floating number out of range\n");
    yylval->realval = 0;

  }else
    yylval->realval = fnum;

  return(NUMBER);
}

//check last case. make it work with trailing integers after .
//do this through regex
int install_num() {  /* Note -- you may use sscanf for this assignment. */
  
  int num;
  long long numLong;
  unsigned int intLimit = (1<<31);//2147483648
  yylval = (TOKEN) talloc();   /* Make new token, set yylval to point to it */
  yylval->tokentype = NUMBERTOK;
  yylval->datatype = INTEGER;
     
  sscanf(yytext, "%d", &num);
  sscanf(yytext, "%lld", &numLong);
  
  if(numLong >= intLimit || yyleng > 10 || num < 0){

    printf("Integer number out of range\n");
    yylval->intval = 0;

  }else
    yylval->intval = num;

  return(NUMBER);
}



int yywrap() {  
  return(1);  
}       /* lex may need this. */
