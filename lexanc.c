/* lex1.c         14 Feb 01; 31 May 12       */
/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */
/* Copyright (c) 2001 Gordon S. Novak Jr. and
   The University of Texas at Austin. */
/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
//                                LEXICAL ANALYSER                                         //
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

#include <stdio.h> 
#include <ctype.h> 
#include <string.h> 
#include "token.h"
#include "lexan.h"
#include <regex.h> 
  

/*
Definitions
*/  

static char *identifiers[] = { " ", "array", "begin", "case", "const", "do",
                               "downto", "else", "end", "file", "for",
                               "function", "goto", "if", "label", "nil",
                               "of", "packed", "procedure", "program", "record",
                               "repeat", "set", "then", "to", "type",
                               "until", "var", "while", "with" };
static char* opprnt[]  = {" ", "+", "-", "*", "/", ":=", "=", "<>", "<", "<=",
                          ">=", ">",  "^", ".", "and", "or", "not", "div",
                          "mod", "in"};
static char *delprnt[] = { " ", ",", ";", ":", "(", ")", "[", "]",".."} ; 

static unsigned int intLimit = (1<<31);//2147483648
static long double floatLimitMin = 1.175495E-38;
static long double floatLimitMax = 3.402823E+38;

static double floatLimitMinLeft = 1.175495;
static double floatLimitMinRight = -38;
static double floatLimitMaxLeft = 3.402823;
static double floatLimitMaxRight = 38;

/* 
Skip blanks and whitespace.  
Expand this function to skip comments too. 
Whitespace: blank, tab, newline.
*/ 


/*
  
{ You may assume the following limits on numbers: }
  works
{You may assume the following limits on numbers: }
  doesnt
{ 123 }
  doesnt

  fuckup on second iteration
*/
void skipblanks() {

 
  //printf("::skipblanks() \n");
  int c;  
  int comment_line = 0;//0 means comment tag not opened  for one line comments
  int comment_block = 0;//0 means comment tag not opened for multi line comments
  int terminate = 0;

  while((c = peekchar()) != EOF && (c == ' ' || c == '\n' || c == '\t')){
    //printf("skipped one blank \n");
    getchar();
  }

  int first = peekchar(); 
  int second = peek2char();
  if((first == '(' && second == '*' ) || first == '{') {

    c = peekchar();
    while (c != EOF && terminate == 0){
       
      c = peekchar();
      first = peekchar(); 
      second = peek2char(); 
    
      int empty = (c == ' ' || c == '\n' || c == '\t'); 
      //if we havent opened a block comment, check if we can open single comment
      int open_line = (first == '{' && comment_block == 0);
      int close_line = (first == '}' && comment_block == 0);
      if(open_line){
        comment_line = 1; 
        //getchar();//skip the beginning 1 character
      }else if(close_line){
        comment_line = 0;
        terminate = 1;
        //getchar();//skip the end 1 character
      }

      //if we havent opened a single comment, check if we can open a block comment

      int open_block = (first == '(' && second == '*' && comment_line == 0);
      int close_block = (first == '*' && second == ')' && comment_line == 0);
      if(open_block){  
        comment_block = 1;
 
      }else if(close_block){
        comment_block = 0;  
        terminate = 1;  
      }    
      if(comment_line || comment_block || empty && (open_block==0 && open_line==0)){ 
        getchar(); 

      }   
    }

  }  

  while((c = peekchar()) != EOF && (c == ' ' || c == '\n' || c == '\t')){
    //printf("skipped one blank \n");
    getchar();
  } 
} 

/* 
Get identifiers and reserved words 
Identifiers: string and symbol table pointer
Reserved words: integer code.
Special: ( ) [ ] + = . etc.

array       downto    function   of           repeat    until
begin       else      goto       packed       set       var  
case        end       if         procedure    then      while
const       file      label      program      to        with 
do          for       nil        record       type  

*/
TOKEN identifier(TOKEN tok) {
    

  char collector[15] = "";
  char c = peekchar(); 
  int counter = 0;
  int loop = -1;//found = 0 means not found
 
  
  while (c != EOF && CHARCLASS[c] == ALPHA ) {//collect the identifier

    if(counter < 15)  
      sprintf(collector, "%s%c", collector, getchar()); 
    else
      getchar();  
    c = peekchar();
    counter++;
  }  

  c = peekchar();
  while(counter<=15 && c != ' ' && c != '\n' && c != '\t'){
    getchar();
    c = peekchar();
  }
 
  counter = 0;
  //look for a match in identifiers[]
  while(counter < 29 && loop == -1){

    if(!strcmp( collector, identifiers[counter] ))
      loop = 2; 
    else
      counter++;
  }   
  if(loop==-1){//if no match

    counter = 0;
    while(counter < 20 && loop == -1){

      if(!strcmp( collector, opprnt[counter] ))
        loop = 0; 
      else
        counter++;
    } 
  }  
  
  tok->datatype = ALPHA;
  if(loop==2){

    tok->whichval = counter;
    tok->tokentype = RESERVED;//RESERVED  

  }else if(loop==0){
    
    tok->whichval = counter;
    tok->tokentype = OPERATOR;   
     
  }else{
    
    tok->tokentype = IDENTIFIERTOK;    
    for(counter = 0; counter < 16; counter++)
      tok->stringval[counter] = collector[counter];//value
    
  } 
  return (tok);  
}

/* 
Get strings
Identifiers: string and symbol table pointer
Alphabetic: A B C ... Z
*/
TOKEN getstring(TOKEN tok) {
    

  int counter = 0;//string open
  int loop = 0;
  char collector[15] = "";
  char c;
  char quote = 0;
    
  c = getchar();//skip first quote 
  while (c != EOF && loop == 0 && counter < 15){

    int first = peekchar(); 
    int second = peek2char(); 
    c = getchar();  
     
    if(first == '\'' && (second == '\n' || second == ' ' || second == '\t')){
 
      loop = 1; 
    }

    if(quote==0 && loop == 0){
      sprintf(collector, "%s%c", collector, c ); 
      
    }
 
    if(first == '\'' && second == '\'') {
      
      quote = 1;
    }else
      quote = 0;

    
    counter++;   
  }   

  while(counter >= 15 && c != '\''){ 
    c = getchar(); 
  }
 
  getchar();//skip the last quote
  

  tok->tokentype = STRINGTOK;// (identifier, operator, etc.)
  tok->datatype = ALPHA;//a numeric code indicating integer, real, etc.

  for(counter = 0; counter < 16; counter++)
    tok->stringval[counter] = collector[counter];//value 
  
  return (tok);

}
/* 
Get operators
Operators: integer code.
Other: characters not in the language ~ @ #
*/
TOKEN special(TOKEN tok) {
 
  
  int counter = 0;
  int loop = -1;
  int c ;
  char collector[16] = "";

  while ((c = peekchar()) != EOF && CHARCLASS[c] == SPECIAL) {//collect the identifier
       
    sprintf(collector, "%s%c", collector, getchar()); 
  } 

  //look for a match in identifiers[]

  while(counter < 9 && loop == -1){
  
    if(!strcmp( collector, delprnt[counter] ) || delprnt[counter]  == collector)  
      loop = 2; 
    else
      counter++;
  }   
  if(loop==-1){//if no match

    counter = 0;
    while(counter < 20 && loop == -1){
  
      if(!strcmp( collector, opprnt[counter] ) || opprnt[counter]  == collector)   
        loop = 0; 
      else
        counter++;
    } 
  }   
 
  tok->datatype = SPECIAL;//a numeric code indicating integer, real, etc.
  if(loop == 2){
    
    tok->tokentype = DELIMITER;// (identifier, operator, etc.)
    tok->whichval = counter;//OPERATOR, DELIMITER, or RESERVED

  }else if(loop == 0){
    
    tok->tokentype = OPERATOR;// (identifier, operator, etc.)
    tok->whichval = counter;//OPERATOR, DELIMITER, or RESERVED
  } 

  return (tok);
}
 

long double convertDotCounter(long double original, unsigned int input, int eCounter, int sign) {

  //3.1415927e-40

  //7 dot spaces    input
  //40 e            eCounter
  //3.14159E+06 |  3141590 is the original.    original

  int isOverflowFloat = 0;
  int counter = 0; 
  while(counter < input && !isOverflowFloat){
    original = original / 10;
 

    isOverflowFloat = ((-eCounter <= floatLimitMinRight && sign == 0) || eCounter >= floatLimitMaxRight);
    isOverflowFloat = isOverflowFloat && (original <= floatLimitMinLeft || original >= floatLimitMaxLeft);

    //printf("1:> %LG isOverflowFloat:%d \n", original,isOverflowFloat);
    counter++;
  } 
  if(isOverflowFloat)
    return -1.0;
  return original;
}
 
long double convertECounter(long double original, unsigned int input, int sign) {
 
  int counter = 0; 
  while(counter < input){
    if(sign == 0)
      original = original / 10;
    else
      original = original * 10;
 
    counter++;
  } 
  return original;
}
  

/* 
Get and convert unsigned numbers of all types. 
Numbers: internal binary form.
Numerals: 0 1 2 3 4 5 6 7 8 9
*/  
/*
ISSUE - e23 doesnt work. what is more than long double?
e-38 to e+38 isthe range we work with. anything beyong 37 if OFF
Take 8 significant digits and let the machine round it for you.
*/
TOKEN number(TOKEN tok) {
 
  //double and long are the same, 64 bits

  //3.1415927e-40 

  int current = peekchar(); 
  int val = 0;
  int terminate = 0;
  int lengthLimit = 7;
  int counter = 0;
  int deletedZeros = 0;

  double left = 0;
  double right = 0;

  int isE = 0;
  int isDot = 0; 
  int sign = 1;
 
  int isOverflowInt = 0;
  int isOverflowFloat = 0;

  int eCounter = 0;
  unsigned int dotCounter = 0;
  unsigned int numCounter = 0;
 
  long double numCounterLong = 0; 
   

  //filter out zeros
  while (current != EOF && current == '0') {
 
    current = getchar();
    deletedZeros = 1;
  } 

  while (!terminate && current != EOF && // counter <= lengthLimit &&
        (CHARCLASS[current] == NUMERIC || current == 'e' || current == '-' ||  current == '.' || current == '+')) {
  
    if(peekchar()=='.' && peek2char()=='.' || 
      (peekchar() == ' ' || peekchar() == '\t' || peekchar() == '\n')){
      terminate = 1;

    }else{

      if(!deletedZeros){
        current = getchar();  
      }else 
        deletedZeros = 0;

      val = (current - '0'); 

      if(current=='e')
        isE = 1;
      
      if(current=='.')
        isDot = 1; 

      if(current=='-')
        sign = 0;

      if(isE && current != '+' && current != '-' && current!='e') 
        eCounter = eCounter * 10 + val; 

      if(isDot && !isE && current!='.')//count integers after dot until e
        dotCounter++; 

      if(!isE && current != '+' && current != '-' && current!='e' && current!='.'){  
        numCounter = numCounter * 10 + val; 
        if(!isOverflowInt)//only set overflow once
          isOverflowInt = numCounter > (numCounter * 10 + val);//check for overflow 
      } 
 
      if(current!='.')
        counter++;//count the length of the number 
    }
  }

  //if number was too large, truncate
  if(counter >= lengthLimit){
    while (current != EOF &&
        (CHARCLASS[current] == NUMERIC || current == 'e' || current == '-' ||  current == '.' || current == '+')) {
      current = getchar();
    }
  }
    
  numCounterLong = numCounter;
  
  if(isDot)
    numCounterLong = convertDotCounter(numCounterLong,dotCounter,eCounter,sign); //if -1 we had overflow
  if(numCounterLong==-1.0){ 
    isOverflowFloat = 1;
  } 
  numCounterLong = convertECounter(numCounterLong,eCounter,sign);      

  tok->tokentype = NUMBERTOK;// (identifier, operator, etc.) 
  
  if(isDot || isE){
   
    printf(":::%LG\n", numCounterLong);
    if(isOverflowFloat){ 
      printf("Integer number out of range\n");
      tok->realval = 0;  
    }else 
      tok->realval = numCounterLong;  
 
    tok->datatype = REAL;  
  } else {

    if(isOverflowInt){
      printf("Integer number out of range\n");
      tok->intval = 0; 
    }else
      tok->intval = numCounter; 
    tok->datatype = INTEGER;  
  }  
  return (tok);
}
 
