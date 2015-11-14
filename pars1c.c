/* pars1c.c    Pascal Parser      Gordon S. Novak Jr.    02 Nov 05    */

/* This file contains a parser for a Pascal subset using the techniques of
   recursive descent and operator precedence.  The Pascal subset is equivalent
   to the one handled by the Yacc version pars1.y .  */

/* Copyright (c) 2005 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License (file gpl.text) for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


/* To use:
                     make pars1c
                     pars1c
                     i:=j .

                     pars1c
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.

                     pars1c
                     if x+y then if y+z then i:=j else k:=2.
*/

/* You may copy this file to be parsc.c, expand it for your assignment,
   then use    make parsec    as above.  */

#include <stdio.h> 
#include <ctype.h> 
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "parse.h"

TOKEN parseresult;
TOKEN savedtoken;

#define DEBUG 127 /* set bits here for debugging, 0 = off  */
#define DB_CONS 1 /* bit to trace cons */
#define DB_BINOP 2 /* bit to trace binop */
#define DB_MAKEIF 4 /* bit to trace makeif */
#define DB_MAKEPROGN 8 /* bit to trace makeprogn */
#define DB_PARSERES 16 /* bit to trace parseresult */
#define DB_GETTOK 32 /* bit to trace gettok */
#define DB_EXPR 64 /* bit to trace expr */

TOKEN cons(TOKEN item, TOKEN list) /* add item to front of list */
{
  item -> link = list;
  if (DEBUG & DB_CONS) {
    printf("cons\n");
    dbugprinttok(item);
    dbugprinttok(list);
  };
  return item;
}

TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs){ 

  /* reduce binary operator */
  /* operator, left-hand side, right-hand side */ 

  op -> operands = lhs; /* link operands to operator       */
  lhs -> link = rhs; /* link second operand to first    */
  rhs -> link = NULL; /* terminate operand list          */
  if (DEBUG & DB_BINOP) {
    printf("binop\n");
    dbugprinttok(op); /*       op         =  (op lhs rhs)      */
    dbugprinttok(lhs); /*      /                                */
    dbugprinttok(rhs); /*    lhs --- rhs                        */
  };
  return op;
}

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart) {

  tok -> tokentype = OPERATOR; /* Make it look like an operator   */
  tok -> whichval = IFOP;
  if (elsepart != NULL) elsepart -> link = NULL;
  thenpart -> link = elsepart;
  exp -> link = thenpart;
  tok -> operands = exp;
  if (DEBUG & DB_MAKEIF) {
    printf("makeif\n");
    dbugprinttok(tok);
    dbugprinttok(exp);
    dbugprinttok(thenpart);
    dbugprinttok(elsepart);
  };
  return tok;
}

TOKEN makeprogn(TOKEN tok, TOKEN statements) {

  tok -> tokentype = OPERATOR;
  tok -> whichval = PROGNOP;
  tok -> operands = statements;
  if (DEBUG & DB_MAKEPROGN) {
    printf("makeprogn\n");
    dbugprinttok(tok);
    dbugprinttok(statements);
  };
  return tok;
}

yyerror(s)
char * s; {
  fputs(s, stderr);
  putc('\n', stderr);
}

TOKEN gettok(){ /* Get the next token; works with peektok. */

  TOKEN tok;
  if (savedtoken != NULL) {

    tok = savedtoken;
    savedtoken = NULL;

  } else 
    tok = gettoken();

  if (DEBUG & DB_GETTOK) {

    printf("gettok\n");
    dbugprinttok(tok);
  };
  return (tok);
}

TOKEN peektok(){ /* Peek at the next token */

  if (savedtoken == NULL) 
    savedtoken = gettoken();

  if (DEBUG & DB_GETTOK) {

    printf("peektok\n");
    dbugprinttok(savedtoken);
  };
  return (savedtoken);
}

int reserved(TOKEN tok, int n){ /* Test for a reserved word */

  return (tok -> tokentype == RESERVED && 
         (tok -> whichval + RESERVED_BIAS) == n);
}

TOKEN parsebegin(TOKEN keytok){ /* Parse a BEGIN ... END statement */

  TOKEN front, end, tok;
  TOKEN statement();
  int done;
  front = NULL;
  done = 0;
  while (done == 0) {

    tok = statement(); /* Get a statement */
    if (front == NULL) /* Put at end of list */
      front = tok;
    else 
      end -> link = tok;
    tok -> link = NULL;
    end = tok;
    tok = gettok(); /* Get token: END or semicolon */

    if (reserved(tok, END)) 
      done = 1;
    else if (tok -> tokentype != DELIMITER || (tok -> whichval + DELIMITER_BIAS) != SEMICOLON) 
      yyerror("Bad item in begin - end.");
  };
  return (makeprogn(keytok, front));
}

TOKEN parseif(TOKEN keytok){ /* Parse an IF ... THEN ... ELSE statement */

  TOKEN expr, thenpart, elsepart, tok;
  TOKEN parseexpr();
  TOKEN statement();
  expr = parseexpr();
  tok = gettok();
  if (reserved(tok, THEN) == 0) 
    yyerror("Missing THEN");

  thenpart = statement();
  elsepart = NULL;
  tok = peektok();

  if (reserved(tok, ELSE)) {

    tok = gettok(); /* consume the ELSE */
    elsepart = statement();
  };
  return (makeif(keytok, expr, thenpart, elsepart));
}

TOKEN parseassign(TOKEN lhs){ /* Parse an assignment statement */

  TOKEN tok, rhs;
  TOKEN parseexpr();
  tok = gettok();
  if (tok -> tokentype != OPERATOR || tok -> whichval != ASSIGNOP) 
    printf("Unrecognized statement\n");
  rhs = parseexpr();
  return (binop(tok, lhs, rhs));
}

/* *opstack and *opndstack allow reduce to manipulate the stacks of parseexpr */
void reduce(TOKEN * opstack, TOKEN * opndstack){ /* Reduce an op and 2 operands */

  TOKEN op, lhs, rhs;
  if (DEBUG & DB_EXPR) {
    printf("reduce\n");
  };
  op = *opstack; /* pop one operator from op stack */
  *opstack = op -> link;
  rhs = * opndstack; /* pop two operands from opnd stack */
  lhs = rhs -> link; 
  *opndstack = lhs -> link; 
  *opndstack = cons(binop(op, lhs, rhs), * opndstack); /* push result opnd */
}





/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


/*
TOKEN opstack, opndstack;

/ *                +  -  *  /  ... * /
int opprec[20] = {
  0, 6, 6, 7, 7, ...
};

void pushop(TOKEN tok)  { /* push op onto stack * /
  tok -> link = opstack;
  opstack = tok;
}

TOKEN popop() { /* pop op from stack * / 

  TOKEN tok;
  tok = opstack;
  opstack = tok -> link;
  return (tok);
}

int prec(TOKEN tok) /* precedence of op tok * / {

  if (tok == NULL)
    return (-1); /* -1 for empty stack * 
  else if (tok -> tokentype == OPERATOR)
    return (opprec[tok -> whichval]);
  else return (-1);
} /* -1 for ( * /

reduceop (){         /* reduce binary op   * /
 
  TOKEN op, lhs, rhs;
  rhs = popopnd();  /* rhs at top * /
  lhs = popopnd();
  op = popop();
  op->operands = lhs; /* first child  * /
  lhs->link = rhs;    /* next sibling * /
  rhs->link = NULL;   /* null terminate * /
  pushopnd(op);  
} /* subtree now operand * /



TOKEN expr() {
  int done;
  TOKEN tok;
  done = 0;
  opstack = NULL;
  opndstack = NULL;
  while (done == 0) {
    tok = gettoken();
    if (EOFFLG == 0){
      switch (tok -> tokentype) {
      case IDENTIFIERTOK:
      case NUMBERTOK:
        pushopnd(tok);
        break;
      case DELIMITER:
        if (tok -> whichval == LPARENTHESIS)
          pushop(tok);
        else if (tok -> whichval == RPARENTHESIS) {
          while (opstack -> whichval != LPARENTHESIS)
            reduceop();
            popop();
          }
        else 
          done = 1;
      break;
      case RESERVED:
        done = 1;
        break;
      case OPERATOR:
        while (prec(tok) <= prec(opstack))
          reduceop();
        pushop(tok);
      break;
    }else 
      done = 1;
  }
  while (opstack != NULL) 
    reduceop();
  return (opndstack);   
}
//(op lhs rhs)
//Recursive descent

/*
http://www.cs.utexas.edu/users/novak/cs37598.html


i:=j.     

binop
 79220  OP     :=  link     0  operands 79172
 79172  ID      I  link 79268
 79268  ID      J  link     0

yyparse result = 0
 79220  OP     :=  link     0  operands 79172

(:= I J)



program
   /
  /
 graph1 -- progn -- progn
            /        /
           /        /
        output     := --- progn
                  /        /
                 /        /
               lim -- 7  := ---- label -- if
                        /         /      /
                       /         /      /
                      i -- 0    0      <= ----- progn
                                      /          /
                                     /          /
                                    i -- lim  funcall ----- := ---- goto
                                              /             /        /
                                             /             /        /
                                          writeln -- '*'  i -- +   0
                                                              /
                                                             /
                                                            i -- 1


{ First test program       -- file trivb.pas }


{ Results should look like this:
 yyparse result =        0
 Symbol table level 0   ... printed out so you can see it. }
{Symbol table level 1
 25590736           i  VAR    0 typ integer  lvl  1  siz     4  off     0   }
{25590832         lim  VAR    0 typ integer  lvl  1  siz     4  off     4
 token 25592784  OP       program  dtype  0  link 0  operands 25589968
}
{
(program graph1 (progn output)
                (progn (:= lim 7)
                       (progn (:= i 0)
                              (label 0)                        }
{                             (if (<= i lim)
                                  (progn (funcall writeln '*')
                                         (:= i (+ i 1))
                                         (goto 0))))))         }

program graph1(output);
var i,lim : integer;
begin
   lim := 7;
   for i := 0 to lim do
      writeln('*')
end.




ERRORS
http://www.cs.utexas.edu/users/novak/cs375109.html


PARSING TECHNIQUES
http://www.cs.utexas.edu/users/novak/cs375110.html


Some examples of good hash functions:
  Treat the symbol name as an integer and find the remainder
  of the name modulo p, where p is prime.




Symbol Table Entry

A symbol table entry stores a variety of information about a symbol:

link  Link to Next Symbol
namestring  Name of Symbol
kind  Kind of Symbol: VARSYM
basicdt Basic Data Type: REAL
datatype  Pointer to Data Type
blocklevel  Block Nesting Level: 1
size  Size in bytes
offset  Offset from Data Area
intnum, realnum Constant Value
lowbound  Low Bound
highbound High Bound
















*/

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

/*                             +     *                                     */
static int precedence[] = {
  0, 1, 0, 3
}; /* **** trivial version **** */

TOKEN parseexpr() /* Parse an expression using operator precedence */
/* Partial implementation -- handles +, *, ()    */ {
  TOKEN tok, op, lhs, rhs;
  int state, done;
  TOKEN opstack, opndstack;

  if (DEBUG & DB_EXPR) {
    printf("parseexpr\n");
  };
  done = 0;
  state = 0;
  opstack = NULL;
  opndstack = NULL;
  while (done == 0) {

    tok = peektok();
    switch (tok -> tokentype) {

      case IDENTIFIERTOK:
      case NUMBERTOK:
        /* operand: push onto stack */
      printf("NUMBERTOK\n" );
        tok = gettok();
        opndstack = cons(tok, opndstack);
        break;

      case DELIMITER:

      printf("DELIMITER\n" );
        if ((tok -> whichval + DELIMITER_BIAS) == LPAREN) {

          tok = gettok();
          opstack = cons(tok, opstack);

        } else if ((tok -> whichval + DELIMITER_BIAS) == RPAREN) {

          tok = gettok();
          while (opstack != NULL && (opstack -> whichval + DELIMITER_BIAS) != LPAREN)
          reduce( & opstack, & opndstack);
          opstack = opstack -> link; /* discard the left paren */

        }else 
          done = 1;
        break;

      case OPERATOR:
      printf("OPERATOR\n" );
      printf("--------------- %d \n" ,tok->whichval);

        if (tok -> whichval != DOTOP) {/* special case for now */
        
          tok = gettok();
      printf("---------------> %d \n" ,tok->whichval);
          while (opstack != NULL && opstack -> tokentype != DELIMITER 
             && (precedence[opstack -> whichval] >= precedence[tok -> whichval]))
            reduce( & opstack, & opndstack);
          opstack = cons(tok, opstack);
        }else 
          done = 1;
        break;

      default:
      printf("DEFAULT\n");
        done = 1;

        break;
    }
  }
  while (opstack != NULL) 
    reduce( & opstack, & opndstack);

  return (opndstack);
}

TOKEN statement(){ /* Parse a Pascal statement: the "big switch" */

/*
Pascal, each statement other than assignment statements begins with 
a unique reserved word; thus, it is easy for a ``big switch'' program 
to determine which statement parser to call.

*/
  TOKEN tok, result;
  result = NULL;
  tok = gettok();
  if (tok -> tokentype == RESERVED) 
    switch (tok -> whichval + RESERVED_BIAS){ /* the big switch */
    
      case BEGINBEGIN:
        result = parsebegin(tok);
        break;
      case IF:
        result = parsebegin(tok);
        break;
    } 
  else if (tok -> tokentype == IDENTIFIERTOK) 
    result = parseassign(tok);
  return (result);
}

int yyparse(){ /* program = statement . */

  TOKEN dottok;
  savedtoken = NULL;
  parseresult = statement(); /* get the statement         */
  dottok = gettok(); /* get the period at the end */
  if (dottok -> tokentype == OPERATOR && dottok -> whichval == DOTOP) 
    return (0);
  else 
    return (1);
}


main(){ /* Call yyparse repeatedly to test */

  int res;
  initscanner();
  init_charclass(); /* initialize character class array */
  printf("Started parser test.\n");
  res = yyparse();
  printf("yyparse result = %8d\n", res);
  if (DEBUG & DB_PARSERES) 
    dbugprinttok(parseresult);
  pplist(parseresult);
  ppexpr(parseresult);
}