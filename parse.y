/*
Semur Nabiev
UTID:sn9755
CSID:snabiev
graph1 parser
*/
%{     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 30 Jul 13   */

/* Copyright (c) 2013 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* 14 Feb 01; 01 Oct 04; 02 Mar 07; 27 Feb 08; 24 Jul 09; 02 Aug 12 */

/*
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
  */


/* NOTE:   Copy your lexan.l lexical analyzer to this directory.      */

       /* To use:
                     make pars1y              has 1 shift/reduce conflict
                     pars1y                   execute the parser
                     i:=j .
                     ^D                       control-D to end input

                     pars1y                   execute the parser
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.
                     ^D

                     pars1y                   execute the parser
                     if x+y then if y+z then i:=j else k:=2.
                     ^D

           You may copy pars1.y to be parse.y and extend it for your
           assignment.  Then use   make parser   as above.
        */

        /* Yacc reports 1 shift/reduce conflict, due to the ELSE part of
           the IF statement, but Yacc's default resolves it in the right way.*/

#include <stdio.h>
#include <ctype.h>
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "parse.h"
#include <string.h>

        /* define the type of the Yacc stack element to be TOKEN */

#define YYSTYPE TOKEN

TOKEN parseresult;

%}

/* Order of tokens corresponds to tokendefs.c; do not change */

%token IDENTIFIER STRING NUMBER   /* token types */

%token PLUS MINUS TIMES DIVIDE    /* Operators */
%token ASSIGN EQ NE LT LE GE GT POINT DOT AND OR NOT DIV MOD IN

%token COMMA                      /* Delimiters */
%token SEMICOLON COLON LPAREN RPAREN LBRACKET RBRACKET DOTDOT

%token ARRAY BEGINBEGIN           /* Lex uses BEGIN */
%token CASE CONST DO DOWNTO ELSE END FILEFILE FOR FUNCTION GOTO IF LABEL NIL
%token OF PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL
%token VAR WHILE WITH

 
%%
 
 
  program    : PROGRAM IDENTIFIER LPAREN IDENTIFIER RPAREN SEMICOLON instructions DOT { parseresult = makeprogram($2, $4, $7); }
             ;
 

  instructions : statements                                            { $$ = $1; } 
               | variables_block                                       { printf("::: variables_block\n");} 
               | variables_block BEGINBEGIN statements                 { printf("::: variables_block BEGINBEGIN statements\n"); $$ = makeprogn($1,cons($2, $3)); } 
               | constants_block variables_block BEGINBEGIN statements { printf("::: variables_block constants_block BEGINBEGIN statements\n"); $$ = makeprogn($2,cons($3, $4)); } 
               ;   
 
  constants_block  : CONST constants constants_block             { printf("::: CONST constants constants_block \n"); } 
                   | CONST constants                             { printf("::: CONST constants \n"); } 
                   ;

  constants    : constant constants                              { printf("::: constant constants \n"); } 
               | constant                                        { printf("::: constant \n"); } 
               ;

  constant     : IDENTIFIER EQ NUMBER SEMICOLON                 { printf("::: IDENTIFIER EQ IDENTIFIER SEMICOLON \n"); instconst($1, $3); } 
               ; 

  variables_block  : VAR variables variables_block             { printf("::: VAR variables variables_block\n" );$$ = $3; } 
                   | VAR variables                             { printf("::: VAR variables\n");$$ = $2; } 
                   ;
 
  variables    : variable COLON IDENTIFIER SEMICOLON variables   { printf("::: variable COLON IDENTIFIER SEMICOLON variables\n"); instvars($1, findid($3)); $$ = $5; } 
               | variable COLON IDENTIFIER SEMICOLON             { printf("::: variable COLON IDENTIFIER SEMICOLON \n"); instvars($1, findid($3));  } 
               ;
             
  variable     : IDENTIFIER COMMA variable                 { printf("::: IDENTIFIER COMMA variable    \n");$$ = $3; }
               | IDENTIFIER                                { printf("::: IDENTIFIER\n");$$ = $1; }
               ; 
 
  
  statements   : statement SEMICOLON statements            { printf("::: statement SEMICOLON statements\n");$$ = cons($1, $3); } 
               | statement                                 { printf("::: statement\n");$$ = $1;  }
               | end                                       { printf("::: end\n");$$ = $1;  }
               ;
               

  statement    :  BEGINBEGIN statement end              { printf("::: BEGINBEGIN statement end\n");$$ = makeprogn($1,cons($2, $3)); } 
               |  FOR assignment TO expr DO statement   { printf("::: FOR assignment TO expr DO statement\n");$$ = makefor(1,$1,$2,$3,$4,$5,$6) ;}
               |  IDENTIFIER LPAREN expr RPAREN end     { printf("::: IDENTIFIER LPAREN expr RPAREN end\n");$$ = makefuncall($2, $1, $3);}
               |  assignment statement                  { printf("::: assignment statement\n");$$ = cons($1,$2); }  
               |  assignment                            { printf("::: assignment\n");$$ = $1; }  
               ; 
  
  end        :  SEMICOLON statement end        { printf("::: SEMICOLON statement end\n");$$ = cons($2, $3); }
             |  END                            { printf("::: END\n");$$ = NULL; }
             ;
  

  assignment :  factor ASSIGN expr             { printf("::: factor ASSIGN expr\n");$$ = binop($2, $1, $3); }
             ;

  expr       :  expr PLUS negation                 { printf("::: expr PLUS negation \n"); $$ = binop($2, $1, $3); }
             |  expr MINUS negation                { printf("::: expr MINUS negation\n"); $$ = binop($2, $1, $3); }
             |  expr TIMES negation                { printf("::: expr TIMES negation\n"); $$ = binop($2, $1, $3); }
             |  expr EQ negation                  {printf("::: expr EQ negation \n"); $$ = binop($2,$1,$3);}
             |  negation                        { printf("::: negation \n");$$ = $1; }
             ;  

  negation   :  MINUS term                     { printf("::: MINUS term \n");$$ = negate($1,$2); }
             |  term                           { printf("::: term \n");$$ = $1; }
             ;

  term       :  term TIMES factor              { $$ = binop($2, $1, $3); }
             |  factor                         { $$ = $1; }
             ;
  factor     :  LPAREN expr RPAREN             { $$ = $2; }
             |  IDENTIFIER                     { printf("::: IDENTIFIER in factor \n");  $$ = $1;}
             |  NUMBER                         { $$ = $1; }   
             |  STRING                         { printf("::: STRING in factor \n");$$ = $1;}
             ;

  



%%

/* You should add your own debugging flags below, and add debugging
   printouts to your programs.

   You will want to change DEBUG to turn off printouts once things
   are working.
  */

#define DEBUG        31             /* set bits here for debugging, 0 = off  */
#define DB_CONS       1             /* bit to trace cons */
#define DB_BINOP      2             /* bit to trace binop */
#define DB_MAKEIF     4             /* bit to trace makeif */
#define DB_MAKEPROGN  8             /* bit to trace makeprogn */
#define DB_PARSERES  16             /* bit to trace parseresult */
  

int labelnumber = 0;  /* sequential counter for internal label numbers */

   /*  Note: you should add to the above values and insert debugging
       printouts in your routines similar to those that are shown here.     */
  
/* makefuncall makes a FUNCALL operator and links it to the fn and args.
   tok is a (now) unused token that is recycled. */
TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args){
    printf("::::::::::::::::::::: FUNCTION CALL\n");
    TOKEN functok = talloc();
    functok->tokentype = OPERATOR;
    functok->whichval = FUNCALLOP; //whichval defined in token.h
    //link function name to input
    fn->link = args; 
    functok->operands = fn;
    return functok;
}
 

TOKEN negate(TOKEN op, TOKEN lhs)        /* reduce binary operator */
{
    printf("::::::::::::::::::::: NEGATION\n");
  printf("You called oneop function \n");
  op->operands = lhs;          /* link operands to operator       */
  lhs->link = NULL;             /* link second operand to first    */
  if (DEBUG & DB_BINOP)
  { printf("onenop: \n");
      dbugprinttok(op);
      dbugprinttok(lhs);
  };
  printf("You finished calling oneop function \n");
  return op;
}


/* instvars will install variables in symbol table.
   typetok is a token containing symbol table pointer for type. */
//Taken from slide: cs375133.html
void instvars(TOKEN idlist, TOKEN typetok)
{  SYMBOL sym, typesym; int align;
   typesym = typetok->symtype;
   align = alignsize(typesym);
   while ( idlist != NULL )   /* for each id */
     {  sym = insertsym(idlist->stringval);
        sym->kind = VARSYM;
        sym->offset =
            wordaddress(blockoffs[blocknumber],
                        align);
        sym->size = typesym->size;
        blockoffs[blocknumber] =
                       sym->offset + sym->size;
        sym->datatype = typesym;
        sym->basicdt = typesym->basicdt;
        idlist = idlist->link;
      };
}
/* instconst installs a constant in the symbol table */
void  instconst(TOKEN idtok, TOKEN consttok) 
{
    printf("You called instconstant \n");
    
    SYMBOL sym;
    sym = insertsym(idtok->stringval);
    //setup kind
    sym->kind = CONSTSYM;
    //set up the basicdt (INTEGER REAL etc)
    sym->basicdt = consttok->datatype;
    
    //set up the size and actual value
    if(sym->basicdt == 1) //real
    {
        sym->constval.realnum = consttok->realval;
        sym->size = 8;
    }
    if(sym->basicdt == 0) //int
    {
        sym->constval.intnum = consttok->intval;
        sym->size = 4;
    }
 
    //sym->size = basicsizes[consttok->datatype]; 
    printf("You finished calling instconst \n");
}




               // |  FOR assignment TO expr DO statement   { $$ = makefor(1,$1,$2,$3,$4,$5,$6) ;}

/* link tokb to integer token  */
TOKEN setlabel(){

    TOKEN toklabel = talloc();
    toklabel->tokentype = NUMBERTOK;
    toklabel->datatype = INTEGER;
    toklabel->intval = labelnumber; // 0 in the diagram
    return toklabel;
}
/*
create 'i' token - which is copying token i from asg tok 
connect <= operator to i
*/
TOKEN settokif(TOKEN endexpr,TOKEN asg){

    TOKEN tokif = talloc();
    tokif->tokentype = asg->operands->tokentype; 
    strcpy (tokif->stringval,asg->operands->stringval); 
    tokif->link = endexpr;

    return tokif;
}
/* goto operand to 0 (lbl) */
TOKEN setgoto(){

    //goto operand to 0 (lbl)
    TOKEN gototok = talloc();
    gototok->tokentype = NUMBERTOK;
    gototok->datatype = INTEGER;
    gototok->intval = labelnumber; 
     
    return gototok;
}
/* set equal and link to statement  */
TOKEN seteqlink(TOKEN statement){
 
    TOKEN eqlink = talloc();
    eqlink->tokentype = OPERATOR;
    eqlink->whichval = ASSIGNOP;
    statement->link = eqlink; 
    return eqlink;
}
/*
Set up progn 
link tokd with progn (less than equal to, to , progn)
*/
TOKEN setprogn(TOKEN statement){

    TOKEN progntok = talloc();
    //progn link to statement
    progntok->operands = statement;
    progntok->whichval = PROGNOP;
    progntok->tokentype = OPERATOR; 

    return progntok;
}
/* i to + and link */
TOKEN setiinc(TOKEN tokj){

    TOKEN iinc = talloc();
    iinc->tokentype = OPERATOR;
    iinc->whichval = PLUSOP;  
    iinc->operands = tokj;
    return iinc;
}
/* i to 1 to increment */
TOKEN setione(){
    TOKEN ione = talloc();
    ione->tokentype = NUMBERTOK;
    ione->datatype = INTEGER;
    ione->intval = 1;//increment i by this amount
    return ione;
}
/* tokg link to goto  */
TOKEN setlinktogo(){ 

    TOKEN linktogo = talloc();
    linktogo->whichval = GOTOOP;
    linktogo->tokentype = OPERATOR;

    return linktogo;
}
/* makefor makes structures for a for statement.
   sign is 1 for normal loop, -1 for downto [not working yet].
   asg is an assignment statement, e.g. (:= i 1)
   endexpr is the end expression
   tok, tokb and tokc are (now) unused tokens that are recycled. */

TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr, TOKEN tokc, TOKEN statement){
     
    TOKEN fortok = talloc();
    fortok->whichval = PROGNOP;   //progn defined in token.h, like the rest of whichval assignments
    fortok->tokentype = OPERATOR;
    fortok->operands = asg;//link new progn FOR to assignment 
    
    //set a new label
    labelnumber = labelnumber+1; 

    //setup tokb to be label
    tokb->whichval = LABELOP; 
    tokb->tokentype = OPERATOR;
    //link assignment asg to new label
    asg->link=tokb;
     
    tokb->operands = setlabel(); 
    
    //set the if operator
    tokc->whichval = IFOP; 
    tokc->tokentype = OPERATOR; 
    tokb->link = tokc;
    
    //create new token <= tok
    TOKEN leseqtok = talloc();
    leseqtok->whichval = LEOP;
    leseqtok->tokentype = OPERATOR; 
    
    //set if statement to leseqtok token 
    tokc->operands = leseqtok;  
    leseqtok->operands = settokif(endexpr,asg);   
    leseqtok->link = setprogn(statement); 
    TOKEN tokeqlink = seteqlink(statement); 
     
    //make a new token and set its value from assigned value to incrementor - which is i
    TOKEN eqitok = talloc();
    eqitok->tokentype = asg->operands->tokentype; 
    strcpy (eqitok->stringval,asg->operands->stringval);
    //link eqial to that token
    tokeqlink->operands = eqitok; 
     
    //set plus to i
    TOKEN plusitok = talloc();
    plusitok->tokentype = asg->operands->tokentype;
    strcpy (plusitok->stringval,asg->operands->stringval); 
    
    eqitok->link = setiinc(plusitok);   
    plusitok->link = setione();   
    TOKEN toklinktogo = setlinktogo();
    toklinktogo->operands = setgoto();

    tokeqlink->link = toklinktogo;  
 
    return fortok;
}

/* initiates tokens for main function - program*/
TOKEN settoken(){

  TOKEN tok = talloc();
  tok->tokentype = OPERATOR;
  tok->whichval = PROGRAMOP;
  return tok;
}
/* makeprogram makes the tree structures for the top-level program */
TOKEN makeprogram(TOKEN name, TOKEN args, TOKEN statements){
 
    //build program token
    TOKEN program = settoken();  
    //set program name
    program->operands = name;
     
    //build program token 
    TOKEN output = settoken(); //args for program - output
    name->link = output;//link program to output
    
    output->operands = args;
    output->link = statements; //link program(output) to begin
     
    return program;
}

 

/* findtype looks up a type name in the symbol table, puts the pointers
 into tok, returns tok. */
TOKEN findid(TOKEN tok){

    SYMBOL sym; 
    sym = searchst(tok->stringval); //get sym table entry
         
    tok->symentry = sym;  
    if(sym->datatype==NULL && sym->kind == BASICTYPE){ //if sype of sym entry not set, set it for Integers for now
      printf("typ is null \n");
      tok->symtype = sym; 
      tok->datatype = sym->basicdt; 
       
    } 
    return tok; 



    //////////////////////////



//     printf("|////////////////////////// \n");
//     printf("You called findid \n");
//     printf("roobs of tok: \n");
//     printf("%s \n",tok->stringval);
//     printf("%i \n",tok->datatype);

    
//     SYMBOL sym, typ;
//     sym = searchst(tok->stringval);
    
//     //if symbol is constant
//     if(sym->kind == CONSTSYM)
//     {
//         if(sym->basicdt == 0) //int
//         {
//             tok->tokentype = NUMBERTOK;
//             tok->datatype = INTEGER;
//             tok->intval = sym->constval.intnum;
//         }
//         if(sym->basicdt == 1) //real
//         {
//             tok->tokentype = NUMBERTOK;
//             tok->datatype = REAL;
//             tok->realval = sym->constval.realnum;
//         }
//     }
    
// //    if(sym->kind == VARSYM)//the tok is a variable
// //    {
// //        //smash the token's tokentype
// //        tok->tokentype = sym->datatype;
// //    }
//     else{
//     tok->symentry = sym; //i dont know why this is happening
//     typ = sym->datatype; //typ is sym's datatype
//     tok->symtype = typ;  //so im changing the symtype of the token to the sym's datatype?
//     if ( typ->kind == BASICTYPE || typ->kind == POINTERSYM)
//         {
//             printf("speak \n");
//             ppexpr(tok);
//             tok->datatype = typ->basicdt;
//         }
//     }
//     printf("You are checking shit findid \n");
//     printf("toobs of tok: \n");
//     printf("%s \n",tok->stringval);
//     printf("%i \n",tok->datatype);

//     printf("////////////////////////// \n");
//     return tok;
}

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { item->link = list;
    if (DEBUG & DB_CONS)
       { printf("cons\n");
         dbugprinttok(item);
         dbugprinttok(list);
       };
    return item;
  }

TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs)        /* reduce binary operator */
  { op->operands = lhs;          /* link operands to operator       */
    lhs->link = rhs;             /* link second operand to first    */
    rhs->link = NULL;            /* terminate operand list          */
    if (DEBUG & DB_BINOP)
       { printf("binop\n");
         dbugprinttok(op);
         dbugprinttok(lhs);
         dbugprinttok(rhs);
       };
    return op;
  }

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart)
  {  tok->tokentype = OPERATOR;  /* Make it look like an operator   */
     tok->whichval = IFOP;
     if (elsepart != NULL) elsepart->link = NULL;
     thenpart->link = elsepart;
     exp->link = thenpart;
     tok->operands = exp;
     if (DEBUG & DB_MAKEIF)
        { printf("makeif\n");
          dbugprinttok(tok);
          dbugprinttok(exp);
          dbugprinttok(thenpart);
          dbugprinttok(elsepart);
        };
     return tok;
   }

TOKEN makeprogn(TOKEN tok, TOKEN statements)
  {  tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
     return tok;
   }

int wordaddress(int n, int wordsize)
  { return ((n + wordsize - 1) / wordsize) * wordsize; }
 
yyerror(s)
  char * s;
  { 
  fputs(s,stderr); putc('\n',stderr);
  }

main()
  { int res;
    initsyms();
    res = yyparse();
    printst();
    printf("yyparse result = %8d\n", res);
    if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
    ppexpr(parseresult);           /* Pretty-print the result tree */
  }