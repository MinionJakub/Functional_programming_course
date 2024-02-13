%{
  open Ast
%}

/* Tokens */

/* Constants */
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING SYM

/* Variables */
%token <string> VAR

/* Symbols */
%token COLON_MINUS            /* :- */
%token QUESTION_MINUS         /* ?- */
%token DOT                    /* .  */
%token LPAREN                 /* (  */
%token RPAREN                 /* )  */
%token COMMA                  /* ,  */
%token MINUS                  /* -  */
%token PLUS                   /* +  */
%token MULT                   /* *  */
%token DIV                    /* /  */
%token TRUE
%token FALSE


/* Meta characters */
%token EOF

/* Start symbols */
%start program

/* Types */
%type <Ast.const> constant
%type <Ast.dec> statement
%type <Ast.exp>  term term_simple term_mult term_neg structure
%type <Ast.exp list>  term_list  
%type <Ast.program> program
%%

add_sym:
| PLUS { "+"}
| MINUS { "-"}
;

mult_sym:
| MULT { "*"}
| DIV { "/"}
;

symbol:
| SYM {$1}
;

constant:
| INT { (IntConst $1)}
| FLOAT { (FloatConst $1)}
| STRING { (StringConst $1)}
;

structure:
| symbol LPAREN RPAREN { TermExp($1,[])}
| symbol LPAREN term_list RPAREN {TermExp($1,$3)}
;

term_simple:
| TRUE {True}
| FALSE {False}
| constant { (ConstExp $1)}
| SYM { TermExp($1,[])}
| VAR { (VarExp $1)}
| structure {$1}
;

term:
| term_mult add_sym term { (TermExp($2,[$1;$3]))}
| term_mult {$1}
;

term_mult:
| term_neg mult_sym term_mult { TermExp($2,[$1;$3])}
| term_neg {$1}
;

term_neg:
| add_sym term_neg { (TermExp($1,[$2]))}
| term_simple {$1}
;

term_list:
| term {[$1]}
| term COMMA term_list {$1 :: $3}
;

statement:
| QUESTION_MINUS term_list DOT { (Query $2)}
// | term COLON_MINUS TRUE DOT {Fact $1}
| term COLON_MINUS term_list DOT { (Rule ($1,$3))}
| term DOT { (Rule ($1,[True]))}
;

statement_list_rev: 
| {[]}
| statement_list_rev statement {$2 :: $1}
;

statement_list:
| statement_list_rev {List.rev $1}
;

program:
| statement_list EOF {$1}
;