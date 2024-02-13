type token =
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | SYM of (string)
  | VAR of (string)
  | COLON_MINUS
  | QUESTION_MINUS
  | DOT
  | LPAREN
  | RPAREN
  | COMMA
  | MINUS
  | PLUS
  | MULT
  | DIV
  | TRUE
  | FALSE
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "yaccParser.mly"
  open Ast
# 26 "yaccParser.ml"
let yytransl_const = [|
  262 (* COLON_MINUS *);
  263 (* QUESTION_MINUS *);
  264 (* DOT *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* COMMA *);
  268 (* MINUS *);
  269 (* PLUS *);
  270 (* MULT *);
  271 (* DIV *);
  272 (* TRUE *);
  273 (* FALSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  260 (* SYM *);
  261 (* VAR *);
    0|]

let yylhs = "\255\255\
\010\000\010\000\011\000\011\000\012\000\002\000\002\000\002\000\
\008\000\008\000\005\000\005\000\005\000\005\000\005\000\005\000\
\004\000\004\000\006\000\006\000\007\000\007\000\009\000\009\000\
\003\000\003\000\003\000\013\000\013\000\014\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\004\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\001\000\003\000\001\000\002\000\001\000\001\000\003\000\
\003\000\004\000\002\000\000\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\028\000\000\000\032\000\000\000\000\000\006\000\007\000\008\000\
\000\000\015\000\000\000\002\000\001\000\011\000\012\000\013\000\
\029\000\000\000\022\000\000\000\000\000\016\000\000\000\000\000\
\031\000\000\000\000\000\000\000\027\000\000\000\003\000\004\000\
\000\000\021\000\000\000\000\000\025\000\000\000\017\000\019\000\
\009\000\000\000\024\000\026\000\010\000"

let yydgoto = "\002\000\
\003\000\016\000\017\000\026\000\019\000\020\000\021\000\022\000\
\027\000\023\000\033\000\024\000\004\000\005\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\000\255\014\000\000\000\000\000\000\000\
\000\000\000\000\034\255\000\000\000\000\000\000\000\000\000\000\
\000\000\002\255\000\000\011\255\028\255\000\000\034\255\006\255\
\000\000\014\255\020\255\034\255\000\000\034\255\000\000\000\000\
\034\255\000\000\017\255\034\255\000\000\023\255\000\000\000\000\
\000\000\022\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\000\000\
\046\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\064\255\056\255\000\000\000\000\000\000\
\000\000\001\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\252\255\000\000\011\000\022\000\000\000\
\013\000\033\000\000\000\000\000\000\000\000\000"

let yytablesize = 75
let yytable = "\018\000\
\006\000\007\000\008\000\009\000\010\000\001\000\011\000\028\000\
\023\000\029\000\023\000\012\000\013\000\025\000\035\000\014\000\
\015\000\006\000\007\000\008\000\009\000\010\000\012\000\013\000\
\036\000\039\000\041\000\037\000\012\000\013\000\044\000\045\000\
\014\000\015\000\006\000\007\000\008\000\009\000\010\000\030\000\
\038\000\031\000\032\000\040\000\034\000\012\000\013\000\042\000\
\043\000\014\000\015\000\014\000\030\000\014\000\005\000\014\000\
\014\000\014\000\014\000\014\000\014\000\020\000\000\000\020\000\
\000\000\020\000\020\000\020\000\020\000\018\000\000\000\018\000\
\000\000\018\000\018\000"

let yycheck = "\004\000\
\001\001\002\001\003\001\004\001\005\001\001\000\007\001\006\001\
\008\001\008\001\010\001\012\001\013\001\000\000\009\001\016\001\
\017\001\001\001\002\001\003\001\004\001\005\001\012\001\013\001\
\011\001\030\000\010\001\008\001\012\001\013\001\008\001\010\001\
\016\001\017\001\001\001\002\001\003\001\004\001\005\001\000\000\
\028\000\014\001\015\001\033\000\023\000\012\001\013\001\035\000\
\036\000\016\001\017\001\006\001\020\000\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\006\001\255\255\008\001\
\255\255\010\001\011\001\012\001\013\001\006\001\255\255\008\001\
\255\255\010\001\011\001"

let yynames_const = "\
  COLON_MINUS\000\
  QUESTION_MINUS\000\
  DOT\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  MINUS\000\
  PLUS\000\
  MULT\000\
  DIV\000\
  TRUE\000\
  FALSE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  STRING\000\
  SYM\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "yaccParser.mly"
       ( "+")
# 150 "yaccParser.ml"
               : 'add_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "yaccParser.mly"
        ( "-")
# 156 "yaccParser.ml"
               : 'add_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "yaccParser.mly"
       ( "*")
# 162 "yaccParser.ml"
               : 'mult_sym))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "yaccParser.mly"
      ( "/")
# 168 "yaccParser.ml"
               : 'mult_sym))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "yaccParser.mly"
      (_1)
# 175 "yaccParser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "yaccParser.mly"
      ( (IntConst _1))
# 182 "yaccParser.ml"
               : Ast.const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 60 "yaccParser.mly"
        ( (FloatConst _1))
# 189 "yaccParser.ml"
               : Ast.const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "yaccParser.mly"
         ( (StringConst _1))
# 196 "yaccParser.ml"
               : Ast.const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbol) in
    Obj.repr(
# 65 "yaccParser.mly"
                       ( TermExp(_1,[]))
# 203 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp list) in
    Obj.repr(
# 66 "yaccParser.mly"
                                 (TermExp(_1,_3))
# 211 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "yaccParser.mly"
       (True)
# 217 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "yaccParser.mly"
        (False)
# 223 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.const) in
    Obj.repr(
# 72 "yaccParser.mly"
           ( (ConstExp _1))
# 230 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "yaccParser.mly"
      ( TermExp(_1,[]))
# 237 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "yaccParser.mly"
      ( (VarExp _1))
# 244 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 75 "yaccParser.mly"
            (_1)
# 251 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'add_sym) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 79 "yaccParser.mly"
                         ( (TermExp(_2,[_1;_3])))
# 260 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 80 "yaccParser.mly"
            (_1)
# 267 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mult_sym) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 84 "yaccParser.mly"
                              ( TermExp(_2,[_1;_3]))
# 276 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 85 "yaccParser.mly"
           (_1)
# 283 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'add_sym) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 89 "yaccParser.mly"
                   ( (TermExp(_1,[_2])))
# 291 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 90 "yaccParser.mly"
              (_1)
# 298 "yaccParser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 94 "yaccParser.mly"
       ([_1])
# 305 "yaccParser.ml"
               : Ast.exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp list) in
    Obj.repr(
# 95 "yaccParser.mly"
                       (_1 :: _3)
# 313 "yaccParser.ml"
               : Ast.exp list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp list) in
    Obj.repr(
# 99 "yaccParser.mly"
                               ( (Query _2))
# 320 "yaccParser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp list) in
    Obj.repr(
# 101 "yaccParser.mly"
                                 ( (Rule (_1,_3)))
# 328 "yaccParser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 102 "yaccParser.mly"
           ( (Rule (_1,[True])))
# 335 "yaccParser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "yaccParser.mly"
  ([])
# 341 "yaccParser.ml"
               : 'statement_list_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list_rev) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.dec) in
    Obj.repr(
# 107 "yaccParser.mly"
                               (_2 :: _1)
# 349 "yaccParser.ml"
               : 'statement_list_rev))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement_list_rev) in
    Obj.repr(
# 111 "yaccParser.mly"
                     (List.rev _1)
# 356 "yaccParser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 115 "yaccParser.mly"
                     (_1)
# 363 "yaccParser.ml"
               : Ast.program))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
