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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
