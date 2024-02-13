{

  let raise_error (lexbuf : Lexing.lexbuf) reason =
  let pos =
    { Ast.start  = lexbuf.lex_start_p
    ; Ast.length = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_start_p.pos_cnum
    }
  in raise (Errors.Parse_error(pos, reason))

  let sym_map =
  let open YaccParser in
  [
    ":-", COLON_MINUS;
    "?-", QUESTION_MINUS;
    "." , DOT;
    "-" , MINUS;
    "+" , PLUS;
    "*" , MULT;
    "/" , DIV;
    "," , COMMA
  ] |> List.to_seq |> Hashtbl.of_seq

  let tokenize_sym str = 
  match Hashtbl.find_opt sym_map str with
  | None -> YaccParser.SYM str
  | Some tok -> tok

  let tokenize_num lexbuf str = 
  try  YaccParser.INT (int_of_string str) with
  | Failure _ -> raise_error lexbuf (InvalidNumber str)
}

let string_quote = '"'
let blank_space = [' ' '\t']
let any_character = [' '-'~']
let non_escape = any_character # ['\\']
let whitespace = ['\011'-'\r' '\t' ' ']
let escape = '\\'
let sym_char = 
[
  ';' ',' '=' '<' '>' '|' '&' '$' '#' '?'
  '!' '@' ':' '^' '.' '+' '-' '~' '*' '/'
]
let var_start = ['A'-'Z' '_']
let lower = ['a'-'z']
let digit = ['0'-'9']
let var_char = var_start | lower | digit
let sign = ['+' '-']
let digits = digit +
let integers = sign ? digits
let floats = integers '.' digits (['e' 'E'] sign ? digits) ? 
| integers ['e' 'E'] sign ? digits
let inf = '+' ? digits '.' digits "Inf" | '+' ? digits '.' digits "inf" | "inf" | "Inf"
let neg_inf = '-' digits '.' digits "Inf" | '-' digits '.' digits "inf" | "-inf" | "-Inf"
rule token = parse
    whitespace+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "/*" { block_comment lexbuf;   token lexbuf }
  | '%'  { skip_line lexbuf;       token lexbuf }
  | '('  { YaccParser.LPAREN    }
  | ')'  { YaccParser.RPAREN    }
  | "true" {YaccParser.TRUE}
  | "false" {YaccParser.FALSE}
  | "True" {YaccParser.TRUE}
  | "False" {YaccParser.FALSE}
  | integers as n {INT (int_of_string n)}
  | floats as f {FLOAT (float_of_string f)}
  | inf {FLOAT infinity}
  | neg_inf {FLOAT neg_infinity}
  | string_quote {strings "" lexbuf}
  | sym_char+           as x { tokenize_sym x   }
  | var_start var_char* as x { YaccParser.VAR x }
  | lower     var_char* as x { tokenize_sym x   }
  (* | digit     var_char* as x { tokenize_num lexbuf x } *)
  | eof    { YaccParser.EOF }
  | _ as x {
      raise_error lexbuf (InvalidChar x)
    }

and block_comment = parse
    '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
  | "*/" { () }
  | eof  {
      raise_error lexbuf EofInComment
    }
  | [^'\n' '*']+ { block_comment lexbuf }
  | '*'          { block_comment lexbuf }

and skip_line = parse
    '\n'     { Lexing.new_line lexbuf }
  | eof      { () }
  | [^'\n']+ { skip_line lexbuf }

and strings acc = parse
| string_quote blank_space* string_quote {strings acc lexbuf}
| string_quote {STRING acc}
| any_character # ['"'] + as s {strings (acc ^ s) lexbuf}
(* | escape {escaped atoms acc lexbuf} *)