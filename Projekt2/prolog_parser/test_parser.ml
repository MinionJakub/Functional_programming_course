 (* parse *)
(* # open "./parser.ml" *)
(* #include parser *)
(* Parser.parse_file "test_file2.pl" *)

open Ast
open Lexer
open Lexing
open YaccParser
open Errors

let printer_node x = print_endline ("Symbol name: "^x.data);;
let rec accumalte_term_data x y = match x with
| [] -> y
| x::xs -> (x.data)::(accumalte_term_data xs y) 

let rec string_of_token x = 
  match x with
  | VAR x ->  "Variable: " ^ x
  | SYM x -> "Symbole: " ^ x
  | NUM x -> "Number: " ^ (string_of_int x)
  | BR_OPN -> "BR_OPEN: ("
  | BR_CLS -> "BR_CLOSE: )"
  | ASTERISK -> "ASTERISK: *"
  | COMMA -> "COMMA: ,"
  | DOT -> "DOT: ."
  | MINUS -> "MINUS: -"
  | PLUS -> "PLUS: +"
  | SLASH -> "SLASH: /"
  | COLON_MINUS -> "COLON_MINUS: :-"
  | IS -> "IS: is"
  | EOF -> "EOF"
  

let rec printer value = 
match value with
| Var x -> print_endline ("Variable: " ^ x)
| Num x -> print_endline ("Number: " ^ (string_of_int x))
| Atom x -> print_endline ("Atom: " ^ x.data)
| Sym (x,y) -> printer_node x ; List.fold_left (fun w z -> printer z) () (accumalte_term_data y []);;

let token_printer lex_buffer = 
  let rec _helper lex_buffer return =
    let y = string_of_token (Lexer.token lex_buffer) in 
    if (String.compare y "EOF") == 0 then return
    else y ^ "\n" ^ (_helper lex_buffer return)
  in print_endline (_helper lex_buffer "");;

print_endline "Test1\n";;
let lex_buffer = Lexing.from_string "cat(tom).";;
token_printer lex_buffer;;
List.fold_left (fun y x -> printer x) () (accumalte_term_data (Parser.parse_query_string "cat(tom).") []);;
print_endline "Test2\n";;
let lex_buffer = Lexing.from_string "cat(finio) :- true.";;
token_printer lex_buffer;;
let lex_buffer = Lexing.from_string "cat(finio) :- true.";;
YaccParser.query Lexer.token lex_buffer;;

(* YaccParser.query Lexer.token (Lexing.from_string "?- cat(tom).");; *)
(* Parser.parse_query_string("animal(X) :- cat(X).");; *)
(* Parser.parse_query_string("ojciec(X, Y) :- rodzic(X, Y), jest_rodzaju_męskiego(X).");; *)
(* print_endline "Test3\n";; *)
(* Parser.parse_query_string "animal(X) :- cat(X).";; *)
