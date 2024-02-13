open Ast
open Lexer
open Lexing
open YaccParser
open Errors

(* let rec printer value = 
  match value with
  | Var x -> print_endline ("Variable: " ^ x)
  | Num x -> print_endline ("Number: " ^ (string_of_int x))
  | Atom x -> print_endline ("Atom: " ^ x.data)
  | Sym (x,y) -> printer_node x ; List.fold_left (fun w z -> printer z) () (accumalte_term_data y []);; *)
  
let string_of_const value = 
  match value with 
  | IntConst x -> "Int: " ^ (string_of_int x)
  | FloatConst x -> "Float: " ^ string_of_float x
  | StringConst x -> "String: " ^ x

let rec string_of_exp value =
  match value with
  | True -> "True"
  | False -> "False"
  | Atom v ->  ("Atom:" ^ v)
  | ConstExp v ->  ("Constant: " ^ (string_of_const v))
  | VarExp v ->  ("Variable: " ^ v) ^ "\n"
  | TermExp(x,y) -> ((x ^ "(\n") ^ (List.fold_left (fun w z -> (w ^ (string_of_exp z))^"") "" y)) ^ ")";;

let string_of_dec value = 
  match value with 
  | Fact v -> "Fact: " ^ string_of_exp v
  | Rule(x,y) -> (("Rule: exp=" ^ string_of_exp x) ^ "\nPred=\n") ^ 
  (List.fold_left (fun w z -> w ^ ((string_of_exp z) ^ ",\n")) "" y)
  | Query x -> ("Query:" ^ (List.fold_left (fun w z -> w ^ ((string_of_exp z) ^ ",\n")) "" x));;

let rec string_of_program value = 
  match value with
  | [] -> ""
  | x :: xs -> (string_of_dec x) ^ ("\n" ^ (string_of_program xs));;

Parser.parse_query_string "cat(tome)." |> string_of_program |> print_endline;;
Parser.parse_query_string "animal(X) :- cat(X) ." |> string_of_program |> print_endline;;
Parser.parse_query_string "father(X,Y) :- male(X),parent(X,Y) ." |> string_of_program |> print_endline;;
Parser.parse_query_string "cat(tome) :- true." |> string_of_program |> print_endline;;
Parser.parse_query_string "cat(\"Tome\") :- true." |> string_of_program |> print_endline;;
Parser.parse_query_string "+1e10." |> string_of_program |> print_endline;;
Parser.parse_query_string "10." |> string_of_program |> print_endline;;
Parser.parse_query_string "inf." |> string_of_program |> print_endline;;
Parser.parse_query_string "ala." |> string_of_program |> print_endline;;
Parser.parse_query_string "X." |> string_of_program |> print_endline;;
Parser.parse_query_string "True." |> string_of_program |> print_endline;;
Parser.parse_query_string "False." |> string_of_program |> print_endline;;
Parser.parse_query_string "cat(tome). cat(finio). animal(X) :- cat(X). animal(X) :- dog(X)."
 |> string_of_program |> print_endline;;
Parser.parse_query_string "?- X+Y." |> string_of_program |> print_endline;;
Parser.parse_query_string "?- cat(tome)." |> string_of_program |> print_endline;;