open Ast
open Common
open Lexer
open Parser
open Evaluator

let is_interactive = 0 = (Sys.command "[ -t 0 ]")
let _ = 
  let rec loop db = (
    try (
      let value = read_line () in
      let dec = parse_query_string value in 
      let new_db = eval_dec (List.hd dec,db)
      in loop new_db
    ) with _ -> loop db
  ) in loop []
