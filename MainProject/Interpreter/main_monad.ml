open Ast
open Common
open Lexer
open Parser
open Evaluator_monad
open State_Uni

let is_interactive = 0 = (Sys.command "[ -t 0 ]")
let _ = 
  let m = SMMonad.return () in
  let rec loop db = (
    try (
      let value = read_line () in
      let dec = parse_query_string value in 
      match (List.hd dec) with
      | Query _ -> 
        let new_db = eval_dec (List.hd dec) db 
        in let _ =  new_db State.empty  in loop db
      | _ -> 
        let new_db = eval_dec (List.hd dec) db 
        in loop new_db
    ) with _ -> loop db
  ) in loop m
