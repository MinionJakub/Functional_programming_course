open Ast
open Common
open Lexer
open Parser
open Evaluator_monad
open State_Uni

let is_interactive = 0 = (Sys.command "[ -t 0 ]")
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
  ) 
let rec loop_c db channel = (
    try (
      try 
        let value = input_line channel in
        let dec = parse_query_string value in 
        match (List.hd dec) with
        | Query _ -> 
          let new_db = eval_dec (List.hd dec) db 
          in let _ = print_endline value ;new_db State.empty  in loop_c db channel
        | _ -> 
          let new_db = eval_dec (List.hd dec) db 
          in loop_c new_db channel
      with End_of_file -> db 
    ) with _ -> loop_c db channel
  )

let unpack rest = match rest with
| None -> SMMonad.return ()
| Some(x,y) -> SMMonad.modify_state (fun _ -> y)

let _ = 
  let num_args = Array.length Sys.argv in
  if num_args = 1 then loop (SMMonad.return ())
  else(
    try 
      let ic = open_in Sys.argv.(1) in 
      let result = loop_c (SMMonad.return ()) ic in
      let result = result State.empty in
      let stan = unpack result in
      let _ = close_in ic in 
      loop stan
    with _ -> print_endline "couldn't read a file" ;loop (SMMonad.return ())
  ) 
    (* in loop m *)