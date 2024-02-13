open Ast
open Parser

let string_of_const value = 
  match value with 
  | IntConst x -> "Int: " ^ (string_of_int x)
  | FloatConst x -> "Float: " ^ string_of_float x
  | StringConst x -> "String: \"" ^ x ^ "\""

let rec string_of_exp value =
  match value with
  | True -> "True"
  | False -> "False"
  | Atom v ->  ("Atom:" ^ v)
  | ConstExp v ->  ("Constant: " ^ (string_of_const v))
  | VarExp v ->  ("Variable: \"" ^ v ^ "\"")
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


(*
  fresh :
    * takes unit 
    * returns a string of the increment of the counter
  reset :
    * takes unit
    * returns unit and resets the counter
  set :
    * takes int 
    * returns unit and set the counter to the given value
*)
let (fresh,reset,set) = 
  let nxt = ref 0 in
  let f () = (nxt := !nxt + 1; string_of_int (!nxt)) in
  let r () = nxt := 0 in
  let s v  = nxt := v in
  (f,r,s);;


(*
  Find vars: takes in list of expression and return a list of all varexp in the list
*)
let find_vars question =
  let rec _find_vars question acc = 
    match question with
    | [] -> acc
    | x :: xs -> (
      match x with 
      | VarExp v -> _find_vars xs (x :: acc)
      | ConstExp c -> _find_vars xs acc
      | True -> _find_vars xs acc
      | False -> _find_vars xs acc
      | Atom s -> _find_vars xs acc
      | TermExp (s,el) -> (_find_vars el []) @ (_find_vars xs acc)
    )
in _find_vars question [];;

(*
  Uniq: takes a list and returns the reversed with only uniq copy of each element   
*)
let uniq l = 
  let rec tail_uniq a l = 
    match l with
    | [] -> a
    | hd :: tl -> 
      tail_uniq (hd :: a) (List.filter (fun x -> (x <> hd)) tl)
in tail_uniq [] l;;

(*
  find_vars_string:
  * takes a list of exp
  * returns a list of names of variables
*)
let find_vars_string question =
  let rec _find_vars_string question acc = 
    match question with
    | [] -> acc
    | x :: xs -> (
      match x with 
      | VarExp v -> _find_vars_string xs (v :: acc)
      | ConstExp c -> _find_vars_string xs acc
      | True -> _find_vars_string xs acc
      | False -> _find_vars_string xs acc
      | Atom s -> _find_vars_string xs acc
      | TermExp (s,el) -> (_find_vars_string el []) @ (_find_vars_string xs acc)
    ) 
  in _find_vars_string question [];;

(*
  get_queries_and_rules_exp
  * takes program
  *returns all expresions within queries and rules
*)
let get_queries_and_rules_exp lista = 
  let rec _get_queries_and_rules lista acc = 
    match lista with 
    | [] -> acc
    | x :: xs -> (
      match x with
      | Query ys -> _get_queries_and_rules xs (ys::acc)
      | Rule (val1,val2) -> _get_queries_and_rules xs ((val1 :: val2)  :: acc)
      | _ -> _get_queries_and_rules xs acc
    )
  in _get_queries_and_rules lista [];;

(*
  get_queries
  * takes program 
  * returns all queries
*)
let get_queries program = 
  let rec _get_queries program acc = 
    match program with
    | [] -> acc
    | x :: xs -> (
      match x with
      | Query _ -> _get_queries xs (x :: acc)
      | _ -> _get_queries xs acc
    )
  in _get_queries program [];;

(*
  get_rules
  * takes program
  * returns all rules
*)
let get_rules program = 
  let rec _get_rules program acc =
    match program with 
    | [] -> acc
    | x :: xs -> (
      match x with 
      | Rule _ -> _get_rules xs (x :: acc)
      | _ -> _get_rules xs acc
    )
  in _get_rules program [];;

(*
  get_facts 
  * takes program
  * return all rules
*)
(* let get_facts program =
  let rec _get_facts program acc = 
    match program with 
    | [] -> acc
    | x :: xs -> (
      match x with 
      | Fact _ -> _get_facts (x :: acc)
      | _ -> get_facts xs acc
    )
  in _get_facts program [];; *)
