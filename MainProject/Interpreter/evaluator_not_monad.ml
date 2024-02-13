open Ast
open Parser
open Common
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
(*
  sub_lift_goal
  * takes list of substitution for variables and a goal of type exp
  * returns the goal with the substitution applied 
*)
let rec sub_lift_goal substitution goal = 
  match goal with
  | True -> goal
  | False -> goal
  | Atom v -> goal
  | VarExp v -> begin
    try let i = List.assoc goal substitution in i
    with Not_found -> VarExp v
  end
  | ConstExp x ->  goal
  | TermExp (name,lista) -> TermExp (name,List.map (fun new_goal -> sub_lift_goal substitution new_goal) lista)

(*
  sub_lift_goals
  * takes list of substitution for varables and list of goals of type exp
  * returns list of goals after substitution each of type exp   
*)
let sub_lift_goals substitution goals =
  List.map (fun goal -> sub_lift_goal substitution goal) goals

(*
  create_sub_list
  * takes list of expresions
  * returns substitution list where every unique variable has unique name 
*)  
let create_sub_list lista =
  let lista_vars = find_vars lista in 
  let vars = uniq lista_vars in
  let sub = List.map (fun x -> (x,VarExp(fresh()))) vars in 
  sub
(*
  rename_vars_in_dec
  * takes a dec type
  * returns a dec with all the variables in d renamed to fresh variable names   
*)
let rec rename_vars_in_dec value =
  match value with
  | Rule (head,body) -> 
    let sub = create_sub_list (head :: body) in
    Rule (sub_lift_goal sub head, sub_lift_goals sub body)
  | Query (body) -> 
    let sub = create_sub_list body in 
    Query (sub_lift_goals sub body)
  | _ -> value

(*
  pair_and_cat
  takes: two list of exps and list of constraints where each constrain is type of (exp * exp)
  returns a new list of constrains where c is prepended with each entry from first and second list
*)
let rec pair_and_cat sargs targs constrains = 
  match sargs,targs with
  | ([],[]) -> constrains
  | (x::xs,y::ys) -> pair_and_cat xs ys ((x,y)::constrains)
  | _ -> raise (Failure "Sargs and Targs should be the same length")

(*
  replace
  * takes: List of constrains and a list of substitution
  * returns: A new list of constrains where the substitutions are applied to both side of each constraints   
*)
let rec replace constraints substitutions =
  match constraints with
  | [] -> []
  | ((s,t)::rest) -> 
    (sub_lift_goal substitutions s,sub_lift_goal substitutions t) :: 
    (replace rest substitutions)

(*
  occurs 
  * takse: a string and expresion
  * returns: true if a string match any variable name in expression false otherwise
*)
let rec occurs value expression =
  match expression with 
  | VarExp m -> value = m
  | TermExp (name,lista) ->
    List.fold_left (fun acc v -> acc || (occurs value v)) false lista
  | _ -> false 

let rec unify constrains =
  match constrains with
  | [] -> Some []
  | ((s,t):: c') ->
    if s = t then unify c'
    else (
      match s with 
      | VarExp(n) -> 
        if (occurs n t) then None 
        else let sub = [(s,t)] in
        let c'' = replace c' sub in 
        let phi = unify c'' in (
          match phi with
          | None -> None
          | Some l -> Some((s,sub_lift_goal l t) :: l)
        )
      | TermExp (sname,sargs) -> (
        match t with
        | VarExp k -> unify ((t,s) :: c')
        | TermExp (tname,targs) -> 
          if (tname = sname && List.length targs = List.length sargs) then unify (pair_and_cat sargs targs c')
          else None
        | _ -> None
      )
      | _ -> (
        match t with 
        | VarExp k -> unify ((t,s) :: c')
        | _ -> None
      )
    )

let rec eval_query (q,db,env) orig_vars =
  (* print_endline "I am here\n"; *)
  match q with
  | [] -> [env]
  | (q_e :: q_list) -> (
    let vars_list_string = (find_vars_string q) @ orig_vars |> uniq in
    let env = List.filter (fun (v,_) -> match v with 
    | VarExp elt -> List.exists (fun a -> String.equal elt a) vars_list_string
    | _ -> false
    ) env in match q_e with 
    | True -> (eval_query (q_list,db,env) orig_vars)
    | TermExp(_,_) -> (
      List.fold_right (fun rule r -> (
        match (rename_vars_in_dec rule) with
        | Rule (head,body) ->(
          match unify [q_e,head] with
          | Some s -> (
            match unify (s@env) with
            | Some env2 -> (
              if (List.length body = 1) then (
                match body with
                | (True :: ys) -> ((eval_query  ((sub_lift_goals s body) @ (sub_lift_goals s q_list),db,env2) orig_vars) @ r)
                | _ -> ((eval_query  ((sub_lift_goals s body) @ (sub_lift_goals s q_list),db,env2) orig_vars) @ r) 
              )
              else ((eval_query  ((sub_lift_goals s body) @ (sub_lift_goals s q_list),db,env2) orig_vars) @ r)
            )
            | _ -> r
          )
          | _ -> r
        )
        | _ -> r
      )
      ) db []
    )
    | _ -> eval_query (q_list,db,env) orig_vars
  )

let string_of_res e orig_query_vars orig_vars_num = 
  List.fold_left (
    fun r2 env -> 
      if orig_vars_num > 0 
        then 
          "===============\n" ^
          (List.fold_left (fun r d -> (
            match d with
            | VarExp v -> (
              try let f = List.assoc (VarExp v) env in (
                match f with 
                | VarExp v2 -> 
                  (v ^ " is free\n") ^ r
                | _ -> (v ^ " = " ^ string_of_exp f ^ "\n") ^ r
              ) with Not_found -> (v ^ "is free\n") ^ r
            )
            | _ -> r
            )
            ) "" (orig_query_vars)) ^ "===============\n" ^ r2
        else "" ^ r2
  ) (if List.length e > 0 then "true\n" else "false\n") e

let rec add_dec_to_db (dec,db) =
  match dec with
  | Fact e -> (
    match e with 
    | True -> db
    | _ -> add_dec_to_db (Rule(e,[]),db)
  )
  | Rule (h,b) -> (
    match h with
    | True -> db
    | _ -> dec :: db
  )
  | Query (b) -> (
    dec :: db
  )

let eval_dec (dec,db) =
  (* print_endline "Hello there!\n"; *)
  match dec with 
  | Query b -> (
    let orig_vars = uniq (find_vars b) in
    let orig_vars_string = find_vars_string b |> uniq in
    let orig_vars_num = List.length orig_vars in
    let res = eval_query (b,db,[]) orig_vars_string in
    print_string (string_of_res (res) orig_vars orig_vars_num);
    reset();
    db
  )
  | _ -> add_dec_to_db (dec,db)