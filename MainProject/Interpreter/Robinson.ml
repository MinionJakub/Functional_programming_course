open Ast
(* open Parse_error *)
open Parser
open Common
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
