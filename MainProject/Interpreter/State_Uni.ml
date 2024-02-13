open Ast
open Parser
open Common
open StateMaybeMonad
open Robinson

let find_sub vars_list_string substitutions = List.filter (fun (v,_) ->
  match v with 
  | VarExp name -> List.exists (fun a -> String.equal name a) vars_list_string
  | _ -> false
) substitutions

type help_state = {goals : exp list; productions : dec list; substitutions : (exp * exp) list; orig_vars : string list}
module State = struct
  type t = help_state
  let make (v : help_state) : t =  v
  let empty = {goals = []; productions = []; substitutions = []; orig_vars = []}
end

module SMMonad = StateMaybeMonad(State);;
let ( let* ) = SMMonad.bind

let change_state_substitution substitution state = State.make {goals = state.goals; productions = state.productions; substitutions = substitution; orig_vars = state.orig_vars}
let change_state_goals goals state = State.make {goals = goals; productions = state.productions; substitutions = state.substitutions; orig_vars = state.orig_vars}
let change_state_goals_substitution goals substitution state = 
  change_state_goals goals (change_state_substitution substitution state)
let change_state_production production state = State.make {goals = state.goals;productions = production ;substitutions = state.substitutions; orig_vars = state.orig_vars}
(* let monad_map func m = 
  let* a = m in
  match a with
  | Some (value,state) -> SMMonad.return (func value)
  | None -> SMMonad.noValue *)

let unify_rule substitutions eval_query elem goal_list rule rest = 
  match (rename_vars_in_dec rule) with
  | Rule (head,body) -> (
    match unify [elem,head] with
    | Some substitution ->(
      match unify (substitution@substitutions) with
      | Some env -> (
        if (List.length body = 1) then (
          match body with
          (*if the rule proved the subgoal then recurse on remaining subgoals*)
          | (True :: ys) -> SMMonad.map2 (List.append) (eval_query (SMMonad.modify_state (change_state_goals_substitution (sub_lift_goals substitution goal_list) env))) rest
          (*if rule wasn't a fact then we have more subgoals from the body of the rule to prove*)
          | _ -> SMMonad.map2 (List.append) (eval_query (SMMonad.modify_state (change_state_goals_substitution ((sub_lift_goals substitution body) @ (sub_lift_goals substitution goal_list)) env))) rest
        )
          (*if rule wasn't a fact then we have more subgoals from the body of the rule to prove*)
          else SMMonad.map2 (List.append) (eval_query (SMMonad.modify_state (change_state_goals_substitution ((sub_lift_goals substitution body) @ (sub_lift_goals substitution goal_list)) env))) rest
      )
      | _ -> rest
    )
    | _ -> rest
  )
  | _ -> rest