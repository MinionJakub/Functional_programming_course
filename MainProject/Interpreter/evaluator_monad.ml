open Ast
open Parser
open Common
open StateMaybeMonad
open Robinson
open State_Uni

let rec eval_query m =
  let* a = m in
  let* state = SMMonad.get_state in
  match state.goals with
  (*No more goals to prove*)
  | [] -> SMMonad.return [state.substitutions]
  (*At least one goal to prove*)
  | (goal_elem :: goal_rest) -> 
    (
      let vars_list_string = (find_vars_string state.goals) @ state.orig_vars |> uniq in
      let substitutions = find_sub vars_list_string state.substitutions in
      match goal_elem with
      | True -> eval_query (SMMonad.modify_state (change_state_goals_substitution goal_rest substitutions))
      | TermExp(_,_) -> List.fold_right (unify_rule substitutions eval_query goal_elem goal_rest) state.productions (SMMonad.return [])
      (*subgoal isn't term exp*)
      | _ -> eval_query (SMMonad.modify_state (change_state_goals_substitution goal_rest substitutions))
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

  
let rec add_dec_to_db dec db =
  match dec with
  | Fact e -> (
    match e with 
    | True -> db
    | _ -> add_dec_to_db (Rule(e,[])) db
  )
  | Rule (h,b) -> (
    match h with
    | True -> db
    | _ -> dec :: db
  )
  | Query (b) -> (
    dec :: db
  )

let rec add_dec_to_state dec state =
  match dec with
  | Fact e -> (
    match e with 
    | True -> state
    | _ -> add_dec_to_state (Rule(e,[])) state
  )
  | Rule (h,b) -> (
    match h with
    | True -> state
    | _ -> (State.make {goals = state.goals; productions = dec :: state.productions; substitutions = state.substitutions; orig_vars = state.orig_vars})
  )
  | _ -> state

let eval_dec dec m = 
  let* a = m in
  let* state = SMMonad.get_state in
  match dec with
  | Query b -> begin let orig_vars = uniq (find_vars b) in 
    let orig_vars_string = find_vars_string b |> uniq in 
    let orig_vars_num = List.length orig_vars in 
    let v = (State.make {goals = b;productions =  state.productions; substitutions = state.substitutions; orig_vars = orig_vars_string}) in 
    let res = eval_query (SMMonad.modify_state (fun x -> v)) in 
    let runned = (res State.empty) in 
    match runned with 
    | None -> m
    | Some (lista,_) ->
    print_string (string_of_res lista orig_vars orig_vars_num);  m
  end
  | _ -> SMMonad.modify_state (fun x -> add_dec_to_state dec state)
