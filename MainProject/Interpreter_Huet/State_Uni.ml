open Ast
open Parser
open Common
open StateMaybeMonad


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


