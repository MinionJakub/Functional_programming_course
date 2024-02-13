open Axiom
open Production
open Environment

module type UnifierType =
sig
type t
(* val change_unifier : t -> elem -> t *)
end

module State (Unifier : UnifierType) = struct
  type t = {mutable environment : Environment.t; mutable unifier : Unifier.t}
  (*set environment and creates new state *)
  let set_environment env state = {environment = env; unifier = state.unifier}
  (*set unifier and creates new state *)
  let set_unifier unifier state = {environment = state.environment; unifier = unifier}
  (*changes environment and does not create new state*)
  let change_environment env state = state.environment <- env
  (*changes unifier and does not create new state*)
  let change_unifier unifier state = state.unifier <- unifier
  let get_environment state = state.environment
  let get_unifier state = state.unifier
end