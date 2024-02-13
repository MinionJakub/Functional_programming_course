open Axiom
open Production

let (fresh,reset,set) = 
  let nxt = ref 0 in
  let f () = (nxt := !nxt + 1; string_of_int (!nxt)) in
  let r () = nxt := 0 in
  let s v  = nxt := v in
  (f,r,s);;

module Prod_name_variable = 
struct
type t = {name : string; from_what : string option ; variable : string; result : string}
let compare a b = 
  let x = String.compare a.name b.name in
  if x != 0 then x else 
    let x,y = a.from_what,b.from_what in
    match x,y with
    | (None,Some _) -> 1
    | (Some _, None) -> -1
    | (None, None) -> String.compare a.variable b.variable
    | (Some x,Some y) -> let x = String.compare x y in
    if x != 0 then x else String.compare a.variable b.variable
  let make name from_what variable result = 
    {name = name; from_what = from_what; variable = variable; result = result}
end

let rec rename dict expresion
(* module SetOfProd_name_variable = Set.Make(Prod_name_variable) *)
module SetOfAxioms = Set.Make(Axioms)
module SetOfProduction = Set.Make(Production)
module Environment = struct
  type t = {axioms : SetOfAxioms.t; productions : SetOfProduction.t}
  let startEnv () : t = {axioms = SetOfAxioms.empty; productions = SetOfProduction.empty}
  let add_axiom (axiom : Axioms.t) (env : t) : t = {axioms = SetOfAxioms.add axiom env.axioms;productions = env.productions}
  let add_production (prod : Production.t) (env : t) : t = {axioms = env.axioms;productions = SetOfProduction.add prod env.productions}
  let get_axioms (env : t) : SetOfAxioms.t = env.axioms
  let get_production (env : t) : SetOfProduction.t = env.productions
  (* let make_the_lowest_production (querry : prod) env =
    let production_list = env.productions in *)
    
end