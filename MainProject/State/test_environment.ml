open Axiom
open Production
open Environment

(*Add element Tests*)
let environment = Environment.startEnv ();;
let x = (Axioms.make_singleton "a" (NumI 1));;
let environment = Environment.add_axiom x environment;;
assert(SetOfAxioms.mem x (Environment.get_axioms environment));;
let y = (Production.make_production (make_prod "a" ["x"]) [(make_prod "b" ["x"]);(make_prod "c" ["x"])]);;
let environment = Environment.add_production y environment;;
assert(SetOfProduction.mem y (Environment.get_production environment));;

(*Test of membership*)
let z = (Production.make_production (make_prod "a" ["x"]) [(make_prod "b" ["x"]);(make_prod "d" ["x"])]);;
assert((SetOfProduction.mem z (Environment.get_production environment)) == false);;

