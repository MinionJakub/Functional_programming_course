open Axiom

(*Equality Tests*)
let x = Axioms.compare 
(Axioms.make_singleton "a" (NumI 1)) 
(Axioms.make_singleton "a" (NumI 1));;
assert(x = 0);;


let x = Axioms.compare 
(Axioms.make_singleton "a" (NumI 1)) 
(Axioms.make_singleton "a" (NumF 1.0));;
assert(x = 0);;

let x = Axioms.compare 
(Axioms.make_singleton "a" (NumF 1.0)) 
(Axioms.make_singleton "a" (NumF 1.0));;
assert(x = 0);;


let x = Axioms.compare 
(Axioms.make_singleton "a" (StringV "a")) 
(Axioms.make_singleton "a" (StringV "a"));;
assert(x = 0);;

(*Diffrence Tests*)
let y = Axioms.compare
(Axioms.make_singleton "a" (NumI 1))
(Axioms.make_singleton "b" (NumI 1));;
assert(y = -1);;

let y = Axioms.compare
(Axioms.make_singleton "a" (NumI 1))
(Axioms.make_singleton "a" (NumF 1.1));;
assert(y = -1);;

let y = Axioms.compare
(Axioms.make_singleton "a" (NumI 1))
(Axioms.make_singleton "a" (StringV "a"));;
assert(y = -1);;

let y = Axioms.compare
(Axioms.make_singleton "a" (NumF 1.0))
(Axioms.make_singleton "a" (StringV "a"));;
assert(y = -1);;

let y = Axioms.compare
(Axioms.make_singleton "b" (NumI 1))
(Axioms.make_singleton "a" (NumI 1));;
assert(y = 1);;

let y = Axioms.compare
(Axioms.make_singleton "a" (NumF 1.1))
(Axioms.make_singleton "a" (NumI 1));;
assert(y = 1);;


let y = Axioms.compare
(Axioms.make_singleton "a" (StringV "a"))
(Axioms.make_singleton "a" (NumF 1.0));;
assert(y = 1);;

let y = Axioms.compare
(Axioms.make_singleton "a" (StringV "a"))
(Axioms.make_singleton "a" (NumI 1));;
assert(y = 1);;

