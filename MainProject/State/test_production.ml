open Production

(*Equality Test*)
let x = Production.compare
(Production.make_production (make_prod "a" ["x"]) [])
(Production.make_production (make_prod "a" ["x"]) []);;
assert(x = 0);;

let x = Production.compare
(Production.make_production (make_prod "a" ["x"]) [(make_prod "b" ["x"]);(make_prod "c" ["x"])])
(Production.make_production (make_prod "a" ["x"]) [(make_prod "c" ["x"]);(make_prod "b" ["x"])]);;
assert(x = 0);;


let x = Production.compare
(Production.make_production (make_prod "a" ["x"]) [(make_prod "b" ["x"]);(make_prod "b" ["x"]);(make_prod "c" ["x"])])
(Production.make_production (make_prod "a" ["x"]) [(make_prod "c" ["x"]);(make_prod "b" ["x"])]);;
assert(x = 0);;


(*Diffrence Test*)
let x = Production.compare
(Production.make_production (make_prod "a" ["x"]) [])
(Production.make_production (make_prod "b" ["x"]) []);;
assert(x = -1);;

let x = Production.compare
(Production.make_production (make_prod "a" ["x"]) [(make_prod "b" ["x"]);(make_prod "c" ["x"])])
(Production.make_production (make_prod "a" ["x"]) [(make_prod "b" ["x"]);(make_prod "d" ["x"])]);;
assert(x = -1);;
