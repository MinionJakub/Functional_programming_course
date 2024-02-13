open Unifier_Huet

let x = uplift (Sym("father",[(uplift Var);uplift Var]))
let y = uplift (Sym("father",[(uplift (Sym("Jan",[])));uplift Var]))
let _ = unify x y
let w = uplift (Sym("father",[(uplift Var);(uplift Var);(uplift Var)]))
(* let _ = unify w x *)
let x = try unify w x with _ -> ()
(* let _ = assert (x == y) *)