(* open UnionFind_notFunctor *)
open UnionFind_notFunctor
open Errors_unify
type symbol = string
type term = term_constr uf_elem
and term_constr =
  | Var
  | Sym of symbol * term list

let uplift value = make value

let rec unify t1 t2 = 
  if eq t1 t2 then ()
  else let v1 = get t1 and v2 = get t2 and t = union t1 t2 in
  set t (match v1,v2 with
  | (Var,v) | (v,Var) -> v
  | (Sym(f1,ts1),Sym(f2,ts2)) -> (
    if ((f1 = f2) && (List.length ts1 = List.length ts2)) then 
      (List.iter2 unify ts1 ts2; Sym(f1,ts1)) else raise NotUnifiable
  )
  )

(* let  *)