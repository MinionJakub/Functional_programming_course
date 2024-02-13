open Errors_unify

type symbol = string
type var = term option ref
and term = 
| Var of var
| Sym of symbol * term list

let rec view t = 
  match t with 
  | Var x -> 
    begin match !x with
    | None -> t
    | Some t -> view t in
    x := Some t; t
  end
  | _ -> t

let rec unify t1 t2 = 
  match view t1, view t2 with 
  | Var x, Var y when x == y -> ()
  | Var x, t | t, Var x -> 
    if contains_var x t then raise NotUnifiable
    else x := Some t
  | Sym(f1,ts1),Sym(f2,ts2) ->
    if f1 = f2 && List.length ts1 = List.length ts2 then
      List.iter2 unify ts1 ts2
    else 
      raise NotUnifiable