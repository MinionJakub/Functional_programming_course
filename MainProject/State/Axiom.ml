type values = 
| NumI of int
| NumF of float
| StringV of string

let rec compare_values x y =
  match x,y with
  | (NumI v1, NumI v2) -> Int.compare v1 v2
  | (NumI v1, NumF v2) -> Float.compare (float_of_int v1) v2
  | (NumI v1, StringV v2) -> -1
  | (NumF v1, NumF v2) -> Float.compare v1 v2
  | (NumF v1, StringV v2) -> -1
  | (StringV v1, StringV v2) -> String.compare v1 v2
  | (_,_) -> -1 * (compare_values y x)

let rec compare_values_list x y = 
  match x,y with
  | ([],[]) -> 0
  | (xs,[]) -> 1
  | ([],ys) -> -1
  | (x::xs,y::ys) -> let comp_val = compare_values x y in 
  if comp_val = 0 then compare_values_list xs ys else comp_val

module Axioms = struct
  type t = string * values list
  let compare a b =
    match (a,b) with
    | ((name1,v1),(name2,v2)) -> 
      let compare_value = String.compare name1 name2 in 
      if compare_value = 0 then compare_values_list v1 v2 else compare_value
  let make_singleton name value : t = (name,[value])
  let make_axiom name values : t = (name,values)
end
