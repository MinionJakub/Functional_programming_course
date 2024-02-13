type prod = {name : string ; params : string list}

let make_prod name params : prod = {name = name; params = params}

let compare_prod a b =
  let x = String.compare a.name b.name in 
  if x = 0 then Int.compare (List.length a.params) (List.length b.params) else x

let rec compare_prod_list x y = 
  match x,y with
  | ([],[]) -> 0
  | (xs,[]) -> 1
  | ([],ys) -> -1
  | (x::xs,y::ys) -> let comp_val = compare_prod x y in
  if comp_val = 0 then compare_prod_list xs ys else comp_val


module Production = struct 
  type t = prod * prod list
  let compare a b =
    match a,b with
    | ((start1,prod1),(start2,prod2)) -> 
      let y = compare_prod start1 start2 in 
      if y = 0 then compare_prod_list prod1 prod2 else y
  let make_production start products : t = (start, List.sort_uniq compare_prod products)
end
