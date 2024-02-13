module type Comparable = sig
  type t
  val compare : t -> t -> int
end
module UnionFind (Data : Comparable) = struct
  type t = {mutable parent : t option;mutable value : Data.t ;mutable rank : int}
  let uplift_to_union_find (value : Data.t) : t = {parent = None ; value = value; rank = 1}
  let make = uplift_to_union_find
  let rec find elem =
    match elem.parent with
    | None -> elem
    | Some x -> let y = find x in elem.parent <- Some y; y
  let union elem1 elem2 = 
    let make_child x y = x.parent <- Some y; y.rank <- (x.rank + y.rank); y in
    let x = find elem1 in
    let y = find elem2 in
    if x.rank < y.rank then (make_child x y)
    else if y.rank < x.rank then (make_child y x)
    else if (Data.compare x.value y.value) = 0 then x else (make_child y x)
  let get elem =
    match elem.parent with
    | None -> elem.value
    | Some _ -> assert false
  let get2 elem =
    elem.value
  let set elem value = 
    match elem.parent with
    | None -> elem.value <- value
    | Some _ -> assert false 
  let rec strong_eq elem1 elem2 =
    elem1.rank = elem2.rank && ((Data.compare elem1.value elem2.value) = 0) && (
      let x,y = elem1.parent,elem2.parent in
      match x,y with
      | (None,None) -> true
      | (None,Some _) -> false
      | (Some _, None) -> false 
      | (Some a, Some b) -> let c = find a in let d = find b in strong_eq c d
    )
  let eq elem1 elem2 = 
    (strong_eq elem1 elem2) || (strong_eq (find elem1) (find elem2))
end
