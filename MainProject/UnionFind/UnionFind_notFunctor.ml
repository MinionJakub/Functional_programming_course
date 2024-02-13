type rank = int
type 'a uf_elem = {mutable parent : 'a uf_elem option ; mutable value : 'a ; mutable rank : rank}
let make (v : 'a) : 'a uf_elem = {parent = None; value = v ; rank = 1}
let rec find (node : 'a uf_elem) : 'a uf_elem = 
  match node.parent with
  | None -> node
  | Some x -> let y = find x in node.parent <- Some y; y
let is_representative (node : 'a uf_elem) : bool = 
  match node.parent with
  | None -> true
  | Some _ -> false
let eq (x : 'a uf_elem) (y : 'a uf_elem) : bool =
  x == y || find x == find y
let strong_eq (x : 'a uf_elem) (y : 'a uf_elem) : bool =
  x == y
let get (x : 'a uf_elem) : 'a =
  match x.parent with
  | None -> x.value
  | Some _ -> assert false
let get2 (x : 'a uf_elem) : 'a = x.value
let set (x : 'a uf_elem) (v : 'a) : unit = 
  let x = find x in x.value <- v
let union (x : 'a uf_elem) (y : 'a uf_elem) : 'a uf_elem =
  let make_child x y = x.parent <- Some y; y.rank <- (x.rank + y.rank); y in
  let x = find x in let y = find y in
  if x == y then x
  else if x.rank < y.rank then make_child x y else make_child y x
let merge (func : 'a -> 'a -> 'a) (x : 'a uf_elem) (y : 'a uf_elem) : 'a uf_elem = 
  let make_child x y = x.parent <-Some y; y.rank <- (x.rank + y.rank); y in
  let x = find x and y = find y in if x == y then x
  else let v = func x.value y.value in if x.rank < y.rank
    then begin let w = make_child x y in set w v; w end
    else begin let w = make_child y x in set w v; w end
