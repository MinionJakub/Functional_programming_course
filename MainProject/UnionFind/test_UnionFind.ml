open UnionFind
open Random

module IntUnionFind = UnionFind(Int);;

let create_random_int_list n = 
  let rec helper n acc = 
    if n < 1 then acc
    else helper (n - 1) ((IntUnionFind.make (Random.int Int.max_int)) :: acc)
  in helper n []
let create_int_list n = 
  let rec helper n acc =
    if n < 1 then acc
    else helper (n - 1) ((IntUnionFind.make n) :: acc)
  in helper n [];;
let rec i_elem_list n lista = 
  if n < 0 then assert false else 
  match n,lista with
  | (_,[]) -> assert false
  | (0,x::xs) -> x
  | (n,x::xs) -> i_elem_list (n-1) xs
let make_unary_operation func lista index = 
  func (i_elem_list index lista)
let make_binary_operation func lista index index2 = 
  (make_unary_operation func lista index) (i_elem_list index2 lista)
let nice_printer elem = 
  let x = IntUnionFind.find elem in
  let a,b = IntUnionFind.get2 elem, IntUnionFind.get x in
  print_endline ((string_of_int a) ^ "->" ^ (string_of_int b))
let rec map_until func n lista = 
  if n < 0 then assert false else 
    match n,lista with 
    | (_,[]) -> assert false
    | (0,x::xs) -> let y = (func x) in y :: []
    | (n,x::xs) -> let y = (func x) in y :: (map_until func (n-1) xs)



let n = 1000;;
let lista = create_int_list n;;
let _ = make_binary_operation IntUnionFind.union lista 0 1;;
let _ = (make_unary_operation IntUnionFind.find lista 1);;
let _ = assert (IntUnionFind.strong_eq (i_elem_list 0 lista) (make_unary_operation IntUnionFind.find lista 1))
(* let _ =  let x = i_elem_list 0 lista in let y = i_elem_list 1 lista in nice_printer x; nice_printer y;; *)
(* print_endline "";; *)
let _ = make_binary_operation IntUnionFind.union lista 1 2;;
let _ = make_binary_operation IntUnionFind.union lista 0 3;;
let _ = make_binary_operation IntUnionFind.union lista 4 5;;
let _ = make_binary_operation IntUnionFind.union lista 5 6;;
let _ = make_binary_operation IntUnionFind.union lista 7 5;;
(* let _ = map_until nice_printer 7 lista;; *)
(* print_endline "";; *)
let _ = assert ((IntUnionFind.strong_eq (make_unary_operation IntUnionFind.find lista 5) (make_unary_operation IntUnionFind.find lista 1)) = false)
let _ = make_binary_operation IntUnionFind.union lista 5 1;;
let _ = assert (IntUnionFind.strong_eq (make_unary_operation IntUnionFind.find lista 5) (make_unary_operation IntUnionFind.find lista 1))
let _ = assert ((IntUnionFind.strong_eq (i_elem_list 0 lista) (i_elem_list 5 lista)) = false)
let _ = assert (IntUnionFind.eq (i_elem_list 0 lista) (i_elem_list 5 lista))
(* let _ = map_until nice_printer 7 lista;; *)


