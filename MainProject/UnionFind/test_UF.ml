open UnionFind_notFunctor

type int_uf = int uf_elem;;

let create_int_list n = 
  let rec helper n acc =
    if n < 1 then acc
    else helper (n - 1) ((make n) :: acc)
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
  let x = find elem in
  let a,b = get2 elem, get x in
  print_endline ((string_of_int a) ^ "->" ^ (string_of_int b))
let rec map_until func n lista = 
  if n < 0 then assert false else 
    match n,lista with 
    | (_,[]) -> assert false
    | (0,x::xs) -> let y = (func x) in y :: []
    | (n,x::xs) -> let y = (func x) in y :: (map_until func (n-1) xs) 

let n = 1000;;
let lista = create_int_list n;;
let _ = make_binary_operation union lista 0 1;;
let _ = (make_unary_operation find lista 1);;
let _ = assert (strong_eq (i_elem_list 0 lista) (make_unary_operation find lista 1))
(* let _ =  let x = i_elem_list 0 lista in let y = i_elem_list 1 lista in nice_printer x; nice_printer y;; *)
(* print_endline "";; *)
let _ = make_binary_operation union lista 1 2;;
let _ = make_binary_operation union lista 0 3;;
let _ = make_binary_operation union lista 4 5;;
let _ = make_binary_operation union lista 5 6;;
let _ = make_binary_operation union lista 7 5;;
(* let _ = map_until nice_printer 7 lista;; *)
(* print_endline "";; *)
let _ = assert ((strong_eq (make_unary_operation find lista 5) (make_unary_operation find lista 1)) = false)
let _ = make_binary_operation union lista 5 1;;
let _ = assert (strong_eq (make_unary_operation find lista 5) (make_unary_operation find lista 1))
let _ = assert ((strong_eq (i_elem_list 0 lista) (i_elem_list 5 lista)) = false)
let _ = assert (eq (i_elem_list 0 lista) (i_elem_list 5 lista))
(* let _ = map_until nice_printer 7 lista;; *)