let length lista = List.fold_left (fun x y -> x+1) 0 lista;;
let rev lista = List.fold_left (fun x y -> y::x) [] lista;;
let map funkcja lista = List.fold_right (fun x y -> funkcja(x)::y) lista [];;
let append lista1 lista2 = List.fold_right (fun x y -> x::y) lista1 lista2;;
let rev_append lista1 lista2 = List.fold_left (fun x y -> y::x) lista2 lista1;;
let filter funkcja lista = List.fold_right 
(fun x y -> if funkcja(x) then x::y else y) lista [];;
let rev_map funkcja lista = List.fold_left (fun x y -> funkcja(y)::x) [] lista;;
