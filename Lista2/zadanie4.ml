let rec merge cmp lista1 lista2 = match lista1 with
| [] -> lista2
| x::lista1 -> (match lista2 with 
| [] -> x::lista1
| y::lista2 -> if (cmp x y) then (x::(merge cmp lista1 (y::lista2))) 
else (y::(merge cmp (x::lista1) lista2)));;


let merge_tail cmp lista1 lista2 = 
    let rec _merge cmp l1 l2 result = 
        match l1, l2 with
            | [] , [] -> result
            | [] , h :: t | h :: t , [] -> _merge cmp  [] t (h::result)
            | h1 :: t1 , h2 :: t2 -> if (cmp h1 h2) 
            then _merge cmp t1 l2 (h1::result) 
            else _merge cmp l1 t2 (h2::result) 
            in List.rev(_merge cmp lista1 lista2 []);;


let rec halve lista = match lista with
| [] -> [[];[]]
| x::[] ->[[x];[]]
| x::y::lista -> (fun z -> [(x::(List.hd z));(y::(List.hd (List.tl z)))])(halve lista) ;;


let halve_tail lista = 
let rec _halve x y z = match x with
| [] -> [y;z]
| x::[] -> [x::y;z]
| u::v::rest -> _halve rest (u::y) (v::z) in _halve lista [] [];; 


let rec merge_sort cmp lista = match lista with
| [] -> []
| x::[] -> [x]
| _ -> let podzial = halve lista in 
merge cmp (merge_sort cmp (List.hd podzial)) 
(merge_sort cmp (List.hd (List.tl podzial)));;

let rec merge_sort_tail cmp lista = match lista with
| [] -> []
| x::[] -> [x]
| _ -> let podzial = halve_tail lista in 
merge_tail cmp (merge_sort_tail cmp (List.hd podzial)) 
(merge_sort_tail cmp (List.hd (List.tl podzial)));;

