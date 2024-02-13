let merge_tail cmp reversed lista1 lista2 = 
    let rec _merge cmp l1 l2 result = 
        match l1, l2 with
            | [] , [] -> result
            | [] , h :: t | h :: t , [] -> _merge cmp  [] t (h::result)
            | h1 :: t1 , h2 :: t2 -> if (cmp h1 h2) == reversed
            then _merge cmp l1 t2 (h2::result) 
            else _merge cmp t1 l2 (h1::result)  
            in _merge cmp lista1 lista2 [];;


let halve_tail lista = 
let rec _halve x y z = match x with
| [] -> [y;z]
| x::[] -> [x::y;z]
| u::v::rest -> _halve rest (u::y) (v::z) in _halve lista [] [];; 

let merge_sort_tail cmp lista = 
let rec _merge_sort_tail cmp reversed lista = match lista with
| [] -> []
| x::[] -> [x]
| _ -> let podzial = halve_tail lista in 
merge_tail cmp reversed 
(_merge_sort_tail cmp (reversed == false) (List.hd podzial)) 
(_merge_sort_tail cmp (reversed == false) (List.hd (List.tl podzial))) 
in _merge_sort_tail cmp true lista ;;