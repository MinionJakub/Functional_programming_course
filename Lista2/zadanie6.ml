let rec insert x lista = match lista with
| [] -> [[x]]
| h::t -> (x::lista) :: (List.map (fun el -> h::el) (insert x t));;

let rec perm_insert lista = match lista with 
| [] -> [lista]
| h::t -> List.flatten (List.map (insert h) (perm_insert t));;


let rec choice value lista = 
match lista with 
| [] -> []
| x::lista -> if x == value then lista else x::(choice value lista);;

let rec insert value lista = match lista with 
| [] -> []
| x::lista -> (value::x) :: (insert value lista);;

let rec perm_choice lista = match lista with 
| [] -> []
| [x] -> [[x]]
| _ -> List.flatten (List.map (fun y -> insert y (perm_choice (choice y lista))) lista);;
