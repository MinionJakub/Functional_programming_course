let rec sublists lista = match lista with
| [] -> [[]]
| x::lista -> 
(fun y -> List.fold_left (fun u z -> (x::z)::u) y y)(sublists lista);; 