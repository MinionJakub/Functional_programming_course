let rec sufiks lista = match lista with
| [] -> [[]]
| x::lista -> (fun y -> (x::(List.hd y))::y)(sufiks lista);;


let rec prefix_pom acc lista = match lista with 
| [] -> [acc]
| x::lista -> acc::(prefix_pom (List.append acc [x]) lista);;

let prefix lista = prefix_pom [] lista;;

let prefix lista = 
    let rec _prefix acc lista =
        match lista with
        | [] -> [acc]
        | x::lista -> acc::(prefix_pom (List.append acc [x]) lista)
    in _prefix [] lista;;

let rec prefixes xs =  match xs with
| [] -> [[]]
| x::ys -> []::(List.map (fun ls -> x::ls)(prefix ys))
