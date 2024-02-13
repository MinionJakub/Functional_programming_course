(*Chyba nie potrzebna komplikacja pomyslu*)
type rank = int
type time = int
type 'a uf_elem = 'a option ref
type 'a uf_ts_elem = {mutable parents : ('a uf_ts_elem uf_elem * time) list option ; mutable value : 'a ; mutable rank : rank}
let make (v : 'a) : 'a uf_ts_elem = {parents = None; value = v ; rank = 1}
let rec get_list_to_time_stample (lista : ('a uf_ts_elem uf_elem * time) list) (t : time) : ('a uf_ts_elem uf_elem * time) list option =
  match lista with
  | [] -> None
  | x:xs -> begin match x with
  | (value,ts) -> if ts <= t then lista else get_list_to_time_stample xs t end
let rec find (node : 'a uf_ts_elem) (t : time): 'a uf_ts_elem = 
  match node.parents with
  | None -> node
  | Some x -> let y = get_list_to_time_stample x t in 
    match y with 
    | None -> assert false 
    | Some y -> let z = List.hd y in let w,_ = z  in begin match w with 
    | None -> node.parents <- Some y; node
    | Some a -> let b = find a t in 
    let y = find x in node.parent <- Some y; y