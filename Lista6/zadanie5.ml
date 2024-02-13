let rec fold_left_cps (func : ('a -> 'b -> ('a -> 'c) -> 'c)) (acc : 'a) (xs : 'b list) (cont : 'a -> 'c)=
  match xs with 
  | [] -> cont acc
  | x :: xs -> let ncont = (fun (acc : 'a) -> fold_left_cps func acc xs cont) in func acc x ncont;;

let for_all func lista = 
  let helper acc elem con =
    if func elem then con acc else false
  in fold_left_cps helper true lista (fun x -> x);;

let mult_list lista = 
  let helper acc elem con = 
    if elem = 0 then 0 else con (elem * acc) in
  fold_left_cps helper 1 lista (fun x -> x);;


(* let sorted lista = 
  let helper (return_value,next) elem con =
    begin match next with 
    | None -> (return_value,(Some elem))
    | Some value -> if return_value && not(elem <= value) then con (true,(Some elem)) else (false,(Some elem))
  end
in fold_left_cps helper (true,None) lista (fun x -> x);; *)

let helper (return_value,next) elem con = 
match next with
| None -> con (return_value,Some elem)
| Some value -> if (elem <= value) then (false, Some elem) else con (return_value,Some elem);;

let sorted lista = 
  fold_left_cps helper (true,None) lista (fun x -> x) |> fst;;

(* sorted [5;2;3;4];; *)