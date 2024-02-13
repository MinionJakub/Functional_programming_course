let rec fold_left_cps (func : ('a -> 'b -> ('a -> 'c) -> 'c)) (acc : 'a) (xs : 'b list) (cont : 'a -> 'c)=
  match xs with 
  | [] -> cont acc
  | x :: xs -> let ncont = (fun (acc : 'a) -> fold_left_cps func acc xs cont) in func acc x ncont;;

let fold_left (func : 'a -> 'b -> 'a) (acc : 'a) (table : 'b list) = 
  fold_left_cps (fun x y con -> con (func x y)) acc table (fun x -> x);;