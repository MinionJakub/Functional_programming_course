


let zero = fun f x -> x;;
let succ value = fun f x -> f(value f  x);;
let int_of_cnum n = n (fun x -> x+1) 0;;
let rec cnum_of_int n = if n == 0 then zero else succ(cnum_of_int(n-1));;
let mult val1 val2 = fun f x -> val1(val2(f)) x;;
let add val1 val2 = fun f x -> (val1 f ((val2) f x));;
let exp val1 val2 = fun f x -> val1(val2) f x;;
let exp2 val1 val2 = fun f x -> val2(val1) f x;;
let is_zero n = n (fun x -> cfalse) ctrue;;

(*let is_zero n = if n == zero then ctrue else cfalse;;*)

(*let mult val1 val2 = fun x -> val1(val2(x));;
let exp val1 val2 = fun x -> val1(val2)x;;*)

