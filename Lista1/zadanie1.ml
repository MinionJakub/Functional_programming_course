(* zadanie 1 *)
fun x -> x;;
(* `a -> `a = fun *)

fun x -> x + 0;;
(* int -> int identyczoność *)  

fun x -> fun y -> fun z -> x(y(z));;
(* ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)

fun x -> fun y -> x ;;
(* 'a -> 'b -> 'a *)

fun (x : 'a) -> fun (x : 'a) -> x;;
fun x -> fun y -> if true then x else y;;
(* 'a -> 'a -> 'a *)
