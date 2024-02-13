(* zadanie 3 *)

(*
let head (value, (n,stream)) = value;;
let tail (value,(n, stream)) = stream;;
*)
(* version 1 *)

(*
let start_from_next stream = fun x -> stream(x+1);;
let tail (value,stream) = start_from_next(stream);;
*)
(* version 2 *)

let head stream = stream(0);;
let tail stream = fun x-> stream(x+1);;
(* final version *)


(* let add x = fun f -> fun y -> f(y) + x;; *)
(* tylko dla int-ow *)

(*
let add (value, funkcja, operator) = fun y -> operator(funkcja(y), value);;
let add (value, funkcja, operator) = fun y -> operator(value)(funkcja(y));;
*)
let add value funkcja operator = fun y -> operator(value)(funkcja(y));;
(* moze ciut za ogolna *)

(* let mapa(strumien, funkcje) = fun x ->  funkcje(strumien(x));; *)
let mapa strumien funkcja = fun x -> funkcja(strumien(x));;
(* ciut za ogolne *)

(* let mapa2(strumien1, strumien2, operator) = fun x -> operator(strumien1(x))(strumien2(x));; *)
let mapa2 strumien1 strumien2 operator = fun x -> operator(strumien1(x))(strumien2(x));;
(*ciut za ogolna *)

let replace n a s  = fun x -> if x = n then a else s(x);;
(* patrz wyzej *)

let take_every n s = fun x -> s(n*x);;
