module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
  (* val get : int -> 'a t -> 'a *)
end

module Shuffle(R: RandomMonad) = struct
  let make_function_of_int (v : int R.t) z y =
     (* R.bind v (fun x -> print_endline (string_of_int x); R.return x); *)
    R.bind v (fun x -> if x <= 0 then z 
    else y)
  let rec nth (lista : 'a list R.t) (n : int R.t) =
    (make_function_of_int n)  
    (R.bind lista (fun y -> 
      match y with 
      | [] -> R.return []
      | e::es -> R.return [e]))
    ( R.bind lista (fun y -> 
      match y with 
      | [] -> R.return []
      | e::es -> (nth (R.return es) (R.bind n (fun z -> R.return (z - 1))))));;
  let rec remining (lista : 'a list R.t) (n : int R.t) (acc : 'a list R.t) = 
    (* print_endline "Hello"; *)
    (make_function_of_int n)  (R.bind acc (fun x -> 
      R.bind lista (fun y -> match y with
      | [] -> R.return []
      | e::y ->
      R.return (List.append x y)))) 
      (R.bind lista (fun x -> match x with 
        | [] -> R.return []
        | e::es -> remining (R.return es) (R.bind n (fun x -> R.return (x - 1))) 
        (R.bind acc (fun x -> R.return (e::x)))
        )
      );;
  let rec shuffle_helper (lista : 'a list R.t) = R.bind lista (fun x -> match x with | [] -> R.return [] | _ ->
    let modulo = R.bind lista (fun x -> R.return (List.length x)) in
     (* print_endline (string_of_int (R.get 5 modulo)); *)
    (make_function_of_int modulo) (R.return [])
    (let elem = R.bind R.random (fun x -> R.bind modulo (fun y -> R.return (x mod y))) in
    R.bind (nth lista elem) 
    (fun x -> R.bind (shuffle_helper (remining lista elem (R.return []))) 
    (fun y -> R.return (List.append x y)))));;
  let shuffle (lista : 'a list) = 
    shuffle_helper (R.return lista)
end


(* module Shuffle(R : RandomMonad) = struct 
  let compare (v : int R.t) (v2 : int) = R.return v2 == v;;
  let modulo (v : int R.t) (v2 : int) = R.bind v (fun x -> R.return (x mod v2))
  let rec nth (v : 'a list) (v2 : int R.t) = 
    match v with
    | [] -> failwith "You are so f*cking dumb"
    | x :: xs -> if compare v2 0 then x else nth xs (R.bind v2 (fun x ->R.return (x - 1)))
  let rec remining (lista : 'a list) (num : int R.t) acc =
    match lista with 
    | [] -> acc
    | x :: xs -> if compare num 0 then List.append (List.rev acc) xs else remining xs (R.bind num (fun x -> R.return (x-1))) (x :: acc);;
  (* let (let* ) = bind *)
  let shuffle (lista : 'a list) = 
    let rec helper (lista : 'a list) acc = 
      match lista with 
      | [] -> acc
      | x :: xs -> let value = R.random in let modulo_value = (modulo value (List.length lista)) in 
      if compare modulo_value 0 then (helper xs (x :: acc)) else helper (remining lista modulo_value []) ((nth lista modulo_value) :: acc) in
     R.return (helper lista []);  ;
end *)
