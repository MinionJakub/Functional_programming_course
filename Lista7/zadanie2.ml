module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
  val get : int -> 'a t -> 'a
end

module Shuffle(R: RandomMonad) = struct
  (* let make_function_of_int (v : int R.t) z y =
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
    shuffle_helper (R.return lista) *)
  let ( let* ) = R.bind
  let rec rem xs n = if n == 0 then List.tl xs else List.hd xs :: (rem (List.tl xs) (n-1))
  let rec shuffle xs = 
    match xs with 
    | [] -> R.return []
    | _ -> let* r = R.random in let i = (r mod (List.length xs)) in let* value = (shuffle (rem xs i)) 
  in R.return ((List.nth xs i) :: value) 
    
end

module RS : sig include RandomMonad end = 
struct 
include Random
  type 'a t = int -> 'a * int
  let get v e = fst (e v);;
  let return elem = (fun x -> (elem,x))
  let bind (elem : 'a t) (func : 'a -> 'b t) = 
    fun s -> let x,ns = elem s in func x ns  
     (* func (fst (elem (fst (random 5)))) *)
  let rand_val s = 16807 * (s mod 127773) - 2863 * (s / 127773)
  let gen_seed s = let z = rand_val s in if z > 0 then z else z + 2147483647
  let random s = (rand_val s,gen_seed s)
end

module Shuffled =  Shuffle(RS);;