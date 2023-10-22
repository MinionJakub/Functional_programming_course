module type OrderedType = sig 
  type t
  val compare : t -> t -> int 
end

module type S = sig 
  type key
  type t
  (* permutacja jako funkcja *)
  val apply : t -> key -> key
  (* permutacja identycznosciowa *)
  val id : t
  (* permutacja odwrotna *)
  val invert : t -> t
  (* permutacja która zmienia dwa elementy miejscami *)
  val swap : key -> key -> t
  (* złożenie permutacji (jako złożenie funkcji) *)
  val compose : t -> t -> t
  (* porównanie permutacji *)
  val compare : t -> t -> int
end

module Make(Key : OrderedType) = struct
  include Map.Make(Key)
  type t = (key Map.Make(Key).t) * (key Map.Make(Key).t)
  let apply (permut : t) (elem : key) = if mem elem (fst permut)
    then (find elem (fst permut)) 
    else elem
  let id : t = (empty, empty)
  let invert (permut : t) : t = let a,b = permut in (b,a)
  let swap (value1 : key)(value2 : key) : t = (add value2 value1 (add value1 value2 empty)) ,(add value1 value2 (add value2 value1 empty))
  let h1 (per1 : t) (per2 : t) = fold 
  (fun klucz data acc -> let v = apply per2 (apply per1 klucz) in
  (add klucz v acc)) (fst per1) empty
  let h2 (per1 : t) (per2 : t) = fold 
  (fun klucz data acc -> let v = apply per2 (apply per1 klucz) in
  (add klucz v acc)) (fst per2) empty
  let h3 per1 per2 = union (fun klucz val1 val2 -> Some val1) per1 per2
  let h4 per1 per2 =  h3 (h1 per1 per2) (h2 per1 per2)
  let compose (per1 : t) (per2 : t) : t =  let value = h4 per1 per2 in 
  fold (fun klucz data acc -> (add klucz data (fst acc)),(add data klucz (snd acc))) value (empty,empty)
  let compare (per1 : t) (per2 : t) = let x,y = (fst per1),(fst per2) in
  compare Key.compare x y
end;;