module type OrderedType = sig 
  type t
  val compare : t -> t -> int 
end

module type Generated = sig
    type t
    val compose : t -> t -> t
    val compare : t -> t -> int
    val id : t
    val invert : t -> t
end

module type Permutation = sig 
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


module type S = sig 
  (* type perm_key *)
  (* type perm_t *)
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
  (* czy dany element jest generowany przez zbior *)
  val is_generated : t -> t list -> bool
end

module Make(Perm : Permutation)= struct
  (* include Perm *)
  type perm_t = Perm.t
  type perm_key = Perm.key
  include Set.Make(Perm)
  type set_t = t
  type t = perm_t
  type key = perm_key
  let id = (Perm.id)
  let invert = (Perm.invert)
  let swap = (Perm.swap)
  let compose = (Perm.compose)
  let compare = (Perm.compare)
  let apply = (Perm.apply)

  let new_acc lista = List.fold_left (fun x y -> union (singleton y) x) (singleton id) lista
  let generate elem acc = fold (fun value acc -> add (invert value) (add (compose value elem) acc)) acc acc
  let generate_set set = fold (fun value acc -> union (generate value set) acc) set set
  let is_in elem acc = fold (fun value acc -> (compare elem value) * acc ) acc 1
  (* let new_acc acc lista = List.fold_left (fun x y -> set_add y x) set_empty lista *)
  let is_generated (elem : t) (lista : t list) =  
    let rec _is_generated (acc : set_t) = if is_in elem acc == 0 then true 
    else (let u = generate_set acc in if subset u acc then false else
    _is_generated u) in _is_generated (new_acc lista)
        
end