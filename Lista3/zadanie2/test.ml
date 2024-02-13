module type OrderedType = sig 
  type t
  val compare : t -> t -> int 
end

module type S = sig
  type t
end

module Make(Key : OrderedType)=  struct
end