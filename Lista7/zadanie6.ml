module MakeSBT(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
end = struct
  type s = State.t
  type 'a t = s -> ('a * s) Seq.type
  let return x : 'a t = fun s -> (Seq.return (x,s))
  let bind (monada : 'a t) (func : 'a -> 'b t) = 
    fun s -> Seq.concat_map (fun (a,s) -> func a s) (monada s)
  let fail : 'a t = fun x -> Seq.empty
  let flip = fun s -> List.to_seq [(true,s);(false,s)]
  let get = fun s -> Seq.return (s,s)
  let put x = fun s -> Seq.return ((),x)
  let run s monada = Seq.map fst (monada s)
end