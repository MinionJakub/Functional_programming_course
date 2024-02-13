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
  type 'a t = s -> 'a sbt_list
  and 'a sbt_list = 
  | Nil 
  | Cons of 'a * s * 'a t
  let fail = fun s -> Nil
  let return elem = fun s -> Cons (elem,s,fail)
  let flip = fun s -> Cons(true,s,fun s -> Cons(false,s,fail))
  let get = fun s -> Cons(s,s,fail)
  let put elem = fun s -> Cons((),x,fail)
  let rec concatM (first : 'a t) (second : 'b t) =
    fun s -> match first s with 
    | Nil -> b s 
    | Cons(x,s,a) -> Cons(x,s, concatM a b);;
  let rec run s monada = 
    match monada s with 
    | Nil -> Seq.empty
    | Cons(a,s,m) -> Seq.cons a (run s m);;
  let rec bind monada func = 
    fun s -> match monada s with
    | Nil -> Nil
    | Cons(a,sa,ma) -> concatM (func a) (bind ma func) s
end