module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig
  include Monad
  val fail : 'a t
  val catch : 'a t -> (unit -> 'a t) -> 'a t
  val run : 'a t -> 'a option
end = struct
  type 'r ans = 'r option
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}
  let return elem = {run = (fun con -> con elem)}
  let bind elem func = {run = (fun con -> elem.run (fun c -> (func c).run con))}
  let run monad : 'a option =
    monad.run (fun a -> Some a)
  let fail = {run = (fun con -> None)}
  let catch monada con = 
    match monada.run (fun a -> Some a) with
    | None -> con ()
    | Some a -> return a
end

module BT : sig
  include Monad
  val fail : 'a t
  val flip : bool t
  val run : 'a t -> 'a Seq.t
end = struct
  type 'r ans = 'r Seq.t
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}
  let return elem = {run = (fun con -> con elem)}
  let bind elem func = {run = (fun con -> 
    let seq =  elem.run (fun a -> Seq.return a) in
    let eval = Seq.concat_map (fun a -> (func a).run (fun z -> Seq.return z)) seq in
    Seq.concat_map (fun b -> con b) eval
    )}
  let fail = {run = fun con -> Seq.empty}
  let flip = {run = fun con -> Seq.concat_map con (List.to_seq [true;false])}
  let run monad = monad.run (fun a -> Seq.return a)
end



module St(State : sig type t end) : sig
  include Monad
  val get : State.t t
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a
end = struct
  type s = State.t
  type 'r ans = s -> 'r
  type 'a t = {run : 'r. ('a -> 'r ans) -> 'r ans}
  let return elem = {run = fun con -> con elem}
  let bind elem func = {run = fun con -> (elem.run (fun a -> (func a).run con))}
  let set s = {run = fun con -> fun old_s -> con () s}
  let get = {run = fun con -> fun s -> con s s}
  let run s elem = elem.run (fun a -> fun s -> a) s
end