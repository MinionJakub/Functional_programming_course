module StateMaybeMonad (State : sig type t end) =
struct
type 'a state = State.t -> ('a * State.t) option
let bind m f = 
  fun s -> let v = m s in
  match v with
  | Some(a,s') -> f a s'
  | None -> None
let return a = fun s -> Some (a,s)
let noValue = fun s -> None
let get_state = fun s -> Some(s,s)
let modify_state f = fun s -> Some ((),f s)
let map f m = 
  fun s -> let v = m s in
  match v with
  | Some(a,s') -> Some (f a, s')
  | None -> None
(* Function below is used for backtracking. The name of this function is in progress *)
let map2 f m1 m2 =
  fun s -> let v = m1 s in
  match v with
  | None -> None
  | Some(a,s') -> 
    let x = m2 s in 
    match x with
    | None -> None
    | Some (a',s'') -> Some(f a a', s)
end