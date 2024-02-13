(* module StateMonad (State : sig type t end) : sig
include Monad.Monad
val runStateMonad : 'a t -> State.t -> 'a * State.t
val put : State.t -> unit *  State.t
val get : 'a * State.t ->  State.t * State.t
end = struct
  type 'a t = State.t -> 'a * State.t
  let return a = fun state -> (a,state)
  let bind value func = fun state -> 
    let a, transient_state  = value state in
      let b , final_state = func a transient_state in (b,final_state) 
  let ( >>= ) = bind
  let runStateMonad a state = a state
  let put = fun s ->  ((),s)
  let get s = let x,y = s in (y,y)
end *)

(* Define the State Monad *)
module StateMonad (State : sig type t end) = struct
  type 'a state = State.t -> 'a * State.t

  (* Return a computation that modifies the state *)
  let return a = fun s -> (a, s)

  (* Bind operator (>>=) to compose computations *)
  let bind m f = fun s ->
    let (a, s') = m s in
    f a s'

  (* Access the current state without modifying it *)
  let get_state = fun s -> (s, s)

  (* Modify the state using a function *)
  let modify_state f = fun s -> ((), f s)
  let ( >>= ) m f = bind m f 
end

(* open StateMonad

(* Example computation using the State Monad *)
let example_computation : int state =
  return 1 >>= (fun a ->
  modify_state (fun s -> s + 1) >>= (fun () ->
  get_state >>= (fun s ->
  return (a + s))))

(* Run the computation with an initial state *)
let result, final_state = example_computation 0

(* Print the result and final state *)
let () =
  Printf.printf "Result: %d\nFinal State: %d\n" result final_state *)
