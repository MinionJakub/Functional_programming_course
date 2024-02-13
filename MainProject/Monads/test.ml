open MaybeMonad
(* open StateMonad2 *)

let x = MaybeMonad.return 1
let z = MaybeMonad.bind x (fun x ->  MaybeMonad.return (x + 1))
let w = MaybeMonad.bind z (fun x -> print_endline (string_of_int x); MaybeMonad.return x)

type 'a state = 
| Value of 'a
| NoValue

module State = struct
  type t = int state
  let make value = Value value
  let make_NoValue = NoValue
end


open StateMonad
module MState = StateMonad(Int);;
let (>>=) = MState.bind;;
let ( let* ) = MState.bind;;
(* Example computation using the State Monad *)
(* let example_computation : int state =
  MState.bind (MState.return 0) (fun a ->
    MState.bind (MState.modify_state (fun s -> s + 1)) (fun () ->
      MState.bind (MState.get_state) (fun s ->
        MState.return (a + s)))) *)

let example_computation = MState.bind (MState.return 1) 
(fun a -> MState.bind (MState.modify_state (fun s -> s + 1)) 
(fun w -> MState.bind MState.get_state (fun s -> MState.return (a+s))))

let example_computation = (MState.return 1) >>=
(fun a -> (MState.modify_state (fun s -> s + 1)) >>= 
fun () -> MState.get_state >>= (fun s -> MState.return (a+s)))

let example_computation = 
  let* a = MState.return 1 in
  let* () = MState.modify_state (fun s -> s + 1) in
  let* s = MState.get_state in 
  MState.return (a + s)

let add_one m = 
  let* a = m in
  let* () = MState.modify_state (fun s ->print_endline (string_of_int s) ;s + 1) in
  let* s = MState.get_state in 
  MState.return (s)

(* Run the computation with an initial state *)
let result, final_state = example_computation 1

(* Print the result and final state *)
let () =
  Printf.printf "Result: %d\nFinal State: %d\n" result final_state

let result,final_state = (add_one (add_one (MState.return 1))) 1

let () =
  Printf.printf "Result: %d\nFinal State: %d\n" result final_state



open StateMaybeMonad
module SMMonad = StateMaybeMonad(Int);;

let ( let* ) = SMMonad.bind

let add_one_if_less_then_five m = 
  let* a = m in
  let* () = SMMonad.modify_state (fun s -> s + 1) in
  let* s = SMMonad.get_state in
  if s > 5 then SMMonad.noValue else SMMonad.return (s)

  let v = (add_one_if_less_then_five(add_one_if_less_then_five(add_one_if_less_then_five(add_one_if_less_then_five (add_one_if_less_then_five (SMMonad.return 1)))))) 1

  let () =
    match v with
    | Some(result,final_state) -> Printf.printf "Result: %d\nFinal State: %d\n" result final_state
    | None -> ()
  