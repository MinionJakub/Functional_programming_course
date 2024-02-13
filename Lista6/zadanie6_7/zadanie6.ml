open Proc
let rec echo k = Proc.recv (fun v -> Proc.send v (fun () -> echo k));;

let rec map (funkcja : 'i -> 'o) (kontynuacja : 'a) = 
  Proc.recv (fun v -> Proc.send (funkcja v) 
  (fun () -> map funkcja kontynuacja));;

let rec filter (funkcja : 'i -> bool) (kontynuacja : 'a) = 
  Proc.recv (fun v -> if funkcja v then Proc.send v (fun () ->
    filter funkcja kontynuacja) else filter funkcja kontynuacja);;

let rec nats_from (num : int) kontynuacja = 
  Proc.send num (fun () -> nats_from (num + 1) kontynuacja);;

let rec sieve kontynuacja = 
  Proc.recv (fun v -> Proc.send v 
  (fun () -> ((filter (fun x -> not((x mod v) == 0))) >|> sieve) kontynuacja));;

let _ = run (nats_from 2 >|> sieve >|> map string_of_int);;