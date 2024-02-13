type (_,_) format = 
| Int : (int -> 'a ,'a) format
| Str : (string -> 'a , 'a) format
| Lit : string -> ('a, 'a) format
| Cat : ('a,'b)format * ('b,'c)format -> ('a,'c)format

let (^^) (exp1 : ('c,'a)format) (exp2 : ('a,'b)format) = Cat (exp1,exp2);;

let rec helper : type a b. (a,b) format -> (string -> b) -> string -> a =
  function 
  | Lit tekst -> fun (kontynuacja : string -> b)(s : string) -> kontynuacja (s ^ tekst)
  | Int -> (fun(kontynuacja : string -> b)(s:string)->(fun(i:int)-> kontynuacja (s ^ (string_of_int i))))
  | Str -> (fun(kontynuacja : string -> b)(s:string)->(fun(sn:string)->kontynuacja(s^sn)))
  | Cat (exp1,exp2) -> fun (kontynuacja:string->b)(s:string) -> helper exp1 (helper exp2 kontynuacja) s;;

let ksprintf exp kontynuacja = 
  helper exp kontynuacja "";;

let sprintf exp = 
  ksprintf exp (fun x -> x);;

let function_composition (f : 'a -> 'b) (g : 'b -> 'c) = 
  fun x -> g(f x);;

let rec helper2 : type a b. (a,b) format -> (unit -> b) -> (unit -> a) =
  function
  | Lit l -> fun (kontynuacja : unit -> b) () -> print_string l; kontynuacja ()
  | Int -> fun (kontynuacja : unit -> b) () (i : int) -> print_int i; kontynuacja ()
  | Str -> fun (kontynuacja : unit -> b) () (s : string) -> print_string s; kontynuacja()
  | Cat (i,o) -> fun (kontynuacja : unit -> b) () -> helper2 i (helper2 o kontynuacja) ();;

let kprintf tekst kontynuacja = 
  helper2 tekst (fun () -> kontynuacja) ();;

let printf tekst = 
  kprintf tekst ();;

let _ = printf (Lit "Ala ma " ^^ Int ^^ Lit " kot" ^^ Str) 3 "y";;