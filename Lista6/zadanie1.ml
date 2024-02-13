type ('a, 'b) format = (string -> 'b) -> (string -> 'a)

let _int (k : string -> 'a) = 
  fun (s : string) (i : int) -> k (s ^ (string_of_int i));;

let _str (k : string -> 'a) =
  fun (s : string) (s2 : string) -> k(s ^ s2);;

let lit sn = fun (k : string -> 'a) (s : string)->
  k(s ^ sn);;

let (^^) (f:('c,'a)format)(s:('a,'b)format)(k:(string -> 'b))=
  f(s k);;

let ksprintf (exp : ('a,'b) format) (k : (string -> 'b)) =
  exp k "";;

let sprintf (exp : ('a,string) format)=
  ksprintf exp (fun x -> x);;

assert (String.equal ((sprintf (lit "Ala ma " ^^ _int ^^ lit " kot" ^^ _str ^^ lit ".")) 5 "ów") "Ala ma 5 kotów.");;
