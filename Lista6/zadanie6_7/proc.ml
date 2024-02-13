
(* interfejs do kanału wejścia *)
type ('z,'i) in_channel =
| In of (('z,'i) out_channel -> 'z)

(* interfejs do kanału wyjścia *)
and ('z,'o) out_channel =
| Out of ('o -> ('z,'o) in_channel -> 'z)

(* interfejs do kanałów *)
type ('z,'i,'o) ans = ('z,'i) in_channel -> ('z,'o) out_channel -> 'z

(* typ nowego "procesu" jako element przyjmujacy funkcje czeka na kontynuacje i zwraca answer*)
type ('a,'z,'i,'o) proc = ('a -> ('z,'i,'o) ans) -> ('z,'i,'o) ans

(* tworzy wyjscie gdzie zwraca pewna wartosc o typie 'o' potem czeka na kontynuacje,
    przyjmuje wczesniej istniejacy channel wejscia i ustawia siebie jako wyjscie*)
let send o k in_ch (Out send) =
  send o (In(fun out_ch -> k () in_ch out_ch))

(* tworzy wejscie gdzie czeka na kontynuacje, ustala siebie jako wejscie 
    a potem nie modyfikuje wyjscia*)
let recv k (In recv) out_ch =
  recv (Out(fun v in_ch -> k v in_ch out_ch))

(* kontynuacja identycznosciowa ktora pomiaj kanaly*)
let exit_k x _ _ = x

(* stworzenie dwoch procesow ktore maja okreslony poczetek i koniec i miedzy nimi jest potok *)
let (>|>) p1 p2 _ in_ch out_ch =
  p2 exit_k
    (In(fun m_ch -> p1 exit_k in_ch m_ch))
    out_ch

let rec stdin_ch =
  In (fun (Out send) -> send (read_line ()) stdin_ch)

let rec stdout_ch =
  Out (fun str (In recv) -> print_endline str; recv stdout_ch)

let run p = p exit_k stdin_ch stdout_ch