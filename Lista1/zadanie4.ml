(* zadanie 4 *)
let rec scan funkcja strumien x n = if n = 0 
then funkcja x (strumien 0) else 
funkcja (scan funkcja strumien x (n-1)) (strumien n) ;;