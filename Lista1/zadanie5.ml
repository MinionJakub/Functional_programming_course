(* zadanie 5 *)
let rec tabulate ?(start=0) final stream  = 
if (start > final) then [] 
else (stream start)::(tabulate ~start:(start+1) final stream);;