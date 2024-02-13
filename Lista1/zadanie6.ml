(* zadanie 6 *)

(*
let ctrue = fun x -> fun y -> if true then x else y;;
let cfalse = fun x -> fun y -> if false then x else y;;
let cand val1 val2 = if val1 == ctrue && val2 == val1 then val1 else cfalse;;
let cor val1 val2 = if val1 == ctrue then val1 else val2;;
*)

let ctrue = fun x y -> x;;
let cfalse = fun x y -> y;;
let cbool_of_bool value = if value then ctrue else cfalse ;;
let bool_of_cbool value = if value(true)(false) = true then true else false;;
let cand val1 val2 = (val1 (val2 ctrue cfalse) cfalse);;
let cor val1 val2 = (val1 ctrue (val2 ctrue cfalse));;