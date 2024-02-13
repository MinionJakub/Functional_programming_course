(*
let ctrue = fun x -> fun y -> if true then x else y;;
let cfalse = fun x -> fun y -> if false then x else y;;
let cbool_of_bool value = if value then ctrue else cfalse ;;
let bool_of_cbool value = if value(true)(false) = true then true else false;;
let cand val1 val2 = if val1 == ctrue && val2 == val1 then val1 else cfalse;;
let cor val1 val2 = if val1 == ctrue then val1 else val2;;
*)

(*
let ctrue : cbool = {cbool = fun x -> fun y -> if true then x else y};;
let cfalse : cbool = {cbool = fun x -> fun y -> if false then x else y};;
let cand (val1 : cbool) (val2 : cbool) : cbool  = 
{cbool = if val1 == ctrue && val1 == val2 then val1.cbool else cfalse.cbool};;
let cor (val1 : cbool) (val2 : cbool) : cbool =
{cbool = if val1 == ctrue then val1.cbool else val2.cbool};;
*)


type cbool = {cbool : 'a. 'a -> 'a -> 'a};;


let ctrue : cbool = {cbool = fun x -> fun y -> x};;
let cfalse : cbool = {cbool = fun x -> fun y -> y};;


let cand (val1 : cbool) (val2 : cbool) : cbool = val1.cbool (val2.cbool ctrue cfalse) cfalse;;

let cor (val1 : cbool) (val2 : cbool) : cbool = val1.cbool ctrue (val2.cbool ctrue cfalse);;

let bool_of_cbool (value : cbool) : bool = ((value.cbool true false) = true);;

let cbool_of_bool (value : bool) : cbool = if value then ctrue else cfalse;;

(*
let zero = fun f x -> x;;
let succ value = fun f x -> f(value f  x);;
let rec int_of_cnum n = n (fun x -> x+1) 0;;
let rec cnum_of_int n = if n == 0 then zero else succ(cnum_of_int(n-1));;
let mult val1 val2 = fun f x -> val1(val2(f)) x;;
let add val1 val2 = fun f x -> (val1 f ((val2) f x));;
let exp val1 val2 = fun f x -> val1(val2) f x;;
let is_zero n = if n == zero then ctrue else cfalse;;


let is_zero (value : cnum) : cbool = 
{cbool = if value == zero then ctrue.cbool else cfalse.cbool};;


*)

type cnum = {cnum : 'a. ('a -> 'a) -> 'a -> 'a};;

let zero : cnum = {cnum = fun f x -> x};;

let succ (value : cnum) : cnum = {cnum = fun f x -> f(value.cnum f x)};;

let int_of_cnum (value : cnum) : int = value.cnum (fun x -> x+1) 0;;

let rec cnum_of_int (value : int) : cnum = 
if value == 0 then zero else (succ (cnum_of_int (value - 1)));;

let mult (val1 : cnum) (val2 : cnum) : cnum = 
{cnum = fun f x -> val1.cnum(val2.cnum(f))x};;

let add (val1 : cnum) (val2 : cnum) : cnum = 
{cnum = fun f x -> (val1.cnum f ((val2.cnum) f x))};;

let exp (val1 : cnum) (val2 : cnum) : cnum = 
{cnum = fun f x -> val1.cnum(val2.cnum) f x};;

let exp2 (val1 : cnum) (val2 : cnum) : cnum = 
{cnum = fun f x -> val2.cnum(val1.cnum) f x};;

let is_zero (value : cnum) : cbool = 
    value.cnum (fun x -> cfalse) ctrue;;