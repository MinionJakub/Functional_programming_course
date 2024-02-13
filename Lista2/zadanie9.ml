type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil : 'a clist = {clist: fun x y -> y};

let cnil : 'a clist = {clist = fun f -> fun z -> z}


let ccons (a : 'a) (list : 'a clist) : 'a clist = {clist = fun f -> fun z -> f a (list.clist f z)}


let map (func : 'a -> 'b) (clis : 'a clist) : 'b clist = clis.clist (fun x y -> ccons (func x) y) cnil

let append (clis1 : 'a clist) (clis2 : 'a clist) : 'a clist = clis1.clist (fun x y -> ccons x y) clis2


let prod (clis1 : 'a clist) (clis2 : 'b clist) : ('a * 'b) clist = clis1.clist (fun x1 y1 -> append (clis2.clist (fun x2 y2 -> ccons (x1,x2) y2) cnil) y1) cnil


let clist_to_list (clis : 'a clist) : 'a list = clis.clist (fun x y -> x :: y) []

let rec clist_of_list (list : 'a list) : 'a clist = match list with 
| [] -> cnil
| x :: xs -> (ccons x (clist_of_list xs))