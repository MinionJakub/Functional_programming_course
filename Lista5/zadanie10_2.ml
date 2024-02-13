type 'a clist = {clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z}
let cnil = {clist = fun f z -> z}
let ccons x cl =
  match cl with
  | {clist = cl} -> {clist = 
      fun f z -> f x (cl f z)
    }

let map fm cl =
  {clist  = fun f z ->
    let nf na nz = f (fm na) nz in
    cl.clist nf z   
  }
    
let append a b =
  match a , b with
  | {clist = a} , {clist = b} ->
      {clist = fun f z ->
        a f (b f z)
      }

let clist_to_list = function
| {clist = cl} -> cl (fun a z -> a :: z) []

let rec list_to_clist (xs : 'a list) =
  match xs with
  | [] -> cnil
  | x :: xs -> ccons x (list_to_clist xs)

let prod a b = 
  {clist = fun f c->
      let temp = {clist = fun f c ->
          (a.clist (fun x z -> append (map (fun y -> x , y) b) z) cnil).clist f c
      } in
      temp.clist f c
  }

let sing x = 
  ccons x cnil

let pot b e =
{clist = fun fout aout ->
  (e.clist 
    (fun i c ->
      (map (fun (h , t) -> ccons h t)            
        (prod (prod (sing i) b) c)
      )
    )
    (sing cnil)
  ).clist fout aout 
}

let fun_gen d c =
  List.map (clist_to_list) (clist_to_list (pot (list_to_clist d) (list_to_clist c)))

type empty  = |


type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Empty : empty fin_type
| Eith :'a fin_type * 'b fin_type -> (('a , 'b) Either.t) fin_type
| Func : 'a fin_type * 'b fin_type -> ('a -> 'b) fin_type




let gen_func (c : 'a Seq.t) (d : 'b Seq.t) =
  let fun_list = fun_gen (List.of_seq d) (List.of_seq c)
  in List.to_seq (List.map (fun x -> 
      (fun z -> (snd (List.find (fun (a, b) -> a = z) x)))
    ) fun_list)

let prod_sing_seq elem sequence = 
  Seq.fold_left (fun x y -> Seq.cons (elem,y) x) Seq.empty sequence;;
    
let product sq1 sq2 =
  Seq.fold_left (fun x y -> Seq.append (prod_sing_seq y sq2) x) Seq.empty sq1;;
    
    
let rec all_values : type a. a fin_type -> a Seq.t = function
  | Unit -> Seq.return ()
  | Bool -> Seq.cons true (Seq.return false)
  | Pair (c, d) -> product (all_values c) (all_values d)
  | Empty -> Seq.empty
  | Eith (c, d) -> Seq.append 
    (Seq.map (fun x-> Either.Left x) (all_values c)) 
    (Seq.map (fun x-> Either.Right x) (all_values d))
  | Func (c, d) -> gen_func (all_values c) (all_values d);;