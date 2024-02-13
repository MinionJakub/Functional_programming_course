(* open Logic *)

type goal =
  {f : Logic.formula; a : (string * Logic.formula) list}

type proof_tree =
  | Goal of goal
  | Lema of Logic.theorem
  | ImpI of {a : (string * Logic.formula) list; f : Logic.formula; x : proof_tree}
  | ImpE of {a : (string * Logic.formula) list; f : Logic.formula; x : proof_tree; y : proof_tree}
  | NegE of {a : (string * Logic.formula) list; f : Logic.formula; x : proof_tree}

type context = 
  | Root
  | NegEc  of {a : (string * Logic.formula) list; f : Logic.formula; ctx : context }
  | ImpEyc of {a : (string * Logic.formula) list; f : Logic.formula; x : proof_tree; ctx : context }
  | ImpExc of {a : (string * Logic.formula) list; f : Logic.formula; y : proof_tree; ctx : context }
  | ImpIc of   {a : (string * Logic.formula) list; f : Logic.formula; ctx : context }
  

let up_ctx (pt : proof_tree) (ctx : context) =
  match ctx with
  | Root -> (pt, ctx)
  | NegEc {a; f; ctx} -> NegE {a; f; x = pt}, ctx
  | ImpIc {a; f; ctx} -> ImpI {a; f; x = pt}, ctx
  | ImpExc {a; f; y; ctx} -> ImpE {a; f; x = pt; y}, ctx
  | ImpEyc {a; f; x; ctx} -> ImpE {a; f; x; y = pt}, ctx
type proof = 
  | Complete of Logic.theorem
  | Proof of context * goal

let proof g f =
  let g = {f = f; a = g} in
    Proof (Root , g)

let qed pf =
  match pf with
  | Complete th -> th
  | Proof _ -> failwith "Holes"
  
let goal pf =
  match pf with
  | Complete _ -> None
  | Proof (_, {f; a}) -> Some (a, f)

let rec down_left (pt : proof_tree) (ctx : context) = 
  match pt with
  | Goal g -> Proof (ctx, g)
  | Lema _ -> failwith "something went wrong"
  | ImpI {a; f; x} -> down_left x  (ImpIc {a; f; ctx})
  | NegE {a; f; x} -> down_left x (NegEc {a; f; ctx})
  | ImpE {a; f; x; y} ->
    match x with 
    | Lema _ -> down_left y (ImpEyc {a; f; x; ctx})
    | _ -> down_left x (ImpExc {a; f; y; ctx})

let rec up_right (pt : proof_tree) (ctx : context) =
  match ctx with
  | Root -> down_left pt ctx
  | NegEc {a; f; ctx} -> up_right (NegE {a; f; x = pt}) ctx
  | ImpIc {a; f; ctx} -> up_right (ImpI {a; f; x = pt}) ctx 
  | ImpEyc {a; f; x; ctx} -> up_right (ImpE {a; f; x; y = pt}) ctx
  | ImpExc {a; f; y; ctx} -> 
    match y with
    | Lema _ -> up_right (ImpE {a; f; x = pt; y}) ctx
    | _ -> down_left y (ImpEyc {a; f; x = pt; ctx})

let next (pf : proof) =
  match pf with
  | Complete _ -> failwith "No holes"
  | Proof (ctx, g) -> up_right (Goal g) ctx 


let add_assum (s, f) (a : (string * Logic.formula) list) = 
  if List.exists (fun (ls, lf) -> ls = s) a
    then failwith "jest już założenie o tej nazwie" 
    else(s, f) :: a

let rem_form (f : Logic.formula) (a : (string * Logic.formula) list) =
  List.filter (fun (s,lf) -> not (lf = f)) a

let intro name pf =
  match pf with
  | Complete _ -> failwith "brak luk"
  | Proof (ctx, {f; a}) ->
    match f with
    | Variable _ -> failwith "oczekiwano Imp napotkano Var"
    | Neg   -> failwith "oczekiwano Imp, napotkano Neg"
    | Implication (q, r) -> 
      let g = {f = r; a = add_assum (name, q) a} in
      Proof ((ImpIc {a; f; ctx}), g)



let aph af pf =
  match pf with
  | Complete _ -> failwith "Completelete proof"
  | Proof (ctx, {f; a}) ->
    let rgt = Goal{f = af; a} in
    let lg = {f = Implication (af, f); a} in
    Proof (ImpExc {a; f; y = rgt; ctx}, lg)


let rec imp_to_list f gf =
  match f with 
  | Logic.Neg -> [Logic.Neg]
  | Logic.Implication (a, b) when b = gf -> a :: [gf]
  | Logic.Implication (a, b) -> a :: imp_to_list b gf
  | Logic.Variable s when f = Logic.Variable s -> [f]
  | Logic.Variable s -> failwith "oczekiwano negacji, lub formuly do uduwodnienia"

let rec complete_goal (pt, ctx : proof_tree * context) (th : Logic.theorem) =
  match ctx with
  | Root -> Complete th
  | NegEc  {a; f; ctx} -> complete_goal (up_ctx (Lema th) ctx) (Logic.bot_e f th)
  | ImpIc {a; f; ctx} -> complete_goal (up_ctx (Lema th) ctx) (Logic.imp_i f th)
  | ImpEyc {a; f; x; ctx} ->
    begin match x with
    | Lema l -> complete_goal (up_ctx (Lema th) ctx) (Logic.imp_e l th)
    | _ -> down_left x (ImpExc {a; f; y = (Lema th); ctx})
    end
  | ImpExc {a; f; y; ctx} -> 
    match y with
    | Lema l -> complete_goal (up_ctx (Lema th) ctx) (Logic.imp_e th l)
    | _ -> down_left y (ImpEyc {a; f; x = (Lema th); ctx})

let complete_luka pf th =
  match pf with
  | Complete _ -> pf
  | Proof (ctx, {a; f}) -> 
    if Logic.consequence th = f 
      then complete_goal (Goal {a; f}, ctx) th
      else  failwith "zly theorem"
      
let apply af pf =
  match pf with
  | Complete _ -> pf
  | Proof (ctx, {a; f}) ->
    let imp_list = List.rev (imp_to_list af f) in
    if imp_list = [f]
      then
        complete_luka pf (Logic.by_assumption f)
      else
        let (beg, res) = begin match imp_list with
        | Neg :: (rs) -> Proof (NegEc {a; f; ctx}, {f = Neg; a}), rs
        | p :: rs when p = f -> Proof (ctx, {a; f}), rs
        | _ -> failwith "niepoprawne dane"
        end in 
        let rec rek imp_l pf=
          match pf with 
          | Complete _ -> failwith "fail"
          | Proof (ctx, {a; f}) -> 
            match imp_l with
            | [] -> pf
            | x :: xs -> rek xs (aph x pf) 
        in rek res beg 






    


let apply_thm (thm : Logic.theorem) pf =
  match pf with
  | Complete _ -> pf
  | Proof (ctx, {a; f}) ->
    if (Logic.consequence thm) = f
      then
        complete_luka pf thm
      else
        let nproof = apply (Logic.consequence thm) pf in
        complete_luka nproof thm

        
        
let apply_assm name pf =
  match pf with
  | Complete _ -> pf
  | Proof (ctx, {a; f}) ->
      let af = List.assoc name a in
      apply_thm (Logic.by_assumption af) pf

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      Logic.pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    Logic.pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()