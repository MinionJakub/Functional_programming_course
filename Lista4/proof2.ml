(* open Logic *)

type goal = {f: Logic.formula; a: (string * Logic.formula) list} 
(* Logic.formula * (string * Logic.formula) list *)

type proof_tree = 
| Goal of goal
| Lema of Logic.theorem
| ImplieI of {a : (string * Logic.formula) list; f: Logic.formula; x: proof_tree}
| ImplieE of {a : (string * Logic.formula) list; f: Logic.formula; x: proof_tree; y: proof_tree}
| BotE of {a : (string * Logic.formula) list; f: Logic.formula; x: proof_tree}

type proof_tree_ctx = 
| Root
| BotEc of {a : (string * Logic.formula) list; f: Logic.formula; ctx: context} 
| ImplieEyc of {a : (string * Logic.formula) list; f: Logic.formula; x: proof_tree ;ctx: context}
| ImplieExc of {a : (string * Logic.formula) list; f: Logic.formula; y: proof_tree ;ctx: context}
| ImplieIc of {a : (string * Logic.formula) list; f: Logic.formula; ctx: context} 

type proof =
| Complete of Logic.theorem
| Incomplete of proof_tree_ctx * goal

let proof goal formula =
    let g = {f = formula; a = goal} in Incomplete (Root,g);

let qed pf = match pf with | Comp th -> th | Incomplete _ -> failwith "Holes" ;



let goal pf =
    match pf with  
    | Complete (th) -> None
    | Incomplete (_, goal) -> Some goal;
  


let up_ctx (p_t : proof_tree) (ctx : proof_tree_ctx) = 
  match ctx with
  | Root -> (p_t,ctx)
  | BotEc {a;f;ctx} -> BotE {a;d;x=p_t}, ctx
  | ImplieIc {a;f;ctx} -> ImplieI{a;f;x=p_t},ctx
  | ImplieExc {a;f;y;ctx} -> ImplieE {a;f;x = p_t; y}, ctx
  | ImplieEyc {a;f;x;ctx} -> ImplieE {a;f;x;y = p_t}, ctx;

let rec down_left (p_t : proof_tree) (ctx : proof_tree_ctx) =
  match p_t with 
  | Goal g -> Incomplete(ctx,g)
  | Lema _ -> failwith "something wrong"
  | ImplieI {a;f;x} -> down_left x (ImplieIc {a;f;ctx})
  | BotE {a;f;x} -> down_left x (BotEc {a;f;ctx})
  | ImplieE {a;f;x;y} ->
    begin match x with 
    | Lema _ -> down_left y (ImplieEyc {a;f;x;ctx})
    | _ -> down_left x (ImplieExc {a;f;y;ctx})
  end;

let rec up_right (p_t : proof_tree) (ctx : proof_tree_ctx) = 
  match ctx with 
  | Root -> down_left p_t ctx
  | BotEc {a;f;ctx} -> up_right (BotE {a;f;x =p_t}) ctx
  | ImplieIc {a;f;ctx} -> up_right (ImplieI {a;f;x = p_t}) ctx
  | ImplieEyc {a;f;x;ctx} -> up_right (ImplieE {a;f;x;y=p_t}) ctx
  | ImplieExc {a;f;y;ctx} ->
    begin match y with 
    | Lema _ -> up_right (ImplieE {a;f;x = p_t;y}) ctx
    | _ -> down_left y (ImplieEyc {a;f;x = p_t; ctx})
  end;

let next pf =
  match pf with
  | Complete (th) -> failwith "No holes"
  | Incomplete (ctx, goal) -> up_right (Goal goal) ctx;

let add_assum (s,f) (a : (string * Logic.formula) list) = 
  if List.exists (fun (ls,lf) -> ls = s) a
    then failwith "There is lema of this name"
  else (s,f) :: a;

let rem_form (f: Logic.formula) (a : (string * Logic.formula) list) =
  List.filter (fun (s,lf) -> not (lf=f)) a;


let intro name pf =
  match pf with
  | Complete _ -> failwith "No holes"
  | Incomplete (ctx,{f;a}) -> 
    match f with 
    | Variable _ -> failwith "It should be Imp"
    | Neg -> failwith "It should be Imp"
    | Implication (q,r) -> let g = {f = r; a = add_assum(name,q) a} in
      Incomplete((ImplieIc {a;f;ctx}),g);

let aph af pf = 
  match pf with
  | Complete _ -> failwith "complete proof"
  | Incomplete (ctx,{f;a}) -> let rgt = Goal{f=af;a} in
  let lg = {f = Implie(af,f);a} in 
  Incomplete (ImplieExc {a;f;y=rgt;ctx},lg);

let rec  imp_to_list f gf =
  match f with
  | Neg -> [Neg]
  | Implication (a,b) when b = gf -> a ::[gf]
  | Implication (a,b) -> a :: (imp_to_list b gf)
  | Variable s when f = Var s -> [f]
  | Variable s -> failwith "expected negation or formula to proof";

let rec complete_goal (p_t, ctx : proof_tree * proof_tree_ctx) (th : Logic.theorem) = 
  match ctx with 
  | Root -> Complete th
  | BotEc {a;f;ctx} -> complete_goal (up_ctx (Lema th) ctx) (Logic.bot_e f th)
  | ImplieIc {a;f;ctx} -> complete_goal (up_ctx (Lema th) ctx) (Logic.imp_i f th)
  | ImplieEyc {a;f;x;ctx} -> 
    begin match x with 
    | Lema l -> complete_goal (up_ctx (Lema th) ctx) (imp_e th l)
    | _ -> down_left x (ImplieExc {a;f;y=(Lema th);ctx})
  end
  | ImplieEyc {a;f;y;ctx} ->
    begin match y with 
    | Lema l -> complete_goal (up_ctx (Lema th) ctx) (imp_e th l)
    | _ -> down_left y (ImplieEyc {a;f;x=(Lema th);ctx})
  end;

let complete_luke pf th = 
  match pf with
  | Complete _ -> pf
  | Incomplete (ctx,{a;f}) ->
    if consequence th = f 
      then complete_goal (Goal {a;f},ctx) th
  else failwith "Bad Theory";

let apply f pf =
  match pf with
  | Complete _ -> pf
  | Incomplete (ctx,{a;f}) ->
    let imp_list = List.rev (imp_to_list af f) in
    if imp_list = [f]
      then complete_luke pf (by_assumption f)
  else 
    let (beg,res) = begin match imp_list with
    | Bot :: (rs) -> Incomplete (BotEc {a;f;ctx},{f=Bot ;a}),rs
    | p :: rs when p = f -> Incomplete (ctx, {a;f}) ,rs
    | _ -> failwith "incorrect data"
  end in let rec rek imp_l pf =
    match pf with 
    | Complete _ -> failwith "fail"
    | Incomplete (ctx,{a;f}) -> 
      match imp_l with
      | [] -> pf
      | x :: xs -> rek xs (aph x pf)
    in rek res beg;
  
  (* TODO: zaimplementuj
  failwith "not implemented" *)

let apply_thm thm pf =
  match pf with
  | Complete _ -> pf
  | Incomplete (ctx,{a;f})->
    if (consequence thm) = f then
      complete_luke pf thm
    else
      let nproof = apply (consequence thm) pf in
      complete_luke nproof thm;
  (* TODO: zaimplementuj
  failwith "not implemented" *)

let apply_assm name pf =
  match pf with
  | Complete _ -> pf
  | Incomplete (ctx,{a;f}) ->
    let af = List.assoc name a in apply_thm (by_assumption af) pf;
  (* TODO: zaimplementuj
  failwith "not implemented" *)

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
    Format.pp_close_box fmtr ();