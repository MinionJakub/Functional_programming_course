type formula =
| False
| Variable of string
| Implication of formula * formula

let rec string_of_formula f =
  match f with 
  | False -> "⊥"
  | Variable a -> a
  | Implication(a,b) ->
    (
      match a with 
      | False -> String.cat  "⊥ → " (string_of_formula b)
      | Variable a -> String.cat a (String.cat " → " (string_of_formula b))
      | Implication (_,_) -> String.cat (String.cat "(" (String.cat (string_of_formula a) ")")) 
      (String.cat " → " (string_of_formula b))
    )

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem (* = TODO: tu wpisz swoją definicję *)

let assumptions thm =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let consequence thm =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let imp_i f thm =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let imp_e th1 th2 =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let bot_e f thm =
  (* TODO: zaimplementuj *)
  failwith "not implemented"
