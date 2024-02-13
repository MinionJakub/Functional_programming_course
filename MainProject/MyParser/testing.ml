open Ast
open Parser



let find_vars question =
  let rec _find_vars question acc = 
    match question with
    | [] -> acc
    | x :: xs -> (
      match x with 
      | VarExp v -> _find_vars xs (x :: acc)
      | ConstExp c -> _find_vars xs acc
      | True -> _find_vars xs acc
      | False -> _find_vars xs acc
      | Atom s -> _find_vars xs acc
      | TermExp (s,el) -> (_find_vars el []) @ (_find_vars xs acc)
    )
  in _find_vars question [];;

let uniq l = 
  let rec tail_uniq a l = 
    match l with
    | [] -> a
    | hd :: tl -> 
      tail_uniq (hd :: a) (List.filter (fun x -> (x <> hd)) tl)
    in tail_uniq [] l;;

let get_queries_and_rules lista = 
  let rec _get_queries_and_rules lista acc = 
    match lista with 
    | [] -> acc
    | x :: xs -> (
      match x with
      | Query ys -> _get_queries_and_rules xs (ys::acc)
      | Rule (val1,val2) -> _get_queries_and_rules xs ((val1 :: val2)  :: acc)
      | _ -> _get_queries_and_rules xs acc
    )
  in _get_queries_and_rules lista [];;

Parser.parse_query_string "cat(tome). cat(finio). animal(X) :- cat(X). animal(X) :- dog(X)." 
|> get_queries_and_rules |> List.hd |> find_vars |> uniq
|> List.fold_left (fun x y -> match y with | VarExp v -> print_endline v | _ -> ()) ();;
