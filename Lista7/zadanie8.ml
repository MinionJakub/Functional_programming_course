type symbol = string
type 'v term = 
| Var of 'v
| Sym of symbol * 'v term list

let reurn v = Var v
let bind epxr sub = 
  let rec helper expr = 
    match expr with
    | Var v -> sub v
    | Sym (s,xt) -> Sym(s,List.map helper expr)
  in helper expr