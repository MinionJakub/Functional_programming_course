exception Error
let for_all p xs = 
  let helper = fun acc x ->
    if p x then acc else raise Error in
  try List.fold_left helper true xs with Error -> false;;
let mult_list xs = 
  let helper = (fun acc x -> if x = 0 then raise Error else acc * x) in
  try List.fold_left helper 1 xs with Error -> 0;;
let sorted xs = 
  let helper = 
    (fun (res,prev) x -> 
      begin match prev with 
      | None -> (res, Some x) 
      | Some elem -> 
        if x <= elem 
          then raise Error 
        else (res,(Some x))
      end)
  in
  try List.fold_left helper (true,None) xs |> fst with Error -> false;;