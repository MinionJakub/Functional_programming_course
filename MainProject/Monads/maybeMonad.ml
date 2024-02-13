module MaybeMonad : Monad.Monad = struct
  type 'a t = 'a option
  let bind m f = match m with 
  | Some x -> f x
  | None -> None
  let ( >>= ) m f = bind m f
  let return x = Some x
end