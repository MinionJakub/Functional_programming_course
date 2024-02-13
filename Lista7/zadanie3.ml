module IdMonad = struct
  type 'a t = 'a
  let return (elem : 'a) = elem
  let bind (elem : 'a t) (func : 'a -> 'b t) = func elem
end

module PostponmentMonad = struct
  type 'a t = unit -> 'a
  let return (elem : 'a) = fun () -> elem
  let bind (elem : 'a t) (func : 'a -> 'b t) = func (elem ())
end