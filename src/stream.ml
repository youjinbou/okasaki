
type 'a cell = Nil | Cons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

let rec (++) s t = lazy (
  match s with
      lazy Nil           -> Lazy.force t
    | lazy (Cons (x,s')) -> Cons (x, s' ++ t)
)

let rec take n s = lazy (
  match n, s with
      0, _                   -> Nil
    | _,lazy Nil             -> Nil
    | _,lazy (Cons (x , s')) -> Cons (x, take (pred n) s')
)

let drop n s = 
  let rec drop' = function 
    | (0, lazy c)               -> c
    | (n, lazy Nil)             -> Nil
    | (n, lazy (Cons (x , s'))) -> drop' (n - 1, s')
  in lazy (drop' (n , s ))
  
let reverse s = 
  let rec reverse' = function
    | lazy Nil, r         -> r
    | lazy (Cons (x,s)),r -> reverse' (s,Cons (x,lazy r))
  in lazy (reverse' (s , Nil))
