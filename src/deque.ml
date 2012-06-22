open Sig

module Banker (C : sig val c : int end) : DEQUE =  (* c>1 *)
struct
 
  let c = C.c
  
  open Stream

  type 'a t = { f : 'a stream; lenf : int; r : 'a stream; lenr : int }
  (* Invariants: |F| <= c|R| + 1, |R| <= c|F| + 1, lenf = |F|, lenr = |R| *)

  exception Empty

  let empty =  { f = lazy Nil; lenf = 0; r = lazy Nil; lenr = 0 }

  let isEmpty q = q.lenr + q.lenf = 0

  let queue q =
    if q.lenf > c * q.lenr + 1 
    then
      let i  = (q.lenf + q.lenr) / 2 in
      let j  = q.lenr + q.lenf - i in
      let f' = take i q.f
      and r' = q.r ++ reverse (drop i q.f) in
      { f = f'; lenf = i ; r = r'; lenr = j }
    else if q.lenr > c * q.lenf + 1 
    then
      let i  = (q.lenf + q.lenr) / 2 in
      let j  = q.lenf + q.lenr - i in
      let f' = q.f ++ reverse (drop j q.r) 
      and r' = take j q.r in
      { f = f'; lenf = i; r = r'; lenr = j }
    else q

  let cons q x = 
    queue { q with f = lazy (Cons (x ,q.f)); lenf = succ q.lenf }

  let head = function 
    | { f = lazy Nil; r = lazy Nil }          -> raise Empty (* empty *)
    | { f = lazy Nil; r = lazy (Cons (x,_)) } -> x           (* one element *)
    | { f = lazy (Cons (x, f)) }              -> x

  let tail = function
    | { f = lazy Nil; r = lazy Nil }           -> raise Empty
    | { f = lazy Nil; r = lazy (Cons (x,_)) }  -> empty
    | { f = lazy (Cons (x,f')) } as q           -> 
      queue { q with f = f'; lenf = pred q.lenf }

  let snoc q x =
    queue { q with r = lazy (Cons (x ,q.r)); lenr = succ q.lenr }

  let last = function 
    | { f = lazy Nil; r = lazy Nil }          -> raise Empty (* empty *)
    | { f = lazy (Cons (x,_)); r = lazy Nil } -> x           (* one element *)
    | { r = lazy (Cons (x, f)) }              -> x

  let init = function
    | { f = lazy Nil; r = lazy Nil }          -> raise Empty
    | { f = lazy (Cons (x,_)); r = lazy Nil } -> empty
    | { r = lazy (Cons (x,r')) } as q         -> 
      queue { q with r = r'; lenr = pred q.lenr }

end


module RealTime (C : sig val c : int end) : DEQUE =     (* c = 2 or c = 3 *)
struct

  open Stream

  let c = C.c

  type 'a t = { f : 'a stream; lenf : int; sf : 'a stream;
                r : 'a stream; lenr : int; sr : 'a stream }

   (* Invariants: |F| c |R| + 1, |R| c |F| + 1, lenf = |F|, lenr = |R| *)

   exception Empty

   let empty = { f = lazy Nil; lenf = 0; sf = lazy Nil; 
		 r = lazy Nil; lenr = 0; sr = lazy Nil }

   let isEmpty q = q.lenf + q.lenr = 0

   let exec1 = function
     | (lazy (Cons (x , s ))) -> s
     | s                      -> s

   let exec2 s = exec1 (exec1 s )

   let rec rotateRev = function
     | lazy Nil, f, a             -> reverse f ++ a
     | lazy (Cons (x, r)), f, a  ->
       lazy (Cons (x, rotateRev (r, drop c f, reverse (take c f) ++ a)))

   let rec rotateDrop r  i  f =
     if i < c 
     then rotateRev (r, drop i f, lazy Nil)
     else match r with
       | lazy (Cons (x, r')) -> lazy (Cons (x, rotateDrop r' (i - c) (drop c f )))
       | _                   -> assert false

  let queue q =
    if q.lenf > c * q.lenr + 1 
    then
      let i  = (q.lenf + q.lenr) / 2 in
      let j  = q.lenr + q.lenf - i in
      let f' = take i q.f
      and r' = q.r ++ reverse (drop i q.f) in
      {f = f'; lenf = i ; sf = f'; r = r'; lenr = j; sr = r' }
    else if q.lenr > c * q.lenf + 1 
    then
      let i  = (q.lenf + q.lenr) / 2 in
      let j  = q.lenf + q.lenr - i in
      let f' = q.f ++ reverse (drop j q.r) 
      and r' = take j q.r in
      { f = f'; lenf = i; sf = f'; r = r'; lenr = j; sr = r' }
    else q

   let cons q x =
     queue { f = lazy (Cons (x , q.f)); lenf = succ q.lenf; sf = exec1 q.sf;
             r = q.r; lenr = q.lenr; sr = exec1 q.sr}

   let head = function 
     | { f = lazy Nil; r = lazy Nil }            -> raise Empty
     | { f = lazy Nil; r = lazy (Cons (x ,_)) }  -> x
     | { f = lazy (Cons (x ,_)) }                -> x

   let tail = function 
     | { f = lazy Nil; r = lazy Nil }            -> raise Empty
     | { f = lazy Nil; r = lazy (Cons (x, _)) }  -> empty
     | { f = lazy (Cons (x, f')) } as q          ->
     queue { q with f = f'; lenf = pred q.lenf; sf = exec2 q.sf; sr = exec2 q.sr }


   let snoc q x =
     queue  { f = q.f; lenf = q.lenf; sf = exec1 q.sf;
	      r = lazy (Cons (x , q.r)); lenr = succ q.lenr; sr = exec1 q.sr }
             
   let last = function 
     | { f = lazy Nil; r = lazy Nil }            -> raise Empty
     | { f = lazy (Cons (x ,_)); r = lazy Nil }  -> x
     | { r = lazy (Cons (x ,_)) }                -> x

   let init = function 
     | { f = lazy Nil; r = lazy Nil }            -> raise Empty
     | { f = lazy (Cons (x, _)); r = lazy Nil }  -> empty
     | { r = lazy (Cons (x, r')) } as q          ->
     queue { q with r = r'; lenr = pred q.lenr; sf = exec2 q.sr; sr = exec2 q.sr }


end
