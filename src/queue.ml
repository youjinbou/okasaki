open Sig

module Batched : QUEUE =
struct

  type 'a t = {f : 'a list; r : 'a list}

  exception Empty

  let empty = { f = []; r = [] }

  let is_empty q = q.f = []

  let queue = function 
    | {f = []; r = r } -> { f = List.rev r; r = [] } 
    | q                -> q

  let snoc q x = queue {f = q.f ; r = x::q.r }

  let head = function 
    | { f = []; r = _ }    -> raise Empty
    | { f = x::f'; r = _ } -> x

  let tail = function 
    | { f = []; r = _ }         -> raise Empty
    | { f = x::f'; r = _ } as q -> { q with f = f' }


end

module Banker : QUEUE = 
struct

  open Stream

  type 'a t = { f: 'a stream; lenf : int; r : 'a stream; lenr : int }
      (* Invariants: |F| >= |R|, LenF = |F|, LenR = |R| *)
      
  exception Empty

  let empty = { f = lazy Nil; lenf = 0; r = lazy Nil; lenr = 0}

  let is_empty q = q.lenf = 0

  let queue q = if q.lenf >= q.lenr then q
    else { f = q.f ++ reverse q.r; lenf = q.lenf + q.lenr; r = lazy Nil; lenr = 0 }

  let snoc q x = queue {f = q.f; lenf = q.lenf; r = lazy (Cons (x,q.r)); lenr = succ q.lenr}

  let head q = match q.f with 
    | lazy Nil          -> raise Empty
    | lazy (Cons (x,f)) -> x
   
  let tail q = match q.f with 
    | lazy Nil          -> raise Empty
    | lazy (Cons (x,f)) -> queue {q with lenf = pred q.lenf}

end

module Physicist : QUEUE = 
struct

  type 'a t = { w : 'a list; f : 'a list Lazy.t; lenf : int; r : 'a list; lenr : int }
      (* Invariant : w is a prefix of force f, w = [] if f = lazy [] *)
      (*              |F| >= |R|, LenF = |F|, LenR = |R|             *)

  exception Empty

  let empty = {w = []; f = lazy [];lenf = 0; r = []; lenr = 0}

  let is_empty q = q.lenf = 0

  let checkw q = match q.w with
      [] -> {w = Lazy.force q.f; f = q.f; lenf = q.lenf; r = q.r; lenr = q.lenr}
    | _  -> q

  let checkr q = 
    if q.lenr <= q.lenf then q
    else let w' = Lazy.force q.f in
	 { w = w'; f = lazy (w' @ List.rev q.r); lenf = q.lenf + q.lenr; r = []; lenr = 0 }

  let queue q = checkw (checkr q)

  let snoc q x = queue { q with r = x::q.r; lenr = succ q.lenr }

  let head q = match q.w with
      []   -> raise Empty
    | x::_ -> x

  let tail q = match q.w with
      []   -> raise Empty
    | x::w -> queue { q with w = w; f = lazy (List.tl (Lazy.force q.f)); lenf = pred q.lenf}

end

module RealTime : QUEUE =
struct

  open Stream

  type 'a t = { f : 'a stream; r : 'a list; s : 'a stream }
      (* invariant : |s| = |f| - |r| *)

  exception Empty

  let empty = {f = lazy Nil; r = []; s = lazy Nil}

  let is_empty q = q.f = lazy Nil

  let rec rotate (f : 'a stream) (r : 'a list) (lazy a) = lazy (
    match f,r with
	lazy Nil          , y::_      -> Cons (y,lazy a)
      | lazy (Cons (x,f')), y::r'     -> Cons (x, rotate f' r' (lazy (Cons (y,lazy a))))
      | lazy Nil          , []        -> a
      | lazy f            , []        -> failwith "lazy f"
  )

  let rec queue q = match q.s with
      lazy (Cons (x,s)) -> { q with s = s }
    | lazy Nil          -> let f' = rotate q.f q.r (lazy Nil) in 
			   queue {f = f'; r = []; s = f'} 

  let snoc q x = queue { q with r = x::q.r }
  let head q = match q.f with 
      lazy Nil          -> raise Empty
    | lazy (Cons (x,_)) -> x

  let tail q = match q.f with
      lazy Nil           -> raise Empty
    | lazy (Cons (x,f')) -> {q with f = f'}

end

