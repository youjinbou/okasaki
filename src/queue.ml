open Sig

module Batched : QUEUE =
struct

  type 'a t = {f : 'a list; r : 'a list}

  let empty = { f = []; r = [] }

  let is_empty q = q.f = []

  let queue = function 
    | {f = []; r = r } -> { f = List.rev r; r = [] } 
    | q                -> q

  let snoc q x = queue {f = q.f ; r = x::q.r }

  let head = function 
    | { f = []; r = _ }    -> raise Not_found
    | { f = x::f'; r = _ } -> x

  let tail = function 
    | { f = []; r = _ }         -> raise Not_found
    | { f = x::f'; r = _ } as q -> { q with f = f' }

end

module Banker : QUEUE = 
struct

  open Stream

  type 'a t = { f: 'a stream; lenf : int; r : 'a stream; lenr : int }
      (* Invariants: |F| >= |R|, LenF = |F|, LenR = |R| *)
      
  let empty = { f = lazy Nil; lenf = 0; r = lazy Nil; lenr = 0}

  let is_empty q = q.lenf = 0

  let queue q = if q.lenf >= q.lenr then q
    else { f = q.f ++ reverse q.r; lenf = q.lenf + q.lenr; r = lazy Nil; lenr = 0 }

  let snoc q x = queue {f = q.f; lenf = q.lenf; r = lazy (Cons (x,q.r)); lenr = succ q.lenr}

  let head q = match q.f with 
    | lazy Nil          -> raise Not_found
    | lazy (Cons (x,f)) -> x
   
  let tail q = match q.f with 
    | lazy Nil          -> raise Not_found
    | lazy (Cons (x,f)) -> queue {q with lenf = pred q.lenf}

end

module Physicist : QUEUE = 
struct

  type 'a t = { w : 'a list; f : 'a list Lazy.t; lenf : int; r : 'a list; lenr : int }
      (* Invariant : w is a prefix of force f, w = [] if f = lazy [] *)
      (*              |F| >= |R|, LenF = |F|, LenR = |R|             *)

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
      []   -> raise Not_found
    | x::_ -> x

  let tail q = match q.w with
      []   -> raise Not_found
    | x::w -> queue { q with w = w; f = lazy (List.tl (Lazy.force q.f)); lenf = pred q.lenf}

end

module RealTime : QUEUE =
struct

  open Stream

  type 'a t = { f : 'a stream; r : 'a list; s : 'a stream }
      (* invariant : |s| = |f| - |r| *)

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
      lazy Nil          -> raise Not_found
    | lazy (Cons (x,_)) -> x

  let tail q = match q.f with
      lazy Nil           -> raise Not_found
    | lazy (Cons (x,f')) -> {q with f = f'}

end

module Bootstrapped : QUEUE = 
struct
  (* assumes polymorphic recursion!
     only available from version 3.12 of OCaml
  *)
  
  type 'a t = 
    | Empty
    | Queue of 'a q
  and 'a q = { f : 'a list; m : 'a list Lazy.t t; lenfm : int; r : 'a list; lenr : int }

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _     -> false

  let rec queue : 'a. 'a q -> 'a t = fun q ->
    if q.lenr <= q.lenfm 
    then check_f q
    else check_f { f = q.f; m = snoc q.m (lazy (List.rev q.r)); lenfm = q.lenfm + q.lenr; r = []; lenr = 0 }

  and check_f : 'a. 'a q -> 'a t = function
    | { f = []; m = Empty } -> Empty
    | { f = [] } as q       -> Queue { q with f = Lazy.force (head q.m); m = tail q.m }
    |  q                    -> Queue q
      
  and snoc : 'a. 'a t -> 'a -> 'a t = fun q x ->  
    match q with
    | Empty     -> Queue { f = [x]; m = Empty; lenfm = 1; r = []; lenr = 0 }
    | Queue q   ->
      queue { f = q.f; m = q.m; lenfm = q.lenfm; r = x::q.r; lenr = succ q.lenr }
      
  and head : 'a. 'a t -> 'a = function
    | Empty                -> raise Not_found
    | Queue { f = x::f' }  -> x
    | _                    -> assert false

  and tail : 'a. 'a t -> 'a t = function
    | Empty                     -> raise Not_found
    | Queue ({ f = x::f } as q) -> queue q 
    | _                         -> assert false

end
