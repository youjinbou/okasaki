open Sig

module BottomUpMerge : SORTABLE =
struct

  open List

  type 'a t = {less : 'a -> 'a -> bool; size : int; segments : 'a list list Lazy.t }

  let merge less xs ys = 
    let rec mrg xs ys = match xs, ys with
      | [], _         -> ys
      | _, []         -> xs
      | x::xs',y::ys' -> if less x y then x::(mrg xs' ys) else y::(mrg xs ys')
    in mrg xs ys

  let create less = {less = less; size = 0; segments = lazy []}

  let add x s = 
    let rec addseg seg segs size = 
      if size mod 2 = 0 then seg::segs 
      else addseg (merge s.less seg (hd segs)) (tl segs) (size / 2)
    in {less = s.less; size = succ s.size; segments = lazy (addseg [x] (Lazy.force s.segments) s.size)}
	
  let sort s = 
    let rec mergeall xs ys = 
      match ys with
	| []        -> xs
	| seg::segs -> mergeall (merge s.less xs seg) segs
    in mergeall [] (Lazy.force s.segments)

end


module ScheduledBottomUpMerge : SORTABLE =
struct

  open Stream
  open List

  type 'a schedule = 'a stream list
  type 'a t = { less : 'a -> 'a -> bool; size : int; segments : ('a stream * 'a schedule) list }

  let merge less xs ys =
    let rec mrg xs yz =
      match xs, ys with 
	  lazy Nil, _ -> ys
	| _, lazy Nil -> xs
        | lazy (Cons (x, xs')), lazy (Cons (y, ys')) ->
          if less x y 
	  then lazy (Cons (x , mrg xs' ys ))
          else lazy (Cons (y , mrg xs  ys'))
    in mrg xs ys
	  
  let rec exec1 = function
    | []                             -> []
    | (lazy Nil)::sched              -> exec1 sched
    | (lazy (Cons (x , xs )))::sched -> xs :: sched

  let rec exec2_per_seg = function
    | []                     -> []
    | ((xs , sched) :: segs) -> (xs , exec1 (exec1 sched))::(exec2_per_seg segs)

  let create less = { less = less ; size = 0; segments = [] }

  let add x s =
    let rec addseg xs segs size rsched =
      if size mod 2 = 0
      then (xs,rev (xs::rsched))::segs
      else match segs with
	  (xs',[])::segs' -> addseg (merge s.less xs xs') segs' (size / 2) (xs::rsched)
	| _               -> assert false
    in 
    let segs' = addseg (lazy (Cons (x, lazy Nil))) s.segments s.size []
    in { less = s.less; size = succ s.size; segments = exec2_per_seg segs' }
    
  let sort s =
    let rec mergeall xs ys = 
      match ys with
	  []                -> xs
	| (xs',sched)::segs -> mergeall (merge s.less xs xs') segs
    and streamtolist = function 
      | lazy Nil              -> []
      | lazy (Cons (x , xs )) -> x :: streamtolist xs
    in streamtolist (mergeall (lazy Nil) s.segments)

end
