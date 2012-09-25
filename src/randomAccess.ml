open Sig

module Binary : RANDOMACCESSLIST =
struct

   type 'a tree  = Leaf of 'a | Node of int * 'a tree * 'a tree (* int is size of tree *)
   type 'a digit = Zero | One of 'a tree

   type 'a t = 'a digit list

   exception Empty 
   exception Index

   let empty = []

   let isEmpty = function [] -> true | _ -> false

   let size = function 
     | Leaf x           -> 1
     | Node (w, t1, t2) -> w

   let link t1 t2 = Node (size t1 + size t2, t1, t2)

   let rec insTree = function
     | (t , [])         -> [One t]
     | (t , Zero::ts)   -> One t::ts
     | (t1, One t2::ts) ->  Zero::insTree (link t1 t2, ts )

   let rec borrowTree = function 
     | []               -> raise Empty
     | [One t]          -> t, []
     | One t::ts        -> t, Zero::ts
     |  Zero::ts        -> 
       match borrowTree ts with
	 | Node (_, t1, t2), ts' -> t1, One t2::ts'
	 | _                     -> assert false

   let cons x ts = insTree (Leaf x, ts)

   let head ts = 
     match borrowTree ts with 
       | Leaf x, _  -> x
       | _          -> assert false  

   let tail ts = snd (borrowTree ts)

   let rec lookupTree l i =
     match l with
     | Leaf x              -> if i = 0 then x else raise Index
     | Node (w, t1, t2)    ->
       if i < w/2 
       then lookupTree t1 i 
       else lookupTree t2 (i - w/2)

   let rec updateTree l i y =
     match l with
     | Leaf x             -> if i = 0 then Leaf y else raise Index
     | Node (w, t1, t2)   ->
       if i < w/2 
       then Node (w, updateTree t1 i y, t2)
       else Node (w, t1, updateTree t2 (i - w/2) y)

   let rec lookup l i = 
     match l with
       | []          -> raise Index
       | Zero ::ts   -> lookup ts i
       | One t::ts   ->
         if i < size t 
	 then lookupTree t i
	 else lookup ts (i - size t)

   let rec update l i y =
     match l with 
       | []          -> raise Index
       | Zero ::ts   -> Zero::update ts i y
       | One t::ts   ->
         if i < size t 
	 then One (updateTree t i y)::ts 
	 else One t::update ts (i - size t) y

end
