module type STREAM =
sig
  type 'a cell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a cell Lazy.t

  val (++)    : 'a stream  -> 'a stream  -> 'a stream (* stream append *)
  val take    : int -> 'a stream -> 'a stream
  val drop    : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end


module type SORTABLE =
sig

  type 'a t

  val create : ('a -> 'a -> bool) -> 'a t
  val add : 'a -> 'a t -> 'a t
  val sort : 'a t -> 'a list

end

module type QUEUE =
sig

  type 'a t

  exception Empty

  val empty : 'a t

  val is_empty : 'a t -> bool

  val snoc : 'a t -> 'a -> 'a t

  val head : 'a t -> 'a 
  val tail : 'a t -> 'a t

end

module type DEQUE =
sig
   type 'a t

   exception Empty

     
   val empty : 'a t
     
   val isEmpty : 'a t -> bool

   (* insert, inspect, and remove the front element *)
   val cons : 'a t -> 'a -> 'a t
   val head : 'a t -> 'a             (* raises Empty if queue is empty *)
   val tail : 'a t -> 'a t           (* raises Empty if queue is empty *)

   (* insert, inspect, and remove the rear element *)
   val snoc : 'a t -> 'a -> 'a t
   val last : 'a t -> 'a             (* raises Empty if queue is empty *)
   val init : 'a t -> 'a t           (* raises Empty if queue is empty *)

end



