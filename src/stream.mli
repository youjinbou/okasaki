
type 'a cell = Nil | Cons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

val (++)    : 'a stream  -> 'a stream  -> 'a stream
val take    : int -> 'a stream -> 'a stream
val drop    : int -> 'a stream -> 'a stream
val reverse : 'a stream -> 'a stream
