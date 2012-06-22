
module Banker : functor (C : sig val c : int end) -> Sig.DEQUE

module RealTime : functor (C : sig val c : int end) -> Sig.DEQUE
