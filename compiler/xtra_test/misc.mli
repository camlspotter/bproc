open Xtra

val regression_by_hash :
  ?runs:int -> string -> int -> (Random.State.t -> 'a) -> unit

val ok_or_fail : 'a Exn.Result.t -> 'a

val must_fail : ('a, 'b) result -> unit

val must_raise : (unit -> 'a) -> unit
