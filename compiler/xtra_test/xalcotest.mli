open Alcotest

val quick : string -> ('a -> unit) -> string * 'a test_case list

val quicks : string -> ('a -> unit) list -> string * 'a test_case list

val slow : string -> ('a -> unit) -> string * 'a test_case list

val slows : string -> ('a -> unit) list -> string * 'a test_case list
