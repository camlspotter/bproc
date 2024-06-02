type t = exn

module Format = Xformat

type printer = t -> (Format.formatter -> unit) option

val register_printer : printer -> unit

val pp : t Xformat.printer

val to_string : t -> string

type exn += String of string

val protect : (unit -> 'a) -> (unit -> unit) -> 'a

val report_error : (unit -> 'a) -> 'a

val failwithf : ('a, Format.formatter, unit, 'b) format4 -> 'a

val invalid_argf : ('a, Format.formatter, unit, 'b) format4 -> 'a

module Result : sig
  type 'a t = ('a, exn) result

  module Monad : Monad.S1 with type 'a t := 'a t

  val catch : (unit -> 'a) -> 'a t

  val get_ok : 'a t -> 'a
  (** Raises if error *)
end

(*
module Result_lwt : sig
  type 'a t = 'a Result.t Lwt.t

  module Monad : Monad.S1 with type 'a t := 'a t

  val get_ok_lwt : 'a t -> 'a Lwt.t
  (** Raises if error *)

  val lift_lwt : 'a Lwt.t -> 'a t
  (** Wraps with [Ok]. Note that [lift_lwt] does not catch exceptions.
      Use [catch] if you want to catch exceptions. *)

  val catch : (unit -> 'a Lwt.t) -> 'a t
  (** Catches the exception raised. *)
end
*)
