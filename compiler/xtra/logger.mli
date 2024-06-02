(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type level = Debug | Info | Notice | Warning | Error | Fatal

module type PRINTER = sig
  val log :
    string list -> level -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

module type S = sig
  val log : level -> ('a, Format.formatter, unit, unit) format4 -> 'a

(*
  val lwt_log : level -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
*)

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val notice : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warning : ('a, Format.formatter, unit, unit) format4 -> 'a

  val error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val fatal : ('a, Format.formatter, unit, unit) format4 -> 'a

(*
  val lwt_debug : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_info : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_notice : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_warning : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_error : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_fatal : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
*)
end

module Make (_ : sig
  val env_var : string
  (** environment variable name to get the log level *)
end) : sig
  val override : (module PRINTER) -> unit
  (** Replace the reporting printer. *)

  val make : string list -> (module S)
  (** [make section_name] *)
end
