(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(** Getting values from the unix environment variables *)

type 'a var

val string : string -> string option var
(** Declaration of variable of string *)

val int : string -> int option var
(** Declaration of variable of int *)

val bool : string -> bool option var
(** Declaration of variable of bool *)

val flag : string -> bool var
(** Declaration of variable of a flag *)

val one_of : string -> (string * 'a) list -> 'a option var
(** Declaration of variable of choice *)

val custom : string -> (string -> 'a) -> 'a option var
(** Declaration of variable of custom value with a parser *)

val with_default : 'a -> 'a option var -> 'a var
(** with default value *)

val get : 'a var -> 'a
(** Get the value of a variable. *)

val get_with_default : 'a -> 'a option var -> 'a

val print_list : Format.formatter -> unit -> unit
(** Get the declared variable list *)

val bind : unit -> unit
(** Scan the environment and bind values of the previously defined
    variables.

    No need to call [bind] explicitly; the first call of [get] implicitly
    calls [bind].

    [bind] fails if it is called more than once.
*)
