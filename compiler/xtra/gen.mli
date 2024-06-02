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

type 'a t = Random.State.t -> 'a
(** The type of the generator *)

include Monad.S1 with type 'a t := 'a t

module Monad : Monad.S1 with type 'a t := 'a t
(** Generator is a monad *)

val int : int -> int t
(** [0..n-1] *)

val int_range : int * int -> int t
(** Inclusive *)

val char : char t
(** Including non ASCIIs *)

val alpha_numeric : char t
(** [0-9A-Za-z] *)

val bool : bool t

val string : int t -> char t -> string t

val bytes : int t -> char t -> bytes t

val list : int t -> 'a t -> 'a list t

val elements : 'a list -> 'a t

val elements_array : 'a array -> 'a t

val nelements : int -> 'a list -> 'a list t
(** [nelemennts n xs] chooses [n] random elements from [xs].
    The order is preserved.  It fails when [n > List.length xs].
*)

val one_of : 'a t list -> 'a t
(** Randomly choose one of the generators *)

val shuffle : 'a list -> 'a list t

val shuffle_inplace : 'a array -> unit t

val set : ('a -> 'a -> int) -> int t -> 'a t -> 'a list t
(** Generate distinct elements *)
