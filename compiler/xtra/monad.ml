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

module type S1 = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t

  val mapM_ : ('a -> unit t) -> 'a list -> unit t

  val iterM : ('a -> unit t) -> 'a list -> unit t
  (** alias of [mapM_] *)

  val fold_leftM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  val parseM : ('a -> 'b list -> ('a * 'b list) t) -> 'a -> 'b list -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

  val ( >> ) : unit t -> 'b t -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >> ) : unit t -> 'b t -> 'b t
  end

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Make1 (A : sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : (('a -> 'b) -> 'a t -> 'b t) option
end) =
struct
  include A

  let ( >>= ) = bind

  let ( let* ) = bind

  let map = map

  let map_default f at = bind at (fun a -> return @@ f a)

  let map = match map with Some map -> map | None -> map_default

  let ( >>| ) at f = map f at

  let ( >|= ) at f = map f at

  let ( let+ ) at f = map f at

  let ( >> ) ut bt = ut >>= fun () -> bt

  let mapM f xs =
    let rec g rev_st = function
      | [] -> return @@ List.rev rev_st
      | x :: xs -> f x >>= fun x' -> g (x' :: rev_st) xs
    in
    g [] xs

  let mapM_ f xs =
    let rec g = function
      | [] -> return ()
      | x :: xs -> f x >>= fun () -> g xs
    in
    g xs

  let iterM = mapM_

  let rec fold_leftM f acc = function
    | [] -> return acc
    | x :: xs -> f acc x >>= fun acc' -> fold_leftM f acc' xs

  let rec parseM f acc = function
    | [] -> return acc
    | input -> f acc input >>= fun (acc', input') -> parseM f acc' input'

  module Infix = struct
    let ( >>= ) = ( >>= )

    let ( >>| ) = ( >>| )

    let ( >|= ) = ( >|= )

    let ( >> ) = ( >> )
  end

  module Syntax = struct
    let ( let* ) = ( let* )

    let ( let+ ) = ( let+ )
  end
end

module type S2 = sig
  type ('a, 'z) t

  val return : 'a -> ('a, 'z) t

  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

  val map : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t

  val mapM : ('a -> ('b, 'z) t) -> 'a list -> ('b list, 'z) t

  val mapM_ : ('a -> (unit, 'z) t) -> 'a list -> (unit, 'z) t

  val iterM : ('a -> (unit, 'z) t) -> 'a list -> (unit, 'z) t
  (** alias of [mapM_] *)

  val fold_leftM : ('a -> 'b -> ('a, 'z) t) -> 'a -> 'b list -> ('a, 'z) t

  val parseM :
    ('a -> 'b list -> ('a * 'b list, 'error) t) ->
    'a ->
    'b list ->
    ('a, 'error) t

  val ( >>= ) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

  val ( >>| ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t

  val ( >|= ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t

  val ( >> ) : (unit, 'z) t -> ('b, 'z) t -> ('b, 'z) t

  val ( let* ) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

  val ( let+ ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t

  module Infix : sig
    val ( >>= ) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

    val ( >>| ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t

    val ( >|= ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t

    val ( >> ) : (unit, 'z) t -> ('b, 'z) t -> ('b, 'z) t
  end

  module Syntax : sig
    val ( let* ) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

    val ( let+ ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t
  end
end

module Make2 (A : sig
  type ('a, 'z) t

  val return : 'a -> ('a, 'z) t

  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t

  val map : (('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t) option
end) =
struct
  include A

  let ( >>= ) = bind

  let ( let* ) = bind

  let map_default f at = bind at (fun a -> return @@ f a)

  let map = match map with Some map -> map | None -> map_default

  let ( >>| ) at f = map f at

  let ( >|= ) at f = map f at

  let ( let+ ) at f = map f at

  let ( >> ) ut bt = ut >>= fun () -> bt

  let mapM f xs =
    let rec g rev_st = function
      | [] -> return @@ List.rev rev_st
      | x :: xs -> f x >>= fun x' -> g (x' :: rev_st) xs
    in
    g [] xs

  let mapM_ f xs =
    let rec g = function
      | [] -> return ()
      | x :: xs -> f x >>= fun () -> g xs
    in
    g xs

  let iterM = mapM_

  let rec fold_leftM f acc = function
    | [] -> return acc
    | x :: xs -> f acc x >>= fun acc' -> fold_leftM f acc' xs

  let rec parseM f acc = function
    | [] -> return acc
    | input -> f acc input >>= fun (acc', input') -> parseM f acc' input'

  module Infix = struct
    let ( >>= ) = ( >>= )

    let ( >>| ) = ( >>| )

    let ( >|= ) = ( >|= )

    let ( >> ) = ( >> )
  end

  module Syntax = struct
    let ( let* ) = ( let* )

    let ( let+ ) = ( let+ )
  end
end
