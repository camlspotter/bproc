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

module Monad = Monad.Make1 (struct
  type nonrec 'a t = 'a t

  let bind (gen : 'a t) (f : 'a -> 'b t) st = f (gen st) st

  let return x _st = x

  let map = None
end)

include Monad
module RS = Random.State

let int sz : int t = fun st -> RS.full_int st sz

let int_range (min, max) : int t =
 fun st -> RS.full_int st (max - min + 1) + min

let string int char : string t =
 fun st ->
  let l = int st in
  String.init l (fun _ -> char st)

let bytes int char rng = Bytes.unsafe_of_string @@ string int char rng

let list int n : 'a list t =
 fun st ->
  let l = int st in
  List.init l (fun _ -> n st)

let char : char t = int 256 >|= Char.chr

let alpha_numeric : char t =
  let+ x = int 60 in
  Char.(
    chr
      (if x < 10 then code '0' + x
       else if x < 35 then code 'a' + x - 10
       else code 'A' + x - 35))

let bool : bool t = RS.bool

let elements : 'a list -> 'a t =
 fun xs ->
  assert (xs <> []);
  let+ i = int (List.length xs) in
  List.nth xs i

let elements_array : 'a array -> 'a t =
 fun xs ->
  assert (xs <> [||]);
  let+ i = int (Array.length xs) in
  Array.unsafe_get xs i

let nelements : int -> 'a list -> 'a list t =
 fun n xs rs ->
  let m = List.length xs in
  assert (n <= m);
  let rec loop acc m n = function
    | [] -> List.rev acc
    | xs when m = n -> List.rev (List.rev_append xs acc)
    | x :: xs ->
        (* n/m *)
        if int m rs < n then loop (x :: acc) (m - 1) (n - 1) xs
        else loop acc (m - 1) n xs
  in
  loop [] m n xs

let one_of : 'a t list -> 'a t =
 fun xs ->
  assert (xs <> []);
  let* i = int (List.length xs) in
  List.nth xs i

let shuffle_inplace a st =
  let size = Array.length a in
  for i = 0 to size - 2 do
    let pos = Random.State.int st (size - i - 1) + i in
    let x = Array.unsafe_get a pos in
    Array.unsafe_set a pos @@ Array.unsafe_get a i;
    Array.unsafe_set a i x
  done

let shuffle xs : 'a list t =
 fun st ->
  let a = Array.of_list xs in
  let size = Array.length a in
  for i = 0 to size - 2 do
    let pos = Random.State.int st (size - i - 1) + i in
    let x = Array.unsafe_get a pos in
    Array.unsafe_set a pos @@ Array.unsafe_get a i;
    Array.unsafe_set a i x
  done;
  Array.to_list a

let set (type a) (compare : a -> a -> int) int g =
  let open Monad in
  let module Set = Set.Make(struct
      type t = a
      let compare = compare
    end)
  in
  let rec f acc = function
    | 0 -> return acc
    | n ->
        let* a = g in
        if Set.mem a acc then f acc n
        else f (Set.add a acc) (n-1)
  in
  let* n = int in
  let+ set = f Set.empty n in
  Set.elements set
