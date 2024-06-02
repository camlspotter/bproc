(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

module Format = Xformat
module Monad = Monad
module Exn = Exn
(* module Lwt = Xlwt *)
module Envconf = Envconf
module List = Xlist
module Gen = Gen
module Yojson = Xyojson
module Yojson_conv = Xyojson_conv
module Logger = Logger
module Span = Span
module Bench = Bench
module Option = Xoption

(*
module String = struct
  include String

  let find_char p s pos =
    let len = String.length s in
    let rec f pos =
      if len <= pos then None
      else if p @@ String.unsafe_get s pos then Some pos
      else f (pos+1)
    in
    f pos

  let split_by_char p s =
    let len = String.length s in
    let rec f rev_list start pos =
      match find_char p s pos with
      | None -> List.rev_map (fun (a,b) -> String.sub s a b) @@ (start, len - start) :: rev_list
      | Some pos' ->
          f ((start, pos' - start) :: rev_list) (pos'+1) (pos'+1)
    in
    f [] 0 0

  let () =
    assert (split_by_char (function '/' -> true | _ -> false) "/1/23//456/" = [""; "1"; "23"; ""; "456"; ""]);
    assert (split_by_char (function '/' -> true | _ -> false) "/" = [""; ""]);
    assert (split_by_char (function '/' -> true | _ -> false) "" = [""])

  let for_all f s =
    let len = String.length s in
    let rec aux = function
      | -1 -> true
      | i ->
          let c = String.unsafe_get s i in
          if f c then aux (i-1) else false
    in
    aux (len - 1)

  module Set = Set.Make(String)
  module Map = Map.Make(String)
end

module List = struct
  include List

  let rec drop n xs = match n, xs with
    | 0, xs -> xs
    | n, _ when n < 0 -> assert false
    | n, _::xs -> drop (n-1) xs
    | _, [] -> []

  let split_at n xs =
    let rec split_at_ n st xs =
      if n <= 0 then st, xs
      else match xs with
      | [] -> st, []
      | x::xs -> split_at_ (n-1) (x::st) xs
    in
    let r, dropped = split_at_ n [] xs in
    rev r, dropped

  let take n xs = fst @@ split_at n xs

  let () = assert (split_at 10 [1;2;3] = ([1;2;3], []))

  let uniq_sorted eq xs =
    let rec f = function
      | [] -> []
      | x::(y::_ as xs) when eq x y -> f xs
      | x :: xs -> x :: f xs
    in
    f xs

  let () =
    assert (uniq_sorted (=) [1;2;3;4;5] = [1;2;3;4;5]);
    assert (uniq_sorted (=) [1;1;2;3;3;4;5;5] = [1;2;3;4;5])

  let rev_filter_map f lst = fold_left (fun st x -> match f x with
      | Some v -> v :: st
      | None -> st) [] lst

  (** mapMaybe of Haskell *)
  let filter_map f lst = rev @@ rev_filter_map f lst

  (** Tail recursive verions *)

  let (@) xs ys = rev_append (rev xs) ys

  let rev_concat xs =
    let rec f acc = function
      | [] -> acc
      | x::xs -> f (rev_append x acc) xs
    in
    f [] xs

  let concat xs = rev @@ rev_concat xs

  let map f xs =
    let rec loop acc = function
      | [] -> rev acc
      | x::xs -> loop (f x :: acc) xs
    in
    loop [] xs

  let map2 f xs ys =
    let rec loop acc xs ys = match xs, ys with
      | [], [] -> rev acc
      | x::xs, y::ys -> loop (f x y :: acc) xs ys
      | _ -> raise (Invalid_argument "List.map2")
    in
    loop [] xs ys

  let combine xs ys =
    let rec loop acc xs ys = match xs, ys with
      | [], [] -> rev acc
      | x::xs, y::ys -> loop ((x, y) :: acc) xs ys
      | _ -> raise (Invalid_argument "List.map2")
    in
    loop [] xs ys

  let mapi f xs =
    let rec loop i acc = function
      | [] -> rev acc
      | x::xs -> loop (i+1) (f i x :: acc) xs
    in
    loop 0 [] xs

  let rev_concat_map f xs =
    let rec loop acc = function
      | [] -> acc
      | x::xs -> loop (rev_append (f x) acc) xs
    in
    loop [] xs

  let concat_map f xs = rev @@ rev_concat_map f xs

  let rec is_prefix xs ys = match xs, ys with
    | [], _ -> Some ys
    | x::xs, y::ys when x = y -> is_prefix xs ys
    | _ -> None

  let take_while_map p xs =
    let rec loop st = function
      | [] -> List.rev st, []
      | x::xs ->
          match p x with
          | None -> List.rev st, x::xs
          | Some y -> loop (y::st) xs
    in
    loop [] xs

  let rev_split xys =
    let rec f xs ys = function
      | [] -> xs, ys
      | (x,y)::xys -> f (x::xs) (y::ys) xys
    in
    f [] [] xys

  let split xys =
    let rxs, rys = rev_split xys in
    List.rev rxs, List.rev rys

  let partition_map f xs =
    let ls, rs =
      List.fold_left (fun (ls,rs) x ->
          match f x with
          | `Left x -> x::ls, rs
          | `Right x -> ls, x::rs ) ([], []) xs
    in
    List.rev ls, List.rev rs
end

module Array = struct
  include Array
  module Syntax = struct
    let (.!()) = unsafe_get
    let (.!() <-) = unsafe_set
  end
end

module MtimeSpan = struct
  let to_float_s span = Mtime.Span.to_float_ns span /. 1_000_000_000.
end

module Open = struct
  type 'a printer = 'a Format.printer

  let to_file ~file:fn s =
    let oc = open_out fn in
    output_string oc s;
    close_out oc

  let with_time f =
    let t1 = Mtime_clock.now () in
    let res = f () in
    let t2 = Mtime_clock.now () in
    res, (Mtime.span t1 t2)

  let with_time_lwt f =
    let open Lwt.Monad in
    let t1 = Mtime_clock.now () in
    let* res = f () in
    let t2 = Mtime_clock.now () in
    Lwt.return (res, (Mtime.span t1 t2))

  let (^/) = Filename.concat

  let (@) = List.(@)

  (* Obj hack.  Only for development *)
  let reachable_words t = Obj.reachable_words (Obj.repr t)

  let reachable_mbs t =
    float (Obj.reachable_words (Obj.repr t) * (Sys.word_size / 8))
    /. 1024. /. 1024.

  let min = min
  let max = max

  module Int = struct
    include Int
    let min (x:int) (y:int) = if x <= y then x else y
    let max (x:int) (y:int) = if x >= y then x else y
  end
end

include Open
*)
