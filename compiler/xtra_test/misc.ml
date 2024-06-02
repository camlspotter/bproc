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

open Xtra
module RS = Random.State

(* XXX This is very fragile.  If the test returns () by mistake
   we cannot detect any change... *)
let regression_by_hash ?(runs = 1) name expected f =
  (* obtained from my /dev/random *)
  let seeds =
    [
      0x50cd_fcd1;
      0x56fa_beca;
      0xb081_e470;
      0xe651_73cf;
      0x70c9_53a7;
      0x3ac8_ccee;
      0xc673_4e77;
      0x5db4_eb6f;
      0x4cd2_6b25;
      0xe945_b5a3;
      0x8f97_0cc4;
      0xc5e9_76dd;
      0x08d9_ba4b;
      0xde74_9736;
      0xe790_4f5b;
      0x73ed_77a8;
      0x1118_fddc;
      0xfb4a_1250;
      0xfe3d_ea3c;
      0x29a1_0c31;
      0x3c3a_0b48;
      0x5376_162c;
      0x8fb2_a415;
      0xe577_4017;
      0x0fac_0a10;
      0x86ba_9471;
      0x860f_3af5;
      0xc927_2d1f;
      0x0666_b2ed;
      0x6f1f_fdca;
      0x40f3_9ef2;
      0xd76f_377c;
      0xe6b0_d960;
      0x2709_430a;
      0x62c7_50ad;
      0xf5cf_d9f6;
      0x661c_ca90;
      0xbbe7_3d85;
      0x8447_2791;
      0xb8ba_e6a9;
    ]
  in
  let runs = if runs <= 0 then 1 else Int.min (List.length seeds) runs in
  let seeds = List.take runs seeds in
  let get_actual () =
    Hashtbl.hash
    @@ List.map
         (fun seed ->
           let rng = Random.State.make [| seed |] in
           f rng)
         seeds
  in
  let actual = get_actual () in
  ignore
    (* Exclude functional value in the test result.
       Unfortunately Lwt.t does not checked by this. *)
    (try actual = actual
     with Invalid_argument _ -> Exn.failwithf "The value is functional!");
  if actual <> expected then
    (* We run it again and if it changes, something is unstable. *)
    (* We cannot detect the case when [f] resturns [_ Lwt.t]; it is STABLE in 1 run, but unstable in several runs *)
    let actual' = get_actual () in
    if actual <> actual' then
      Exn.failwithf
        "%s: Regression error: result is unstable! %d and %d"
        name
        actual
        actual'
    else Exn.failwithf "%s: Regression error: obtained %d" name actual

let ok_or_fail = function Ok x -> x | Error s -> raise s

let must_fail = function Ok _ -> failwith "must fail" | Error _ -> ()

let must_raise f =
  match f () with
  | exception _ -> ()
  | _ -> failwith "must raise"
