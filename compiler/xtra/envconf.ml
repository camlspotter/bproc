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

type 'a var = Raw of 'a ref | With_default of 'a * 'a option var

type ty = String | Int | Bool | Flag | Oneof of string list | Custom

let string_of_ty = function
  | String -> "string"
  | Int -> "int"
  | Bool -> "bool"
  | Flag -> "flag"
  | Oneof ks -> String.concat "|" ks
  | Custom -> "custom"

let groups = ref []

let find_group g = List.assoc_opt g !groups

let list () =
  let groups = List.sort (fun (g1, _) (g2, _) -> compare g1 g2) !groups in
  List.map
    (fun (g, rxs) -> (g, List.sort (fun (n1, _) (n2, _) -> compare n1 n2) !rxs))
    groups

let print_list ppf () =
  let groups = list () in
  Format.fprintf ppf "@[<v>";
  List.iter
    (fun (g, vs) ->
      List.iter
        (fun (v, (ty, _binder)) ->
          Format.fprintf ppf "%s_%s: %s@," g v (string_of_ty ty))
        vs)
    groups;
  Format.fprintf ppf "@]"

let parse_var var =
  match String.index var '_' with
  | exception _ -> None
  | n ->
      let group = String.sub var 0 n in
      let var = String.sub var (n + 1) (String.length var - n - 1) in
      Some (group, var)

let bind_var var value =
  match parse_var var with
  | None -> ()
  | Some (group, var) -> (
      match List.assoc_opt group !groups with
      | None -> ()
      | Some vars -> (
          match List.assoc_opt var !vars with
          | None ->
              List.iter (fun (n, vs) ->
                  List.iter
                    (fun (v, (ty, _)) ->
                      Format.eprintf "%s_%s : %s@." n v (string_of_ty ty))
                    vs)
              @@ list ();
              failwith
                (Printf.sprintf
                   "Unknown var %s_%s is bound in the environment"
                   group
                   var)
          | Some (_ty, binder) -> binder value))

let bound = ref false

let bind () =
  if !bound then invalid_arg "Envconf.bind: called twice";
  bound := true;
  Array.iter
    (fun s ->
      let var, value =
        match String.index s '=' with
        | exception _ -> (s, "")
        | n -> (String.sub s 0 n, String.sub s (n + 1) (String.length s - n - 1))
      in
      bind_var var value)
    (Unix.environment ())

let register gn ty binder =
  match parse_var gn with
  | None -> invalid_arg "Envconf: variable must have the form GROUP_VAR"
  | Some (g, n) ->
      let rxs =
        match find_group g with
        | None ->
            let rxs = ref [] in
            groups := (g, rxs) :: !groups;
            rxs
        | Some rxs -> rxs
      in
      if List.mem_assoc n !rxs then
        invalid_arg
          (Printf.sprintf "Envconf.register: %s_%s already registered" g n)
      else rxs := (n, (ty, binder)) :: !rxs

let string gn =
  let v = ref None in
  register gn String (fun s -> v := Some s);
  Raw v

let int gn =
  let v = ref None in
  register gn Int (fun s -> v := Some (int_of_string s));
  Raw v

let bool gn =
  let v = ref None in
  register gn Bool (fun s -> v := Some (bool_of_string s));
  Raw v

let flag gn =
  let v = ref false in
  register gn Flag (fun _ -> v := true);
  Raw v

let one_of gn kvs =
  let v = ref None in
  let ks = List.map fst kvs in
  register gn (Oneof ks) (fun s ->
      match List.assoc_opt s kvs with
      | None ->
          invalid_arg
            (Printf.sprintf
               "Env var %s can only take one of: %s"
               gn
               (String.concat "|" ks))
      | Some v' -> v := Some v');
  Raw v

let custom gn f =
  let v = ref None in
  register gn Custom (fun s -> v := Some (f s));
  Raw v

let with_default default v = With_default (default, v)

let rec get : 'a. 'a var -> 'a =
 fun var ->
  if not !bound then bind ();
  match var with
  | Raw ref -> !ref
  | With_default (default, v) -> Option.value ~default (get v)

let get_with_default default var = get @@ with_default default var
