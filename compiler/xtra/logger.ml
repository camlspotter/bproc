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

let parse_level = function
  | "DEBUG" -> Debug
  | "INFO" -> Info
  | "NOTICE" -> Notice
  | "WARNING" -> Warning
  | "ERROR" -> Error
  | "FATAL" -> Fatal
  | s -> Exn.invalid_argf "Invalid log level: %S" s

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

module Make (A : sig
  val env_var : string
end) =
struct
  module Default_prim : PRINTER = struct
    let log_level_var = Envconf.string A.env_var

    let parse_settings s =
      let tokens = String.split_on_char ',' s in
      let parse_path s = String.split_on_char '.' s in
      let parse_setting s =
        match String.split_on_char '=' s with
        | [ n; l ] -> (parse_path n, parse_level l)
        | [ l ] -> ([], parse_level l)
        | _ ->
            Exn.invalid_argf "Logger.parse_settings: invalid log setting %S" s
      in
      List.map parse_setting tokens

    let settings = ref None

    let get_settings () =
      match !settings with
      | Some lev -> lev
      | None ->
          let s = Envconf.get_with_default "NOTICE" log_level_var in
          let ss = parse_settings s in
          settings := Some ss;
          ss

    let rec match_path pattern path =
      match (pattern, path) with
      | [], _ -> true
      | "*" :: pattern, _ :: path -> match_path pattern path
      | s1 :: pattern, s2 :: path when s1 = s2 -> match_path pattern path
      | _ :: _, _ :: _ -> false
      | _, [] -> false

    let match_settings path =
      let rec f = function
        | [] -> Notice
        | (p, l) :: settings -> if match_path p path then l else f settings
      in
      f (get_settings ())

    let log path =
      let threshold = match_settings path in
      fun level fmt ->
        if level >= threshold then
          Format.kasprintf
            (fun s ->
              let path_name =
                match path with [] -> "nopath" | _ -> String.concat "." path
              in
              let s = path_name ^ ": " ^ s in
              prerr_endline s)
            fmt
        else
          let b = Buffer.create 0 in
          let ppf = Format.formatter_of_buffer b in
          Format.ikfprintf ignore ppf fmt
  end

  let prim = ref (module Default_prim : PRINTER)

  let override m = prim := m

  module Make (Path : sig
    val path : string list
  end) : S = struct
    let log level fmt =
      let (module Log : PRINTER) = !prim in
      Log.log Path.path level fmt

(*
    let lwt_log level fmt =
      Format.kasprintf
        (fun s ->
          log level "%s" s;
          Lwt.return_unit)
        fmt
*)

    let debug fmt = log Debug fmt

    let info fmt = log Info fmt

    let notice fmt = log Notice fmt

    let warning fmt = log Warning fmt

    let error fmt = log Error fmt

    let fatal fmt = log Fatal fmt

(*
    let lwt_debug fmt = lwt_log Debug fmt

    let lwt_info fmt = lwt_log Info fmt

    let lwt_notice fmt = lwt_log Notice fmt

    let lwt_warning fmt = lwt_log Warning fmt

    let lwt_error fmt = lwt_log Error fmt

    let lwt_fatal fmt = lwt_log Fatal fmt
*)
  end

  let make path : (module S) =
    let module Path = struct
      let path = path
    end in
    let module Log = Make (Path) in
    (module (Log : S))
end
