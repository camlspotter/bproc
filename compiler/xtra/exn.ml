module Format = Xformat
(* module Lwt = Xlwt *)

type t = exn

type printer = t -> (Format.formatter -> unit) option

let printers : printer list ref = ref []

let register_printer f = printers := f :: !printers

let () =
  register_printer @@ function
  | Failure s -> Some (fun ppf -> Format.fprintf ppf "Failure: %s" s)
  | Invalid_argument s ->
      Some (fun ppf -> Format.fprintf ppf "Invalid argument: %s" s)
  | _ -> None

let pp ppf e =
  let rec loop = function
    | [] -> Format.string ppf (Printexc.to_string e)
    | p :: ps -> ( match p e with None -> loop ps | Some f -> f ppf)
  in
  loop !printers

let to_string e = Format.asprintf "%a" pp e

let failwithf fmt = Format.kasprintf failwith fmt

let invalid_argf fmt = Format.kasprintf invalid_arg fmt

type exn += String of string

let () =
  register_printer @@ function
  | String s -> Some (fun ppf -> Format.string ppf s)
  | _ -> None

let catch f = try Ok (f ()) with exn -> Error exn

let get_ok = function Ok x -> x | Error e -> raise e

let protect f g =
  let res = catch f in
  g ();
  get_ok res

let report_error f =
  match f () with
  | exception e ->
      Format.eprintf "Error: %a@." pp e;
      raise e
  | x -> x

module Result = struct
  type 'a t = ('a, exn) Result.t

  module Monad = Monad.Make1 (struct
    type nonrec 'a t = 'a t

    let bind = Result.bind

    let map = Some Result.map

    let return x = Ok x
  end)

  let catch = catch

  let get_ok = get_ok
end

(*
module Result_lwt = struct
  type 'a t = 'a Result.t Lwt.t

  module Monad = Monad.Make1 (struct
    type nonrec 'a t = 'a t

    let bind at f =
      Lwt.bind at (function Error e -> Lwt.return_error e | Ok a -> f a)

    let map =
      Some
        (fun f at ->
          Lwt.map (function Error e -> Error e | Ok a -> Ok (f a)) at)

    let return x = Lwt.return_ok x
  end)

  let get_ok_lwt x =
    let open Lwt.Monad in
    let+ x in
    match x with Ok x -> x | Error e -> raise e

  let lift_lwt x = Lwt.map (fun x -> Ok x) x

  let catch f =
    match f () with
    | exception e -> Lwt.return_error e
    | lwt -> Lwt.catch (fun () -> Lwt.map (fun x -> Ok x) lwt) Lwt.return_error
end
*)
