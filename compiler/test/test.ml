open Bproc
open Compiler

let () =
  let proc = load "example/fred.ml" in
  Format.eprintf "@[<2>Converted:@ @[%a@]@]@." Compiler.Process.pp proc;
  let today = Lang.Date.of_string ~loc:Location.none "2024-06-01" in
  let init = Map.of_list ["Start", Lang.Value.Bool true] in
  Format.eprintf "init: %a@." State.pp init;
  let transitions =
    [ "start";
      "send_truck";
      "count_truck";
      "send_truck";
      "count_truck";
      "send_truck";
      "count_truck";
      "send_truck";
      "count_truck";
      "send_truck";
      "count_truck";
      "send_truck";
      "count_truck";
      "send_truck";
      "count_truck";
      "send_truck";
      "send_truck";
      "send_truck";
      "count_truck";
      "count_truck";
      "count_truck";
      "pay" ]
  in
  let final =
    List.fold_left (fun state n ->
        let trans = List.assoc n proc.rules in
        match Trans.apply state today trans with
        | Some state ->
            Format.eprintf "%s => %a@." n State.pp state;
            state
        | None ->
            Format.eprintf "%s => XXX@." n;
            assert false
      ) init transitions
  in
  Format.eprintf "final: %a@." State.pp final
