let with_time f =
  let counter = Mtime_clock.counter () in
  let res = f () in
  let span = Mtime_clock.count counter in
  (res, span)

(*
let with_time_lwt f =
  let open Lwt.Syntax in
  let counter = Mtime_clock.counter () in
  let+ res = f () in
  let span = Mtime_clock.count counter in
  (res, span)
*)
