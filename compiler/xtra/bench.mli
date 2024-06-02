val with_time : (unit -> 'a) -> 'a * Mtime.span

(*
val with_time_lwt : (unit -> 'a Lwt.t) -> ('a * Mtime.span) Lwt.t
*)
