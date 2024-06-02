type t = {
  utime : int64; (* microsecs *)
  stime : int64; (* microsecs *)
  maxrss : int64;
  ixrss : int64;
  idrss : int64;
  isrss : int64;
  minflt : int64;
  majflt : int64;
  nswap : int64;
  inblock : int64;
  outblock : int64;
  msgsnd : int64;
  msgrcv : int64;
  nsignals : int64;
  nvcsw : int64;
  nivcsw : int64;
}

type flag = SELF | CHILDREN

external get : flag -> t = "caml_unix_getrusage"

open Xtra.Yojson_conv

type stat =
  { maxrss_gib : float;
    minflt : int;
    majflt : int
  } [@@deriving yojson_of]

let stat () =
  let rusage : t = get SELF in
  let maxrss_bytes =
    match (Posix_uname.uname ()).sysname with
    | "Darwin" ->
        (* Mac OS's maxrss is in bytes *)
        Int64.to_int rusage.maxrss
    | _ ->
        (* Other unicies' maxrss is in KiB *)
        Int64.to_int rusage.maxrss * 1024
  in
  { maxrss_gib = float maxrss_bytes /. 1024. /. 1024. /. 1024.;
    minflt= Int64.to_int rusage.minflt;
    majflt= Int64.to_int rusage.majflt
  }
