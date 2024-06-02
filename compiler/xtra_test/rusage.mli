type t = {
  utime : int64;
  stime : int64;
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
(** Check getrusage manual page for the details of the fields *)

type flag = SELF | CHILDREN

val get : flag -> t

(** [get flag] returns information describing the resources utilized.

    If [flag = SELF], it returns the resource information of the current process.

    If [flag = CHILDREN], it returns the resource information of all the terminated child
    processes of the current process.

    The funciton may raise [Unix_error _] if [getrusage()] system call fails.
*)

type stat =
  { maxrss_gib : float;
    minflt : int;
    majflt : int
  } [@@deriving yojson_of]

val stat : unit -> stat
