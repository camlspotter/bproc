[@@@ocaml.warning "-8"]
(* Rules are expressed as non-exhausitve pattern matches *)

type rule
(** Type of transition rules *)

type resource = ..

type store = resource list

type trans = store -> store

type date = Date of string

let [@prim] today : date = assert false

let [@prim] trans : (store -> store) -> rule = assert false
(** Rewriting rule *)

let [@prim] when_ : bool -> store -> store = assert false

(* End of primitive declarations *)

type resource +=
  | Start
  | End
  | S
  | Truck of int
  | C of int

let start [Start] = [S; C 0; Truck 0]

let send_truck [S; Truck i] = when_ (i < 10) [S; Truck (i + 1)]

let count_truck [Truck i; C j] = when_ (j < 10) [Truck (i-1); C (j+1)]

let pay [C 10] = when_ (today <= Date "2024-06-20") [End]
