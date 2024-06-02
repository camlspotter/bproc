[@@@ocaml.warning "-8"]
(* Rules are expressed as non-exhausitve pattern matches *)

type rule
(* Type of transition rules *)

type resource = ..

type store = resource list

type trans = store -> store

type date = Date of string
(* ISO-8601 date string such as "2024-06-01" *)

let [@prim] today : date = assert false

let [@prim] when_ : bool -> store -> store = assert false

(* End of primitive declarations *)

(* Declaration of resources *)
type resource +=
  | Start
  | End
  | S
  | Truck of int
  | C of int

(* Rules are declared as OCaml functions:

     let name input = output

   Input:
     An input is a set of resource patterns. A resource pattern can take
     pattern variables in its arguments.

     An input is written as an OCaml list pattern. For example:

       [T1 1; T2; T3 x]

   Output:
     An output is a set of resource expressions. A resource expression can
     have variables in its arguments.

     An output is written as an OCaml list expression. For example:

       [T1 1; T2; T3 x]

   Guard:
     An output can be wrapped by a guard condition c:

       when_ c output

     When a rule is applied, the guard condition c is evaluated under
     the environment bound by the input. The rule application fails
     when the condition c is evaluated to false.
*)

let start [Start] = [S; C 0; Truck 0]

let send_truck [S; Truck i] = when_ (i < 10) [S; Truck (i + 1)]

let count_truck [Truck i; C j] = when_ (j < 10) [Truck (i-1); C (j+1)]

let pay [C 10] = when_ (today <= Date "2024-06-20") [End]
