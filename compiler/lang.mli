(* Mini language *)

open Xtra

val errorf :
  ?loc:Warnings.loc ->
  ?sub:Location.msg list ->
  ('a, Format.formatter, unit, 'b) format4 -> 'a

module Date : sig
  type t

  val of_string : loc:Location.t -> string -> t
end

module Value : sig
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Date of Date.t
  [@@deriving yojson]

  val of_ocaml : Parsetree.expression -> t
  val to_ocaml : t -> Parsetree.expression
  val pp : t Format.printer
end

module Patt : sig
  type t =
    | Var of string
    | Const of Value.t
  [@@deriving yojson]

  val of_ocaml : Parsetree.pattern -> t
  val to_ocaml : t -> Parsetree.pattern
  val pp : t Format.printer
end

module Expr : sig

  type t =
    | Var of string
    | Const of Value.t
    | App of t * t list
    | If of t * t * t
  [@@deriving yojson]

  val of_ocaml : Parsetree.expression -> t
  val to_ocaml : t -> Parsetree.expression
  val pp : t Format.printer

  val eval : (string * Value.t) list -> t -> Value.t
end

module List : sig
  val pattern_of_ocaml :
    (Parsetree.pattern -> 'a) -> Parsetree.pattern -> 'a list

  val expression_of_ocaml :
    (Parsetree.expression -> 'a) -> Parsetree.expression -> 'a list

  val ocaml_of_expression :
    ('a -> Parsetree.expression) -> 'a list -> Parsetree.expression

  val ocaml_of_pattern :
    ('a -> Parsetree.pattern) -> 'a list -> Parsetree.pattern
end
