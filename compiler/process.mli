type type_ = [ `Int ]
[@@deriving yojson]
(** Type of resource arguments *)

module Map : Map.S with type key = string

module State : sig
  type t = Lang.Value.t Map.t
end

module Trans : sig
  type resource_pattern = string * Lang.Patt.t option
  [@@deriving yojson]

  type resource_expression = string * Lang.Expr.t option
  [@@deriving yojson]

  type t =
    { input : resource_pattern list;
      when_ : Lang.Expr.t option;
      output : resource_expression list;
    } [@@deriving yojson]

  val of_ocaml : Parsetree.expression -> t
  val apply : State.t -> t -> State.t option
end

type rule = string * Trans.t
[@@deriving yojson]

type process =
  { resources : (string * type_ list) list;
    rules : rule list;
  } [@@deriving yojson]

val load : string -> process

val applicables : State.t -> rule list -> rule list
(** List the rules applicable to the state *)
