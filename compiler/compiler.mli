open Xtra

module Type_ : sig
  type t = Int
  [@@deriving yojson]
end

module Map : Map.S with type key = string

module State : sig
  type t = Lang.Value.t Map.t

  val pp : t Format.printer
end

module Resource : sig
  type decl = string * Type_.t list
  [@@deriving yojson]

  type pattern = string * Lang.Patt.t option
  [@@deriving yojson]

  type expression = string * Lang.Expr.t option
  [@@deriving yojson]
end

module Trans : sig
  type t =
    { input : Resource.pattern list;
      when_ : Lang.Expr.t option;
      output : Resource.expression list;
    } [@@deriving yojson]

  val apply : State.t -> Lang.Date.t -> t -> State.t option
end

module Rule : sig
  type t = string * Trans.t
  [@@deriving yojson]
end

module Process : sig
  type t =
    { resources : Resource.decl list;
      rules : Rule.t list;
    } [@@deriving yojson]

  val pp : t Format.printer
end

val load : string -> Process.t

val applicables : State.t -> Lang.Date.t -> Rule.t list -> Rule.t list
