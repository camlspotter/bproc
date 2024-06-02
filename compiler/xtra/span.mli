include module type of Mtime.Span

val div : t -> float -> t

module Op : sig
  val ( + ) : t -> t -> t

  val ( / ) : t -> float -> t
end
