include Mtime.Span

let div span d = Option.get @@ of_float_ns @@ (to_float_ns span /. d)

module Op = struct
  let ( + ) = add

  let ( / ) = div
end
