open Ppx_yojson_conv_lib
include Yojson_conv

exception Of_yojson_error = Of_yojson_error

exception Yojson_of_error of exn * Yojson.Safe.t

let () =
  Exn.register_printer @@ function
  | Of_yojson_error (exn, j) (* defined in Yojson_conv *) ->
      Some
        (fun ppf ->
          Format.fprintf
            ppf
            "@[<2>Of_yojson_error:@ %a,@ @[%a@]@]"
            Exn.pp
            exn
            (Yojson.Safe.pretty_print ~std:true)
            j)
  | Yojson_of_error (exn, j) ->
      Some
        (fun ppf ->
          Format.fprintf
            ppf
            "@[<2>Yojson_of_error:@ %a,@ @[%a@]@]"
            Exn.pp
            exn
            (Yojson.Safe.pretty_print ~std:true)
            j)
  | _ -> None

type 'a enum = 'a

let enum_of_yojson (a_of_yojson : Yojson.Safe.t -> 'a) j =
  a_of_yojson (`List [ j ])

let yojson_of_enum yojson_of_a j =
  match yojson_of_a j with
  | `List [ j ] -> j
  | j -> raise (Yojson_of_error (Failure "yojson_of_enum: not an enum", j))
