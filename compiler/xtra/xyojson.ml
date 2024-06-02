exception Json_error = Yojson.Json_error

exception End_of_array = Yojson.End_of_array

exception End_of_object = Yojson.End_of_object

exception End_of_tuple = Yojson.End_of_tuple

exception End_of_input = Yojson.End_of_input

(* I only use Safe *)
include Yojson.Safe

let pp = pretty_print ~std:true

let pp_as_json yojson_of_a ppf a = pp ppf @@ yojson_of_a a

let t_of_yojson = Fun.id

let yojson_of_t = Fun.id
