include Format

type 'a printer = formatter -> 'a -> unit

let string = pp_print_string

let rec list (sep : (unit, formatter, unit) format) p ppf = function
  | [] -> ()
  | [ x ] -> p ppf x
  | x :: xs ->
      fprintf ppf "@[%a@]%t%a" p x (fun ppf -> fprintf ppf sep) (list sep p) xs

let option p ppf = function
  | None -> fprintf ppf "None"
  | Some x -> fprintf ppf "Some (%a)" p x
