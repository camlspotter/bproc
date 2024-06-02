(* very simple expression *)

open Xtra
open Yojson_conv

let errorf = Location.raise_errorf

(* (_ e1) e2 => _ e1 e2
   _ @@ e => _ e
*)
let rec fix_apply e =
  let open Parsetree in
  match e.pexp_desc with
  | Pexp_apply ({ pexp_desc= Pexp_apply (f, args); _ }, args') ->
      fix_apply { e with pexp_desc= Pexp_apply (f, args @ args') }
  | Pexp_apply ({ pexp_desc= Pexp_ident {txt= Lident "@@"; _ }; _ },
                [Nolabel, f; Nolabel, e]) ->
      fix_apply { e with pexp_desc= Pexp_apply (f, [Nolabel, e]) }
  | _ -> e

let lid ?(loc=Location.none) txt = Asttypes.{ txt; loc }

module Date = struct
  type t = Timedesc.Date.t

  let yojson_of_t t = `String (Timedesc.Date.Ymd.to_iso8601 t)

  let t_of_yojson j =
    let error () = of_yojson_error "Date: ISO8601 string expected" j in
    match j with
    | `String s ->
        (match Timedesc.Date.Ymd.of_iso8601 s with
         | Error _ -> error ()
         | Ok t -> t)
    | _ -> error ()

  open Parsetree
  open Ast_helper
  open Longident

  let of_string ~loc s =
    match Timedesc.Date.Ymd.of_iso8601 s with
    | Error _ -> errorf ~loc "Invalid ISO8601 string"
    | Ok t -> t

  let to_string = Timedesc.Date.Ymd.to_iso8601

  let of_ocaml e =
    match e.pexp_desc with
    | Pexp_construct ({txt= Lident "Date"; _},
                      Some { pexp_desc= Pexp_constant (Pconst_string (s, loc, _)); _ }) ->
        of_string ~loc s
    | _ -> errorf ~loc:e.pexp_loc "date expected"

  let to_ocaml t =
    let loc = Location.none in
    let s = Timedesc.Date.Ymd.to_iso8601 t in
    Exp.(construct
           (lid (Lident "Date"))
           (Some (constant (Pconst_string (s, loc, None)))))
end

module Value = struct
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Date of Date.t
  [@@deriving yojson]

  open Parsetree
  open Ast_helper
  open Asttypes

  let of_ocaml e : t =
    let open Parsetree in
    match (fix_apply e).pexp_desc with
    | Pexp_constant (Pconst_integer (i, None)) -> Int (int_of_string i)
    | Pexp_constant (Pconst_string (s, _, _)) -> String s
    | Pexp_construct ({ txt= Lident "true"; _ }, None) -> Bool true
    | Pexp_construct ({ txt= Lident "false"; _ }, None) -> Bool false
    | Pexp_construct ({ txt= Lident "Date"; _ }, _) -> Date (Date.of_ocaml e)
    | _ -> errorf ~loc:e.pexp_loc "Unsupported constant"

  let to_ocaml e =
    let loc = Location.none in
    match e with
    | Int i -> Exp.constant ~loc (Pconst_integer (string_of_int i, None))
    | String s -> Exp.constant ~loc (Pconst_string (s, loc, None))
    | Bool b -> Exp.construct ~loc (lid (Longident.Lident (string_of_bool b))) None
    | Date d -> Date.to_ocaml d

  let pp ppf c = Pprintast.expression ppf @@ to_ocaml c
end

module Patt = struct
  type t =
    | Var of string
    | Const of Value.t
  [@@deriving yojson]

  open Parsetree
  open Ast_helper
  open Asttypes
  open Longident

  let of_ocaml p =
    match p.ppat_desc with
    | Ppat_var { txt; _ } -> Var txt
    | Ppat_constant (Pconst_integer (i, None)) -> Const (Int (int_of_string i))
    | Ppat_constant (Pconst_string (s, _, _)) -> Const (String s)
    | Ppat_construct ({ txt= Lident "true"; _ }, None) -> Const (Bool true)
    | Ppat_construct ({ txt= Lident "false"; _ }, None) -> Const (Bool false)
    | Ppat_construct ({ txt= Lident "Date"; _ },
                      Some ([], {ppat_desc= Ppat_constant (Pconst_string (s, loc, _)); _})) ->
        Const (Date (Date.of_string ~loc s))
    | _ -> errorf ~loc:p.ppat_loc "Variable or integer constant expected"

  let to_ocaml p =
    let loc = Location.none in
    let lid txt = { txt; loc } in
    match p with
    | Var v ->
        Pat.var ~loc { txt=v; loc }
    | Const (Int i) ->
        Pat.constant ~loc (Pconst_integer (string_of_int i, None))
    | Const (String s) ->
        Pat.constant ~loc (Pconst_string (s, loc, None))
    | Const (Bool b) ->
        Pat.construct ~loc (lid (Longident.Lident (string_of_bool b))) None
    | Const (Date d) ->
        Pat.construct ~loc (lid (Longident.Lident "Date"))
          (Some ([], Pat.constant (Pconst_string (Date.to_string d, loc, None))))

  let pp ppf p = Pprintast.pattern ppf @@ to_ocaml p
end

module Expr = struct
  type t =
    | Var of string
    | Const of Value.t
    | App of t * t list
    | If of t * t * t
  [@@deriving yojson]

  open Parsetree
  open Ast_helper
  open Asttypes
  open Longident

  let of_ocaml e =
    let rec f e =
      match (fix_apply e).pexp_desc with
      | Pexp_ident { txt= Lident v; _ } -> Var v
      | (Pexp_constant _ | Pexp_construct _) ->
          Const (Value.of_ocaml e)
      | Pexp_apply (func, args) ->
          let func = f func in
          let args =
            List.map (function
                | (Asttypes.Nolabel, e) -> f e
                | _ -> errorf ~loc:e.pexp_loc "No labels allowed") args
          in
          App (func, args)
      | Pexp_ifthenelse (b, t, Some e) ->
          let b = f b in
          let t = f t in
          let e = f e in
          If (b, t, e)
      | Pexp_ifthenelse (_, _, None) ->
          errorf ~loc:e.pexp_loc "Else is required"
      | _ -> errorf ~loc:e.pexp_loc "Unsupported expression"
    in
    f e

  let to_ocaml e =
    let loc = Location.none in
    let lid txt = { txt; loc } in
    let rec f e =
      match e with
      | Var v -> Exp.ident ~loc (lid (Lident v))
      | Const v -> Value.to_ocaml v
      | App (func, es) ->
          Exp.apply ~loc (f func) (List.map (fun e -> Nolabel, f e) es)
      | If (b, t, e) ->
          Exp.ifthenelse ~loc (f b) (f t) (Some (f e))
    in
    f e

  let pp ppf e = Pprintast.expression ppf @@ to_ocaml e

  let rec eval env e : Value.t =
    match e with
    | Var v -> List.assoc v env
    | Const v -> v
    | App (Var "+", [e1; e2]) ->
        (match eval env e1, eval env e2 with
         | Int v1, Int v2 -> Int (v1 + v2)
         | _ -> invalid_arg "eval")
    | App (Var "-", [e1; e2]) ->
        (match eval env e1, eval env e2 with
         | Int v1, Int v2 -> Int (v1 - v2)
         | _ -> invalid_arg "eval")
    | App (Var "-", [e]) ->
        (match eval env e with
         | Int v -> Int (-v)
         | _ -> invalid_arg "eval")
    | App (Var "=", [e1; e2]) ->
        Bool (eval env e1 = eval env e2)
    | App (Var "<>", [e1; e2]) ->
        Bool (eval env e1 <> eval env e2)
    | App (Var "<", [e1; e2]) ->
        Bool (eval env e1 < eval env e2)
    | App (Var ">", [e1; e2]) ->
        Bool (eval env e1 > eval env e2)
    | App (Var "<=", [e1; e2]) ->
        Bool (eval env e1 <= eval env e2)
    | App (Var ">=", [e1; e2]) ->
        Bool (eval env e1 >= eval env e2)
    | App _ -> invalid_arg "eval"
    | If (b, t, e) ->
        (match eval env b with
         | Bool true -> eval env t
         | Bool false -> eval env e
         | _ -> invalid_arg "eval")
end

module List = struct

  open Longident
  open Parsetree

  let pattern_of_ocaml f pat =
    let rec loop f pat =
      match pat.ppat_desc with
      | Ppat_construct ({txt= Lident "[]"; _}, None) -> []
      | Ppat_construct ({txt= Lident "::"; _}, Some ([], p))  ->
          (match p.ppat_desc with
           | Ppat_tuple [p1; p2] -> f p1 :: loop f p2
           | _ -> assert false)
      | _ -> errorf ~loc:pat.ppat_loc "List pattern expected"
    in
    loop f pat

  let expression_of_ocaml f expr =
    let rec loop f expr =
      match expr.pexp_desc with
      | Pexp_construct ({txt= Lident "[]"; _}, None) -> []
      | Pexp_construct ({txt= Lident "::"; _}, Some expr) ->
          (match expr.pexp_desc with
           | Pexp_tuple [e1; e2] -> f e1 :: loop f e2
           | _ -> assert false)
      | _ -> errorf ~loc:expr.pexp_loc "List expected"
    in
    loop f expr

  open Ast_helper
  open Asttypes

  let lid txt = { txt; loc= Location.none }

  let rec ocaml_of_expression f = function
    | [] -> Exp.construct (lid (Lident "[]")) None
    | x::xs ->
        Exp.construct
          (lid (Lident "::"))
          (Some (Exp.tuple [ f x; ocaml_of_expression f xs ]))

  let rec ocaml_of_pattern f = function
    | [] -> Pat.construct (lid (Lident "[]")) None
    | x::xs ->
        Pat.construct
          (lid (Lident "::"))
          (Some ([], Pat.tuple [ f x; ocaml_of_pattern f xs ]))
end
