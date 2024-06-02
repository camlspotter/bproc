open Xtra
open Yojson_conv

type type_ = [ `Int ]
[@@deriving yojson]

let errorf = Location.raise_errorf

module Map = Map.Make(String)

module State = struct
  type t = Lang.Value.t Map.t
end

module Trans = struct
  type resource_pattern = string * Lang.Patt.t option
  [@@deriving yojson]

  type resource_expression = string * Lang.Expr.t option
  [@@deriving yojson]

  (* rewriting rule *)
  type t =
    { input : resource_pattern list;
      when_ : Lang.Expr.t option;
      output : resource_expression list
    }
  [@@deriving yojson]

  let of_ocaml e =
    let open Parsetree in
    let rec pat_list f pat =
      match pat.ppat_desc with
      | Ppat_construct ({txt= Lident "[]"; _}, None) -> []
      | Ppat_construct ({txt= Lident "::"; _}, Some ([], p))  ->
          (match p.ppat_desc with
           | Ppat_tuple [p1; p2] -> f p1 :: pat_list f p2
           | _ -> assert false)
      | _ -> errorf ~loc:pat.ppat_loc "List pattern expected"
    in
    let pat_resource pat =
      match pat.ppat_desc with
      | Ppat_construct ({txt= Lident n; _}, None) -> n, None
      | Ppat_construct ({txt= Lident n; _}, Some ([], p)) -> n, Some (Lang.Patt.of_ocaml p)
      | _ -> errorf ~loc:pat.ppat_loc "Constructor expected"
    in
    let rec expr_list f expr =
      match expr.pexp_desc with
      | Pexp_construct ({txt= Lident "[]"; _}, None) -> []
      | Pexp_construct ({txt= Lident "::"; _}, Some expr) ->
          (match expr.pexp_desc with
           | Pexp_tuple [e1; e2] -> f e1 :: expr_list f e2
           | _ -> assert false)
      | _ -> errorf ~loc:expr.pexp_loc "List expected"
    in
    let expr_resource expr =
      match expr.pexp_desc with
      | Pexp_construct ({txt= Lident n; _}, None) -> n, None
      | Pexp_construct ({txt= Lident n; _}, Some expr) -> n, Some (Lang.Expr.of_ocaml expr)
      | _ -> errorf ~loc:expr.pexp_loc "Constructor expected"
    in
    let pat_input pat = pat_list pat_resource pat in
    let expr_output expr = expr_list expr_resource expr in
    let rec when_output e =
      match e.pexp_desc with
      | Pexp_apply ({ pexp_desc= Pexp_apply (f, args); _ }, args') ->
          when_output { e with pexp_desc= Pexp_apply (f, args @ args') }
      | Pexp_apply ({ pexp_desc= Pexp_ident {txt= Lident "@@"; _ }; _ },
                    [Nolabel, f; Nolabel, e]) ->
          when_output { e with pexp_desc= Pexp_apply (f, [Nolabel, e]) }
      | Pexp_apply ({ pexp_desc= Pexp_ident {txt= Lident "when_"; _ }; _ },
                    [Nolabel, b; Nolabel, e]) ->
          let b = Lang.Expr.of_ocaml b in
          let bopt, output = when_output e in
          (match bopt with
           | None -> Some b
           | Some b' -> Some (Lang.Expr.App (Var "&&", [b; b']))), output
      | _ -> None, expr_output e
    in
    match e.pexp_desc with
    | Pexp_fun (Nolabel, None, pat, expr) ->
        let input = pat_input pat in
        let when_, output = when_output expr in
        { input; when_; output }
    | Pexp_function [case] ->
        let input = pat_list pat_resource case.pc_lhs in
        let when_ = Option.map Lang.Expr.of_ocaml case.pc_guard in
        let when_', output = when_output case.pc_rhs in
        let when_ =
          match when_, when_' with
          | Some when_, Some when_' -> Some (Lang.Expr.App (Var "&&", [when_; when_']))
          | Some when_, None -> Some when_
          | None, Some when_' -> Some when_'
          | None, None -> None
        in
        { input; when_; output }
    | Pexp_function _ -> errorf ~loc:e.pexp_loc "Multi case function is not allowed"
    | _ -> errorf ~loc:e.pexp_loc "Function expected"

  let apply (state : State.t) { input; when_; output } =
    let matches =
      List.fold_left (fun acc (v, patopt) ->
          match patopt with
          | None ->
              (match Map.find_opt v state with
               | Some (Lang.Value.Bool true) -> acc
               | None | Some _ -> raise Exit)
          | Some patt ->
              (match Map.find_opt v state with
               | None -> raise Exit
               | Some value ->
                   let match_ (patt : Lang.Patt.t) =
                     match patt with
                     | Var x -> (x, value) :: acc
                     | Const value' when value = value' -> acc
                     | _ -> invalid_arg "apply"
                   in
                   match_ patt))
        [] input
    in
    (match when_ with
     | None -> ()
     | Some when_ ->
         match Lang.Expr.eval matches when_ with
         | Bool true -> ()
         | Bool false -> raise Exit
         | _ -> assert false);
    let output =
      List.map (function
          | (n, None) ->
              n, Lang.Value.Bool true
          | (n, Some e) ->
              n, Lang.Expr.eval matches e) output
    in
    List.fold_left (fun state (x, v) -> Map.add x v state) state output

  let apply state trans =
    try
      Some (apply state trans)
    with
    | Exit -> None
end

type rule = string * Trans.t
[@@deriving yojson]

type process =
  { resources : (string * type_ list) list;
    rules : rule list
  } [@@deriving yojson]

let load path =
  let str =
    Pparse.parse_implementation
      ~tool_name:"bprocc"
      path
  in
  let open Parsetree in
  let core_type (cty : core_type) =
    match cty.ptyp_desc with
    | Ptyp_constr ({txt= Lident "int"; _}, []) -> `Int
    | _ -> errorf ~loc:cty.ptyp_loc "Unsupported type constructor"
  in
  let is_special_attr (a : attribute) = a.attr_name.txt = "prim" in

  let parse_rule_decl vb =
    let name =
      match vb.pvb_pat.ppat_desc with
      | Ppat_var s ->
          let name = s.txt in
          name
      | _ -> errorf ~loc:vb.pvb_pat.ppat_loc "No complex pattern allowed"
    in
    name, Trans.of_ocaml vb.pvb_expr
  in

  let init = { resources= []; rules= [] } in
  let res =
    List.fold_left (fun acc (sitem : structure_item) ->
        match sitem.pstr_desc with
        | Pstr_type _ ->
            (* type resource = .. hits here *)
            acc
        | Pstr_typext tyext ->
            (match tyext.ptyext_path.txt with
             | Lident "resource" -> ()
              | _ ->
                 errorf ~loc:tyext.ptyext_path.loc "Only type resource can be extended");
            (match tyext.ptyext_params with
             | [] -> ()
             | _ ->
                 errorf ~loc:tyext.ptyext_loc "No type parameters allowed");
            (match tyext.ptyext_private with
             | Private ->
                 errorf ~loc:tyext.ptyext_loc "No private type allowed";
             | Public -> ()
            );
            Format.eprintf "type resource:@.";
            let constrs =
              List.map (fun pext ->
                  Format.eprintf "pext_name: %s@." pext.pext_name.txt;
                  match pext.pext_kind with
                  | Pext_decl (_, _, Some _) ->
                      errorf ~loc:pext.pext_loc "No GADT constructor allowed"
                  | Pext_decl ([], carg, None) ->
                      (match carg with
                       | Pcstr_tuple ctys ->
                           (pext.pext_name.txt, List.map core_type ctys)
                       | Pcstr_record _ ->
                           errorf ~loc:pext.pext_loc "No record argument allowed"
                      )
                  | Pext_decl (_existentials, _, None) ->
                      errorf ~loc:pext.pext_loc "No existentials allowed"
                  | Pext_rebind _ ->
                      errorf ~loc:pext.pext_loc "No rebinding allowed")
                tyext.ptyext_constructors
            in
            { acc with resources = acc.resources @ constrs }
        | Pstr_attribute _ -> acc
        | Pstr_value (Recursive, _) ->
            errorf ~loc:sitem.pstr_loc "No recursion allowed"
        | Pstr_value (_, vbs) ->
            let rules =
              List.filter_map (fun vb ->
                  if List.exists is_special_attr vb.pvb_attributes then None
                  else Some (parse_rule_decl vb)
                ) vbs
            in
            { acc with rules = acc.rules @ rules }
        | _ ->
            Location.raise_errorf
              ~loc:sitem.pstr_loc
              "Unsupported structure item"
      ) init str
  in
  Format.eprintf "%a@." (Yojson.pp_as_json yojson_of_process) res;
  res

let load path =
  try load path with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    raise exn

let applicables state rules =
  List.filter (fun (_n, trans) ->
      match Trans.apply state trans with
      | None -> false
      | Some _ -> true) rules
