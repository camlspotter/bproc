open Xtra
open Yojson_conv

let errorf = Location.raise_errorf

let lid ?(loc=Location.none) txt = Asttypes.{ txt; loc }

module Type_ = struct
  (* Type_ to avoid a name collision with Ast_helper.Type *)

  type t = Int
  [@@deriving yojson]

  open Ast_helper
  open Parsetree

  let to_ocaml t =
    match t with
    | Int -> Typ.constr (lid (Longident.Lident "int")) []

  let of_ocaml (cty : core_type) =
    match cty.ptyp_desc with
    | Ptyp_constr ({txt= Lident "int"; _}, []) -> Int
    | _ -> errorf ~loc:cty.ptyp_loc "Unsupported type constructor"
end

module Map = Map.Make(String)

module State = struct
  type t = Lang.Value.t Map.t

  let pp ppf m =
    let open Format in
    fprintf ppf "[@[";
    pp_print_list
      ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
      (fun ppf (k, v) ->
         match v with
         | Lang.Value.Bool true -> fprintf ppf "@[%s@]" k
         | Lang.Value.Bool false -> ()
         | _ -> fprintf ppf "@[%s: %a@]" k Lang.Value.pp v)
      ppf (Map.to_list m);
    fprintf ppf "@]]"
end

module Resource = struct
  type decl = string * Type_.t list
  [@@deriving yojson]

  type pattern = string * Lang.Patt.t option
  [@@deriving yojson]

  type expression = string * Lang.Expr.t option
  [@@deriving yojson]

  open Parsetree

  let decls_of_ocaml tyext =
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
    List.map (fun pext ->
        match pext.pext_kind with
        | Pext_decl (_, _, Some _) ->
            errorf ~loc:pext.pext_loc "No GADT constructor allowed"
        | Pext_decl ([], carg, None) ->
            (match carg with
             | Pcstr_tuple ctys ->
                 (pext.pext_name.txt, List.map Type_.of_ocaml ctys)
             | Pcstr_record _ ->
                 errorf ~loc:pext.pext_loc "No record argument allowed"
            )
        | Pext_decl (_existentials, _, None) ->
            errorf ~loc:pext.pext_loc "No existentials allowed"
        | Pext_rebind _ ->
            errorf ~loc:pext.pext_loc "No rebinding allowed")
      tyext.ptyext_constructors

  let pattern_of_ocaml pat =
    match pat.ppat_desc with
    | Ppat_construct ({txt= Lident n; _}, None) -> n, None
    | Ppat_construct ({txt= Lident n; _}, Some ([], p)) -> n, Some (Lang.Patt.of_ocaml p)
    | _ -> errorf ~loc:pat.ppat_loc "Constructor expected"

  let expression_of_ocaml expr =
    match expr.pexp_desc with
    | Pexp_construct ({txt= Lident n; _}, None) ->
        n, None
    | Pexp_construct ({txt= Lident n; _}, Some e) ->
        n, Some (Lang.Expr.of_ocaml e)
    | _ -> errorf ~loc:expr.pexp_loc "Constructor expected"

  open Ast_helper
  open Longident

  let ocaml_of_decls (decls : decl list) =
    let loc = Location.none in
    let ecs =
      List.map (fun (n, tys) ->
          Te.constructor { txt=n; loc }
            (let c_args =
               Pcstr_tuple (List.map Type_.to_ocaml tys)
             in
             (Pext_decl ([], c_args, None)))) decls
    in
    Te.mk (lid (Lident "resource")) ecs

  let ocaml_of_pattern = function
    | (n, None) -> Pat.construct (lid (Lident n)) None
    | (n, Some p) ->
        Pat.construct (lid (Lident n)) (Some ([], Lang.Patt.to_ocaml p))

  let ocaml_of_expression = function
    | (n, None) -> Exp.construct (lid (Lident n)) None
    | (n, Some p) ->
        Exp.construct (lid (Lident n)) (Some (Lang.Expr.to_ocaml p))
end

module Trans = struct
  (* rewriting rule *)
  type t =
    { input : Resource.pattern list;
      when_ : Lang.Expr.t option;
      output : Resource.expression list
    }
  [@@deriving yojson]

  open Ast_helper
  open Longident

  let to_ocaml { input; when_; output } =
    let input = Lang.List.ocaml_of_pattern Resource.ocaml_of_pattern input in
    let output = Lang.List.ocaml_of_expression Resource.ocaml_of_expression output in
    let when_ = Option.map Lang.Expr.to_ocaml when_ in
    let output =
      match when_ with
      | None -> output
      | Some when_ ->
          Exp.(apply (ident (lid (Lident "when_")))
                 [ Nolabel, when_; Nolabel, output ])
    in
    Exp.fun_ Nolabel None input output

  let of_ocaml e =
    let open Parsetree in
    let pat_input pat =
      Lang.List.pattern_of_ocaml Resource.pattern_of_ocaml pat
    in
    let expr_output expr =
      Lang.List.expression_of_ocaml Resource.expression_of_ocaml expr
    in
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
        let input = pat_input case.pc_lhs in
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

  let apply (state : State.t) today { input; when_; output } =
    let matches, state =
      List.fold_left (fun (matches, state) (v, patopt) ->
          match patopt with
          | None ->
              (match Map.find_opt v state with
               | Some (Lang.Value.Bool true) ->
                   (matches, Map.remove v state)
               | None | Some _ -> raise Exit)
          | Some patt ->
              (match Map.find_opt v state with
               | None -> raise Exit
               | Some value ->
                   let match_ (patt : Lang.Patt.t) =
                     match patt with
                     | Var x ->
                         (x, value) :: matches, Map.remove v state
                     | Const value' when value = value' ->
                         matches, Map.remove v state
                     | _ -> invalid_arg "apply"
                   in
                   match_ patt))
        (["today", Lang.Value.Date today], state) input
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

  let apply state today trans =
    try
      Some (apply state today trans)
    with
    | Exit -> None
end

module Rule = struct
  type t = string * Trans.t
  [@@deriving yojson]

  open Parsetree
  open Ast_helper

  let of_ocaml vb =
    let name =
      match vb.pvb_pat.ppat_desc with
      | Ppat_var s ->
          let name = s.txt in
          name
      | _ -> errorf ~loc:vb.pvb_pat.ppat_loc "No complex pattern allowed"
    in
    name, Trans.of_ocaml vb.pvb_expr

  let to_ocaml (n, trans) =
    let loc = Location.none in
    Vb.mk (Pat.var { txt=n; loc }) (Trans.to_ocaml trans)
end

module Process = struct
  type t =
    { resources : Resource.decl list;
      rules : Rule.t list
    } [@@deriving yojson]

  open Parsetree

  let of_ocaml str =
    let is_special_attr (a : attribute) = a.attr_name.txt = "prim" in

    let init = { resources= []; rules= [] } in
    List.fold_left (fun (acc : t) (sitem : structure_item) ->
        match sitem.pstr_desc with
        | Pstr_type _ ->
            (* type resource = .. hits here *)
            acc
        | Pstr_typext tyext ->
            let decls = Resource.decls_of_ocaml tyext in
            { acc with resources = acc.resources @ decls }
        | Pstr_attribute _ -> acc
        | Pstr_value (Recursive, _) ->
            errorf ~loc:sitem.pstr_loc "No recursion allowed"
        | Pstr_value (_, vbs) ->
            let rules =
              List.filter_map (fun vb ->
                  if List.exists is_special_attr vb.pvb_attributes then None
                  else Some (Rule.of_ocaml vb)
                ) vbs
            in
            { acc with rules = acc.rules @ rules }
        | _ ->
            Location.raise_errorf
              ~loc:sitem.pstr_loc
              "Unsupported structure item"
      ) init str

  open Ast_helper

  let to_ocaml { resources; rules } : structure =
    Str.type_extension
      (Resource.ocaml_of_decls resources)
    ::
    List.map (fun rule ->
        let vb = Rule.to_ocaml rule in
        Str.value Nonrecursive [vb]) rules

  let pp ppf proc = Pprintast.structure ppf @@ to_ocaml proc
end

(* "example/fred.ml" *)
let load path =
  try
    let str =
      Pparse.parse_implementation
        ~tool_name:"bprocc"
        path
    in
    Process.of_ocaml str
  with exn ->
    Format.eprintf "%a@." Location.report_exception exn;
    raise exn

let applicables state today rules =
  List.filter (fun (_n, trans) ->
      match Trans.apply state today trans with
      | None -> false
      | Some _ -> true) rules
