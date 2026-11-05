open Utils
open Ast

let ( let* ) = Option.bind

let error_at (pos : pos) (msg : string) : 'a option =
  prerr_string (string_of_positions pos ^ ": " ^ msg ^ "\n");
  None

type ty =
  | Int
  | Bool
  | Char
  | Float
  | Unit
  | String
  | List of ty
  | Pair of ty * ty
  | Function of ty * ty
  | Any
  | TypeVar of string
  | TypeApp of ty * ty

let rec string_of_type (t : ty) : string =
  match t with
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"
  | Unit -> "()"
  | List a -> "[" ^ string_of_type a ^ "]"
  | Pair (a, b) -> "(" ^ string_of_type a ^ ", " ^ string_of_type b ^ ")"
  | Function (a, b) -> string_of_type a ^ " -> " ^ string_of_type b
  | Any -> "any"
  | TypeVar a -> a
  | TypeApp (t1, t2) -> "(" ^ string_of_type t1 ^ " " ^ string_of_type t2 ^ ")"

let rec types_match (t1 : ty) (t2 : ty) : ty option =
  match t1, t2 with
  | Any, t | t, Any -> Some t
  | TypeVar _, t | t, TypeVar _ -> Some t
  | List a, List b -> (
      match types_match a b with
      | Some elem -> Some (List elem)
      | None -> None
    )
  | Pair (a1, b1), Pair (a2, b2) -> (
      match types_match a1 a2, types_match b1 b2 with
      | Some a, Some b -> Some (Pair (a, b))
      | _ -> None
    )
  | Function (a1, b1), Function (a2, b2) -> (
      match types_match a1 a2, types_match b1 b2 with
      | Some a, Some b -> Some (Function (a, b))
      | _ -> None
    )
  | TypeApp (a1, b1), TypeApp (a2, b2) -> (
      match types_match a1 a2, types_match b1 b2 with
      | Some a, Some b -> Some (TypeApp (a, b))
      | _ -> None
    )
  | _ when t1 = t2 -> Some t1
  | _ -> None

let type_of_binop (op : op) (left_ty : ty) (right_ty : ty) : (ty, string) result =
  match op, left_ty, right_ty with
  | (Add | Sub | Mult), Int, Int -> Ok Int
  | (Add | Sub | Mult), (Float | Int), (Float | Int) -> Ok Float
  | Div, (Float | Int), (Float | Int) -> Ok Float
  | IntDiv, (Float | Int), Int -> Ok Int
  | Mod, Int, Int -> Ok Int
  | Mod, Float, Int -> Ok Float
  | (Add | Sub | Mult | Div | IntDiv | Mod), _, _ -> Error "int"

  | (LOr | LAnd), Bool, Bool -> Ok Bool
  | (Lt | Leq | Gt | Geq), Int, Int -> Ok Bool
  | (Lt | Leq | Gt | Geq), Float, Float -> Ok Bool
  | (Eq | Neq), t1, t2 -> (
      match types_match t1 t2 with
      | Some _ -> Ok Bool
      | None -> Error "matching types"
    )
  | (LOr | LAnd | Lt | Leq | Gt | Geq), _, _ -> Error "bool"

  | Cons, elem_ty, List list_elem_ty -> (
      match types_match elem_ty list_elem_ty with
      | Some res -> Ok (List res)
      | None -> Error ("list of " ^ string_of_type elem_ty)
    )
  | Cons, elem_ty, _ -> Error ("list of " ^ string_of_type elem_ty)
  | Concat, String, String -> Ok String
  | Concat, List l1, List l2 -> (
      match types_match l1 l2 with
      | Some res -> Ok (List res)
      | None -> Error "matching list"
    )
  | Concat, _, _ -> Error "string"  

  | Compose, Function (b1, c), Function (a, b2) -> (
      match types_match b1 b2 with
      | Some _ -> Ok (Function (a, c))
      | None ->
          Error ("incompatible types for composition (.): "
                 ^ string_of_type b1 ^ " does not match " ^ string_of_type b2)
    )
  | Compose, _, _ -> Error "function composition (.) requires two functions"

  | Dollar, Function (param_ty, ret_ty), arg_ty -> (
      match types_match param_ty arg_ty with
      | Some _ -> Ok ret_ty
      | None ->
          Error ("function parameter expected " ^ string_of_type param_ty
                 ^ ", but found " ^ string_of_type arg_ty)
    )
  | Dollar, _, _ -> Error "function on left side of ($)"

let type_of_unop (op : unop) (operand_ty : ty) : (ty, string) result =
  match op, operand_ty with
  | Neg, Int -> Ok Int
  | Neg, Float -> Ok Float
  | Neg, _ -> Error "int"

let pos_of_prog (p : 't prog) : pos =
  match p with
  | IntLit r -> r.pos
  | BoolLit r -> r.pos
  | FloatLit r -> r.pos
  | CharLit r -> r.pos
  | UnitLit r -> r.pos
  | StringLit r -> r.pos
  | EmptyList r -> r.pos
  | Var r -> r.pos
  | Constructor r -> r.pos
  | Namespace r -> r.pos
  | Ast.Pair r -> r.pos
  | BinExpr r -> r.pos
  | UnaryExpr r -> r.pos
  | App r -> r.pos
  | If r -> r.pos
  | Let r -> r.pos
  | LetBindings r -> r.pos
  | Lambda r -> r.pos
  | Match r -> r.pos
  | DefFunc r -> r.pos
  | DefFuncTyped r -> r.pos
  | Import r -> r.pos
  | TypeDef r -> r.pos
  | DataDef r -> r.pos
  | Prog r -> r.pos

let rec ty_of_typ (t : 't typ) : ty =
  match t with
  | TyBase "Int" | TyBase "Integer" -> Int
  | TyBase "Bool" -> Bool
  | TyBase "Char" -> Char
  | TyBase "Double" -> Float
  | TyBase "String" -> String
  | TyBase s -> TypeVar s
  | TyVar s -> TypeVar s
  | TyUnit -> Unit
  | TyPair (t1, t2) -> Pair (ty_of_typ t1, ty_of_typ t2)
  | TyArrow (t1, t2) -> Function (ty_of_typ t1, ty_of_typ t2)
  | TyList t1 -> List (ty_of_typ t1)
  | TyApp (t1, t2) -> TypeApp (ty_of_typ t1, ty_of_typ t2)

let has_duplicates (lst : string list) : bool =
  let rec check seen = function
    | [] -> false
    | x :: xs -> if List.mem x seen then true else check (x :: seen) xs
  in
  check [] lst

let fn_signature_params (params : 't param list) (ret_opt : 't typ option) : ty =
  let ret_ty = match ret_opt with 
    | Some t -> ty_of_typ t 
    | None -> Unit 
  in match params with
  | [] -> ret_ty
  | [ParamUnit] -> Function (Unit, ret_ty)
  | _ ->
      List.fold_right
        (fun p acc ->
           let param_ty =
             match p with
             | ParamUnit -> Unit
             | ParamUntyped _ -> Any
             | ParamTyped (_, t) -> ty_of_typ t
           in
           Function (param_ty, acc))
        params ret_ty

let validate_func_header_params (pos : pos) (name : string) (params : 't param list) (ret_opt : 't typ option) (seen_names : string list) : bool =
  let desc = if params = [] then "variable" else "function" in
  let param_names =
    List.filter_map (function
      | ParamUnit -> None
      | ParamUntyped s | ParamTyped (s, _) -> Some s
    ) params
  in
  if List.mem name seen_names then
    let _ = error_at pos ("duplicate " ^ desc ^ " definition '" ^ name ^ "'") in false
  else if has_duplicates param_names then
    let _ = error_at pos ("duplicate formal parameter name in function '" ^ name ^ "'") in false
  else if List.exists (function ParamUntyped _ -> true | _ -> false) params then
    let _ = error_at pos ("missing parameter type annotation in function '" ^ name ^ "'") in false
  else if Option.is_none ret_opt then
    let _ = error_at pos ("missing return type annotation for " ^ desc ^ " '" ^ name ^ "'") in false
  else
    true

type env = (string * ty) list

let initial_env : env = [
  ("error", Function (String, TypeVar "a"));
  ("ord", Function (Char, Int));
  ("chr", Function (Int, Char));
]

let check_lit_pat (pos : pos) (expected_ty : ty) (lit_ty : ty) (desc : string) (construct : ty -> ty pat) : (ty pat * env) option =
  match types_match lit_ty expected_ty with
  | Some resolved_ty -> Some (construct resolved_ty, [])
  | None ->
      error_at pos ("expected pattern of type " ^ string_of_type expected_ty ^ ", but found " ^ desc)

let rec typecheck_pat (p : unit pat) (expected_ty : ty) : (ty pat * env) option =
  match p with
  | PatWildcard r ->
      Some (PatWildcard { prop = expected_ty; pos = r.pos }, [])
  | PatVar r ->
      Some (PatVar { prop = expected_ty; pos = r.pos; name = r.name }, [(r.name, expected_ty)])
  | PatInt r ->
      check_lit_pat r.pos expected_ty Int "int pattern"
        (fun prop -> PatInt { prop; pos = r.pos; value = r.value })
  | PatBool r ->
      check_lit_pat r.pos expected_ty Bool "bool pattern"
        (fun prop -> PatBool { prop; pos = r.pos; value = r.value })
  | PatFloat r ->
      check_lit_pat r.pos expected_ty Float "float pattern"
        (fun prop -> PatFloat { prop; pos = r.pos; value = r.value })
  | PatChar r ->
      check_lit_pat r.pos expected_ty Char "char pattern"
        (fun prop -> PatChar { prop; pos = r.pos; value = r.value })
  | PatString r ->
      check_lit_pat r.pos expected_ty String "string pattern"
        (fun prop -> PatString { prop; pos = r.pos; value = r.value })
  | PatUnit r ->
      check_lit_pat r.pos expected_ty Unit "unit pattern ()"
        (fun prop -> PatUnit { prop; pos = r.pos })
  | PatEmptyList r ->
      check_lit_pat r.pos expected_ty (List Any) "empty list pattern []"
        (fun prop -> PatEmptyList { prop; pos = r.pos })
  | PatCons r -> (
      match expected_ty with
      | List elem_ty ->
          let* (head_pat, env_h) = typecheck_pat r.head elem_ty in
          let* (tail_pat, env_t) = typecheck_pat r.tail expected_ty in
          (match List.find_opt (fun (name, _) -> List.mem_assoc name env_t) env_h with
           | Some (dup, _) ->
               error_at r.pos ("variable '" ^ dup ^ "' is bound several times in this pattern")
           | None ->
               Some (PatCons { prop = expected_ty; pos = r.pos; head = head_pat; tail = tail_pat }, env_h @ env_t))
      | _ ->
          error_at r.pos ("expected pattern of type " ^ string_of_type expected_ty ^ ", but found cons pattern (::)")
    )
  | PatPair r -> (
      match expected_ty with
      | Pair (fst_ty, snd_ty) ->
          let* (fst_pat, env_f) = typecheck_pat r.fst fst_ty in
          let* (snd_pat, env_s) = typecheck_pat r.snd snd_ty in
          (match List.find_opt (fun (name, _) -> List.mem_assoc name env_s) env_f with
           | Some (dup, _) ->
               error_at r.pos ("variable '" ^ dup ^ "' is bound several times in this pattern")
           | None ->
               Some (PatPair { prop = expected_ty; pos = r.pos; fst = fst_pat; snd = snd_pat }, env_f @ env_s))
      | _ ->
          error_at r.pos ("expected pattern of type " ^ string_of_type expected_ty ^ ", but found pair pattern")
    )
  | PatConstructor r ->
      let rec check_args arg_pats acc_pats acc_env =
        match arg_pats with
        | [] -> Some (List.rev acc_pats, acc_env)
        | p_i :: rest ->
            let* (typed_pi, env_i) = typecheck_pat p_i Any in
            (match List.find_opt (fun (name, _) -> List.mem_assoc name acc_env) env_i with
             | Some (dup, _) ->
                 error_at r.pos ("variable '" ^ dup ^ "' is bound several times in this pattern")
             | None ->
                 check_args rest (typed_pi :: acc_pats) (env_i @ acc_env))
      in
      let* (typed_args, env_args) = check_args r.args [] [] in
      Some (PatConstructor { prop = expected_ty; pos = r.pos; name = r.name; args = typed_args }, env_args)

let rec typecheck_with_env (env : env) (p : unit prog) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit { r with prop = Int })
  | BoolLit r -> Some (BoolLit { r with prop = Bool })
  | FloatLit r -> Some (FloatLit { r with prop = Float })
  | CharLit r -> Some (CharLit { r with prop = Char })
  | UnitLit r -> Some (UnitLit { r with prop = Unit })
  | StringLit r -> Some (StringLit { r with prop = String })
  | EmptyList r -> Some (EmptyList { r with prop = List Any })

  | Ast.Pair r ->
      let* fst_prog = typecheck_with_env env r.fst in
      let* snd_prog = typecheck_with_env env r.snd in
      let pair_ty = Pair (prop_of_prog fst_prog, prop_of_prog snd_prog) in
      Some (Ast.Pair { r with prop = pair_ty; fst = fst_prog; snd = snd_prog })

  | Var r -> (
      match List.assoc_opt r.name env with
      | Some ty -> Some (Var { r with prop = ty })
      | None -> error_at r.pos ("'" ^ r.name ^ "' is not defined")
    )

  | Constructor r -> (
      match List.assoc_opt r.name env with
      | Some ty -> Some (Constructor { r with prop = ty })
      | None -> Some (Constructor { r with prop = TypeVar r.name })
    )

  | Namespace r ->
      let* ns_prog = typecheck_with_env env r.ns in
      (match List.assoc_opt r.member env with
       | Some ty -> Some (Namespace { r with prop = ty; ns = ns_prog })
       | None -> Some (Namespace { r with prop = TypeVar r.member; ns = ns_prog }))

  | BinExpr r ->
      let* left_prog = typecheck_with_env env r.left in
      let* right_prog = typecheck_with_env env r.right in
      let left_ty = prop_of_prog left_prog in
      let right_ty = prop_of_prog right_prog in
      (match type_of_binop r.op left_ty right_ty with
       | Ok result_ty ->
           Some (BinExpr { r with prop = result_ty; left = left_prog; right = right_prog })
       | Error expected ->
           error_at r.pos
             ("expected " ^ expected ^ " operands, but found "
              ^ string_of_type left_ty ^ ", " ^ string_of_type right_ty))

  | UnaryExpr r ->
      let* operand_prog = typecheck_with_env env r.operand in
      let operand_ty = prop_of_prog operand_prog in
      (match type_of_unop r.op operand_ty with
       | Ok result_ty ->
           Some (UnaryExpr { r with prop = result_ty; operand = operand_prog })
       | Error expected ->
           error_at r.pos
             ("expected " ^ expected ^ " operand, but found " ^ string_of_type operand_ty))

  | If r ->
      let* cond_prog = typecheck_with_env env r.condition in
      let cond_ty = prop_of_prog cond_prog in
      let* _ =
        match cond_ty with
        | Bool | Any -> Some ()
        | _ -> error_at (pos_of_prog cond_prog)
                 ("expected bool condition, but found " ^ string_of_type cond_ty)
      in
      let* cons_prog = typecheck_with_env env r.consequent in
      let* alt_prog = typecheck_with_env env r.alternative in
      let cons_ty = prop_of_prog cons_prog in
      let alt_ty = prop_of_prog alt_prog in
      (match types_match cons_ty alt_ty with
       | Some unified_ty ->
           Some (If { r with prop = unified_ty; condition = cond_prog; consequent = cons_prog; alternative = alt_prog })
       | None ->
           error_at r.pos
             ("if branch type mismatch: consequent has type "
              ^ string_of_type cons_ty ^ ", but alternative has type "
              ^ string_of_type alt_ty))

  | Let r ->
      let* value_prog = typecheck_with_env env r.value in
      let value_ty = prop_of_prog value_prog in
      let body_env = (r.name, value_ty) :: env in
      let* body_prog = typecheck_with_env body_env r.body in
      Some (Let { r with prop = prop_of_prog body_prog; value = value_prog; body = body_prog })

  | LetBindings r ->
      let rec collect_sigs bindings acc =
        match bindings with
        | [] -> acc
        | LetFuncBind fb :: bs ->
            let fn_sig = fn_signature_params fb.params fb.return_type in
            collect_sigs bs ((fb.name, fn_sig) :: acc)
        | LetVarBind _ :: bs ->
            collect_sigs bs acc
      in
      let fn_env = collect_sigs r.bindings [] in
      let body_scope_env = fn_env @ env in
      let rec check_bindings bs acc_bindings acc_env =
        match bs with
        | [] -> Some (List.rev acc_bindings, acc_env)
        | LetVarBind vb :: rest ->
            let* typed_val = typecheck_with_env env vb.value in
            let val_ty = prop_of_prog typed_val in
            let* final_ty =
              match vb.return_type with
              | Some ret_t ->
                  let expected = ty_of_typ ret_t in
                  (match types_match val_ty expected with
                   | Some u -> Some u
                   | None ->
                       error_at (pos_of_prog vb.value)
                         ("expected type " ^ string_of_type expected ^ ", but found " ^ string_of_type val_ty))
              | None -> Some val_ty
            in
            let typed_vb = LetVarBind { vb with value = typed_val; return_type = Obj.magic vb.return_type } in
            check_bindings rest (typed_vb :: acc_bindings) ((vb.name, final_ty) :: acc_env)
        | LetFuncBind fb :: rest ->
            let param_bindings =
              List.filter_map (function
                | ParamUnit -> None
                | ParamUntyped name -> Some (name, TypeVar name)
                | ParamTyped (name, typ) -> Some (name, ty_of_typ typ)
              ) fb.params
            in
            let fb_env = param_bindings @ body_scope_env in
            let* typed_body = typecheck_with_env fb_env fb.body in
            let body_ty = prop_of_prog typed_body in
            let* _ =
              match fb.return_type with
              | Some ret_t ->
                  let expected = ty_of_typ ret_t in
                  (match types_match body_ty expected with
                   | Some u -> Some u
                   | None ->
                       error_at (pos_of_prog fb.body)
                         ("expected return type " ^ string_of_type expected ^ ", but found " ^ string_of_type body_ty))
              | None -> Some body_ty
            in
            let fn_ty = fn_signature_params fb.params fb.return_type in
            let typed_fb = LetFuncBind { fb with body = typed_body; params = Obj.magic fb.params; return_type = Obj.magic fb.return_type } in
            check_bindings rest (typed_fb :: acc_bindings) ((fb.name, fn_ty) :: acc_env)
      in
      let* (typed_bindings, new_env) = check_bindings r.bindings [] env in
      let* typed_body = typecheck_with_env (new_env @ fn_env @ env) r.body in
      Some (LetBindings { r with prop = prop_of_prog typed_body; bindings = typed_bindings; body = typed_body })

  | Lambda r ->
      let param_bindings =
        List.filter_map (function
          | ParamUnit -> None
          | ParamUntyped name -> Some (name, TypeVar name)
          | ParamTyped (name, typ) -> Some (name, ty_of_typ typ)
        ) r.params
      in
      let body_env = param_bindings @ env in
      let* typed_body = typecheck_with_env body_env r.body in
      let body_ty = prop_of_prog typed_body in
      let* final_ret_ty =
        match r.return_type with
        | Some ret_typ ->
            let expected_ret = ty_of_typ ret_typ in
            (match types_match body_ty expected_ret with
             | Some unified -> Some unified
             | None ->
                 error_at r.pos
                   ("expected lambda return type " ^ string_of_type expected_ret
                    ^ ", but body has type " ^ string_of_type body_ty))
        | None -> Some body_ty
      in
      let lam_ty =
        match r.params with
        | [] | [ParamUnit] -> Function (Unit, final_ret_ty)
        | _ ->
            List.fold_right
              (fun p acc ->
                 let p_ty =
                   match p with
                   | ParamUnit -> Unit
                   | ParamUntyped _ -> Any
                   | ParamTyped (_, t) -> ty_of_typ t
                 in
                 Function (p_ty, acc))
              r.params final_ret_ty
      in
      Some (Lambda { r with prop = lam_ty; body = typed_body; return_type = Obj.magic r.return_type; params = Obj.magic r.params })

  | App r ->
      let* func_prog = typecheck_with_env env r.func in
      let* arg_prog = typecheck_with_env env r.arg in
      let func_ty = prop_of_prog func_prog in
      let arg_ty = prop_of_prog arg_prog in
      (match func_ty with
       | Function (param_ty, ret_ty) ->
           (match types_match param_ty arg_ty with
            | Some _ ->
                Some (App { r with prop = ret_ty; func = func_prog; arg = arg_prog })
            | None ->
                error_at r.pos
                  ("function argument type mismatch: expected "
                   ^ string_of_type param_ty ^ ", but found " ^ string_of_type arg_ty))
       | _ ->
           error_at r.pos
             ("expected function type, but found " ^ string_of_type func_ty))

  | Match r ->
      let* expr_prog = typecheck_with_env env r.expr in
      let in_ty = prop_of_prog expr_prog in
      let rec check_cases = function
        | [] -> error_at r.pos "empty match cases"
        | [(p, b)] ->
            let* (pat, pat_env) = typecheck_pat p in_ty in
            let* body_prog = typecheck_with_env (pat_env @ env) b in
            Some ([(pat, body_prog)], prop_of_prog body_prog)
        | (p, b) :: rest ->
            let* (pat, pat_env) = typecheck_pat p in_ty in
            let* body_prog = typecheck_with_env (pat_env @ env) b in
            let body_ty = prop_of_prog body_prog in
            let* (rest_cases, rest_ty) = check_cases rest in
            (match types_match body_ty rest_ty with
             | Some unified_ty ->
                 Some ((pat, body_prog) :: rest_cases, unified_ty)
             | None ->
                 error_at (pos_of_prog body_prog)
                   ("match cases have incompatible types: "
                    ^ string_of_type body_ty ^ " and " ^ string_of_type rest_ty))
      in
      let* (typed_cases, result_ty) = check_cases r.cases in
      Some (Match { r with prop = result_ty; expr = expr_prog; cases = typed_cases })

  | DefFuncTyped r ->
      if not (validate_func_header_params r.pos r.name r.params r.return_type []) then None
      else if r.params = [] then
        let* typed_var, _ = check_var_params env r.name r.pos r.params r.return_type r.body in
        Some typed_var
      else
        let* typed_fn, _ = check_func_params env r.name r.pos r.params r.return_type r.body in
        Some typed_fn

  | DefFunc r ->
      let* typed_body = typecheck_with_env env r.body in
      Some (DefFunc { r with prop = prop_of_prog typed_body; body = typed_body })

  | Prog r ->
      let rec pass1 cur_env seen items =
        match items with
        | [] -> Some cur_env
        | DefFuncTyped fr :: ds ->
            if not (validate_func_header_params fr.pos fr.name fr.params fr.return_type seen) then None
            else
              let fn_sig = fn_signature_params fr.params fr.return_type in
              (match fn_sig with
               | Function _ ->
                   pass1 ((fr.name, fn_sig) :: cur_env) (fr.name :: seen) ds
               | _ ->
                   pass1 cur_env (fr.name :: seen) ds)
        | DefFunc fr :: ds ->
            let fn_sig = Function (Any, Any) in
            pass1 ((fr.name, fn_sig) :: cur_env) (fr.name :: seen) ds
        | DataDef dr :: ds ->
            let res_ty =
              match dr.type_params with
              | [] -> TypeVar dr.name
              | ps -> List.fold_left (fun acc p -> TypeApp (acc, TypeVar p)) (TypeVar dr.name) ps
            in
            let ctor_bindings =
              List.map (fun (cname, cargs) ->
                let cty = List.fold_right (fun t acc -> Function (ty_of_typ t, acc)) cargs res_ty in
                (cname, cty)
              ) dr.constructors
            in
            pass1 (ctor_bindings @ cur_env) seen ds
        | _ :: ds -> pass1 cur_env seen ds
      in
      let rec pass2 cur_env items acc =
        match items with
        | [] -> Some (List.rev acc)
        | DefFuncTyped fr :: ds ->
            let fn_sig = fn_signature_params fr.params fr.return_type in
            let* typed_fr, next_env =
              match fn_sig with
              | Function _ ->
                  check_func_params cur_env fr.name fr.pos fr.params fr.return_type fr.body
              | _ ->
                  check_var_params cur_env fr.name fr.pos fr.params fr.return_type fr.body
            in
            pass2 next_env ds (typed_fr :: acc)
        | DefFunc fr :: ds ->
            let* typed_body = typecheck_with_env cur_env fr.body in
            let typed_fr = DefFunc { fr with prop = prop_of_prog typed_body; body = typed_body } in
            pass2 cur_env ds (typed_fr :: acc)
        | DataDef dr :: ds ->
            let res_ty =
              match dr.type_params with
              | [] -> TypeVar dr.name
              | ps -> List.fold_left (fun acc p -> TypeApp (acc, TypeVar p)) (TypeVar dr.name) ps
            in
            let ctor_bindings =
              List.map (fun (cname, cargs) ->
                let cty = List.fold_right (fun t acc -> Function (ty_of_typ t, acc)) cargs res_ty in
                (cname, cty)
              ) dr.constructors
            in
            let typed_dr = DataDef { dr with prop = Unit; constructors = Obj.magic dr.constructors } in
            pass2 (ctor_bindings @ cur_env) ds (typed_dr :: acc)
        | TypeDef tr :: ds ->
            let typed_tr = TypeDef { tr with prop = Unit; typ = Obj.magic tr.typ } in
            pass2 cur_env ds (typed_tr :: acc)
        | Import ir :: ds ->
            let typed_ir = Import { ir with prop = Unit } in
            pass2 cur_env ds (typed_ir :: acc)
        | expr :: ds ->
            let* typed_expr = typecheck_with_env cur_env expr in
            pass2 cur_env ds (typed_expr :: acc)
      in
      let* pass1_env = pass1 env [] r.decls in
      let* typed_decls = pass2 pass1_env r.decls [] in
      let result_ty =
        match List.rev typed_decls with
        | [] -> Unit
        | last :: _ -> prop_of_prog last
      in
      Some (Prog { r with prop = result_ty; decls = typed_decls })

  | Import r -> Some (Import { r with prop = Unit })
  | TypeDef r -> Some (TypeDef { r with prop = Unit; typ = Obj.magic r.typ })
  | DataDef r -> Some (DataDef { r with prop = Unit; constructors = Obj.magic r.constructors })

and check_func_params (env : env) (name : string) (pos : pos)
                      (params : unit param list)
                      (return_type : unit typ option)
                      (body : unit prog) : (ty prog * env) option =
  let param_bindings =
    List.filter_map (function
      | ParamUnit -> None
      | ParamUntyped n -> Some (n, TypeVar n)
      | ParamTyped (n, t) -> Some (n, ty_of_typ t)
    ) params
  in
  let self_sig = fn_signature_params params return_type in
  let body_env = (name, self_sig) :: (param_bindings @ env) in
  let* typed_body = typecheck_with_env body_env body in
  let body_ret_ty = prop_of_prog typed_body in
  let* final_ret_ty =
    match return_type with
    | Some expected_typ ->
        let expected_ty = ty_of_typ expected_typ in
        (match types_match body_ret_ty expected_ty with
         | Some unified -> Some unified
         | None ->
             error_at pos
               ("expected function return type " ^ string_of_type expected_ty
                ^ ", but body has type " ^ string_of_type body_ret_ty))
    | None -> Some body_ret_ty
  in
  let fn_ty =
    match params with
    | [] -> final_ret_ty
    | [ParamUnit] -> Function (Unit, final_ret_ty)
    | _ ->
        List.fold_right
          (fun p acc ->
             let p_ty =
               match p with
               | ParamUnit -> Unit
               | ParamUntyped _ -> Any
               | ParamTyped (_, t) -> ty_of_typ t
             in
             Function (p_ty, acc))
          params final_ret_ty
  in
  let typed_fn = DefFuncTyped {
    prop = fn_ty;
    pos;
    name;
    params = Obj.magic params;
    return_type = Obj.magic return_type;
    body = typed_body;
  } in
  Some (typed_fn, (name, fn_ty) :: env)

and check_var_params (env : env) (name : string) (pos : pos)
                     (params : unit param list)
                     (return_type : unit typ option)
                     (body : unit prog) : (ty prog * env) option =
  let* typed_body = typecheck_with_env env body in
  let body_ty = prop_of_prog typed_body in
  let* final_ty =
    match return_type with
    | Some expected_typ ->
        let expected_ty = ty_of_typ expected_typ in
        (match types_match body_ty expected_ty with
         | Some unified -> Some unified
         | None ->
             error_at pos
               ("expected variable type " ^ string_of_type expected_ty
                ^ ", but body has type " ^ string_of_type body_ty))
    | None -> Some body_ty
  in
  let typed_var = DefFuncTyped {
    prop = final_ty;
    pos;
    name;
    params = Obj.magic params;
    return_type = Obj.magic return_type;
    body = typed_body;
  } in
  Some (typed_var, (name, final_ty) :: env)

let typecheck_prog (p : unit prog) : ty prog option =
  typecheck_with_env initial_env p

let typecheck (p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
