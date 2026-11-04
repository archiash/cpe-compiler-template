open Utils
open Ast

(* ========================================================================= *)
(* 1. Monadic Option & Error Helpers                                         *)
(* ========================================================================= *)

let ( let* ) = Option.bind

let error_at (pos : pos) (msg : string) : 'a option =
  prerr_string (string_of_positions pos ^ ": " ^ msg ^ "\n");
  None

(* ========================================================================= *)
(* 2. Type Definitions & String Representation                               *)
(* ========================================================================= *)

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

(* ========================================================================= *)
(* 3. Type Compatibility & Operator Typing                                   *)
(* ========================================================================= *)

let rec types_match (t1 : ty) (t2 : ty) : ty option =
  match t1, t2 with
  | Any, t | t, Any -> Some t
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
  | _ when t1 = t2 -> Some t1
  | _ -> None

let type_of_binop (op : op) (left_ty : ty) (right_ty : ty) : (ty, string) result =
  match op, left_ty, right_ty with
  (* Arithmetic operators *)
  | (Add | Sub | Mult), Int, Int -> Ok Int
  | (Add | Sub | Mult), (Float | Int), (Float | Int) -> Ok Float
  | (Add | Sub | Mult), Any, t | (Add | Sub | Mult), t, Any -> Ok t
  | Div, (Float | Int), (Float | Int) -> Ok Float
  | Div, (Float | Int | Any), (Float | Int | Any) -> Ok Float
  | IntDiv, (Float | Int), Int -> Ok Int
  | IntDiv, (Float | Int | Any), (Int | Any) -> Ok Int
  | Mod, (Int | Any), (Int | Any) -> Ok Int
  | Mod, Float, (Int | Any) -> Ok Float
  | (Add | Sub | Mult | Div | IntDiv | Mod), _, _ -> Error "int"

  (* Boolean & comparison operators *)
  | (LOr | LAnd), Bool, Bool -> Ok Bool
  | (Lt | Leq | Gt | Geq), Int, Int -> Ok Bool
  | (Lt | Leq | Gt | Geq), Float, Float -> Ok Bool
  | (Eq | Neq), t1, t2 -> (
      match types_match t1 t2 with
      | Some _ -> Ok Bool
      | None -> Error "matching types"
    )
  | (LOr | LAnd | Lt | Leq | Gt | Geq), _, _ -> Error "bool"

  (* List & string operators *)
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
  | Concat, elem_ty, List list_elem_ty -> (
      match types_match elem_ty list_elem_ty with
      | Some res -> Ok (List res)
      | None -> Error ("list of " ^ string_of_type elem_ty)
    )
  | Concat, _, _ -> Error "string"  

let type_of_unop (op : unop) (operand_ty : ty) : (ty, string) result =
  match op, operand_ty with
  | Neg, Int -> Ok Int
  | Neg, Float -> Ok Float
  | Neg, _ -> Error "int"

(* ========================================================================= *)
(* 4. AST Helpers & Environment                                              *)
(* ========================================================================= *)

let pos_of_prog (p : 't prog) : pos =
  match p with
  | IntLit r -> r.pos
  | BoolLit r -> r.pos
  | FloatLit r -> r.pos
  | CharLit r -> r.pos
  | UnitLit r -> r.pos
  | BinExpr r -> r.pos
  | UnaryExpr r -> r.pos
  | StringLit r -> r.pos
  | Var r -> r.pos
  | If r -> r.pos
  | Let r -> r.pos
  | EmptyList r -> r.pos
  | App r -> r.pos
  | Match r -> r.pos
  | DefFunc r -> r.pos
  | Prog r -> r.pos

let rec cast_typ (t : 'a typ) : 'b typ =
  match t with
  | TyBase s -> TyBase s
  | TyArrow (t1, t2) -> TyArrow (cast_typ t1, cast_typ t2)
  | TyList t1 -> TyList (cast_typ t1)
  | TyVar s -> TyVar s

let cast_args (args : (string * 'a typ option) list) : (string * 'b typ option) list =
  List.map (fun (name, opt_t) -> (name, Option.map cast_typ opt_t)) args

let cast_ret (ret : 'a typ option) : 'b typ option =
  Option.map cast_typ ret

let has_duplicates (lst : string list) : bool =
  let rec check seen = function
    | [] -> false
    | x :: xs -> if List.mem x seen then true else check (x :: seen) xs
  in
  check [] lst

let fn_placeholder_type (arg_names : string list) : ty =
  match arg_names with
  | [] -> Function (Unit, Any)
  | _ -> List.fold_right (fun _ acc -> Function (Any, acc)) arg_names Any

let validate_func_header (pos : pos) (name : string) (args : string list) (seen_names : string list) : bool =
  if List.mem name seen_names then
    (error_at pos ("duplicate function definition '" ^ name ^ "'") |> ignore; false)
  else if has_duplicates args then
    (error_at pos ("duplicate formal parameter name in function '" ^ name ^ "'") |> ignore; false)
  else
    true

type env = (string * ty) list

let initial_env : env = [
  ("error", Function (String, Any));
  ("ord", Function (Char, Int));
  ("chr", Function (Int, Char));
]

(* ========================================================================= *)
(* 5. Pattern Typechecking                                                   *)
(* ========================================================================= *)

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
  | PatEmptyList r ->
      check_lit_pat r.pos expected_ty (List Any) "empty list pattern []"
        (fun prop -> PatEmptyList { prop; pos = r.pos })
  | PatCons r -> (
      let elem_and_list_ty =
        match expected_ty with
        | List elem_ty -> Some (elem_ty, expected_ty)
        | Any -> Some (Any, List Any)
        | _ -> None
      in
      match elem_and_list_ty with
      | Some (elem_ty, list_ty) ->
          let* (head_pat, env_h) = typecheck_pat r.head elem_ty in
          let* (tail_pat, env_t) = typecheck_pat r.tail list_ty in
          Some (PatCons { prop = list_ty; pos = r.pos; head = head_pat; tail = tail_pat }, env_h @ env_t)
      | None ->
          error_at r.pos ("expected pattern of type " ^ string_of_type expected_ty ^ ", but found cons pattern (::)")
    )

(* ========================================================================= *)
(* 6. Expression & Program Typechecking (The Core Engine)                    *)
(* ========================================================================= *)

let decl_of_prog (p : 't prog) : 't decl =
  match p with
  | DefFunc r -> DefFunc { prop = r.prop; pos = r.pos; name = r.name; args = r.args; body = r.body }
  | e -> Expr e

let prog_of_decl (d : 't decl) : 't prog =
  match d with
  | DefFunc r -> DefFunc { prop = r.prop; pos = r.pos; name = r.name; args = r.args; body = r.body }
  | Expr e -> e

let rec collect_decl_funcs (cur_env : env) (seen : string list) (decls : unit decl list) : env option =
  match decls with
  | [] -> Some cur_env
  | d :: ds -> (
      match d with
      | DefFunc fr ->
          if not (validate_func_header fr.pos fr.name fr.args seen) then
            None
          else
            let placeholder = fn_placeholder_type fr.args in
            collect_decl_funcs ((fr.name, placeholder) :: cur_env) (fr.name :: seen) ds
      | Expr _ -> collect_decl_funcs cur_env seen ds
    )

let rec typecheck_with_env (env : env) (p : unit prog) : ty prog option =
  match p with
  (* Literals *)
  | IntLit r -> Some (IntLit { r with prop = Int })
  | BoolLit r -> Some (BoolLit { r with prop = Bool })
  | FloatLit r -> Some (FloatLit { r with prop = Float })
  | CharLit r -> Some (CharLit { r with prop = Char })
  | UnitLit r -> Some (UnitLit { r with prop = Unit })
  | StringLit r -> Some (StringLit { r with prop = String })
  | EmptyList r -> Some (EmptyList { r with prop = List Any })

  (* Variable lookup *)
  | Var r -> (
      match List.assoc_opt r.name env with
      | Some ty -> Some (Var { r with prop = ty })
      | None -> error_at r.pos ("'" ^ r.name ^ "' is not defined")
    )

  (* Binary expressions *)
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

  (* Unary expressions *)
  | UnaryExpr r ->
      let* operand_prog = typecheck_with_env env r.operand in
      let operand_ty = prop_of_prog operand_prog in
      (match type_of_unop r.op operand_ty with
       | Ok result_ty ->
           Some (UnaryExpr { r with prop = result_ty; operand = operand_prog })
       | Error expected ->
           error_at r.pos
             ("expected " ^ expected ^ " operand, but found " ^ string_of_type operand_ty))

  (* Conditional: if c then e1 else e2 *)
  | If r ->
      let* cond_prog = typecheck_with_env env r.condition in
      let cond_ty = prop_of_prog cond_prog in
      let* _ =
        match types_match cond_ty Bool with
        | Some _ -> Some ()
        | None ->
            error_at (pos_of_prog cond_prog)
              ("expected bool condition, but found " ^ string_of_type cond_ty)
      in
      let* cons_prog = typecheck_with_env env r.consequent in
      let* alt_prog = typecheck_with_env env r.alternative in
      let cons_ty = prop_of_prog cons_prog in
      let alt_ty = prop_of_prog alt_prog in
      (match types_match cons_ty alt_ty with
       | Some resolved_ty ->
           Some (If { r with prop = resolved_ty; condition = cond_prog;
                             consequent = cons_prog; alternative = alt_prog })
       | None ->
           error_at r.pos
             ("expected branches to have same type, but found "
              ^ string_of_type cons_ty ^ ", " ^ string_of_type alt_ty))

  (* Local binding: let x = e1 in e2 *)
  | Let r ->
      let* val_prog = typecheck_with_env env r.value in
      let val_ty = prop_of_prog val_prog in
      let env' = (r.name, val_ty) :: env in
      let* body_prog = typecheck_with_env env' r.body in
      Some (Let { r with prop = prop_of_prog body_prog; value = val_prog; body = body_prog })

  (* Function application: f x *)
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
                  ("expected argument of type " ^ string_of_type param_ty
                   ^ ", but found " ^ string_of_type arg_ty))
       | Any ->
           Some (App { r with prop = Any; func = func_prog; arg = arg_prog })
       | _ ->
           error_at r.pos
             ("expected function type, but found " ^ string_of_type func_ty))

  (* Pattern matching: match e with | p1 -> e1 | ... *)
  | Match r ->
      let* expr_prog = typecheck_with_env env r.expr in
      let expr_ty = prop_of_prog expr_prog in
      let rec check_cases (cases : (unit pat * unit prog) list)
          : ((ty pat * ty prog) list * ty) option =
        match cases with
        | [] -> error_at r.pos "empty match cases"
        | [(pat, body)] ->
            let* (pat_prog, pat_env) = typecheck_pat pat expr_ty in
            let* body_prog = typecheck_with_env (pat_env @ env) body in
            Some ([(pat_prog, body_prog)], prop_of_prog body_prog)
        | (pat, body) :: rest ->
            let* (pat_prog, pat_env) = typecheck_pat pat expr_ty in
            let* body_prog = typecheck_with_env (pat_env @ env) body in
            let body_ty = prop_of_prog body_prog in
            let* (rest_typed, rest_ty) = check_cases rest in
            (match types_match body_ty rest_ty with
             | Some common_ty ->
                 Some ((pat_prog, body_prog) :: rest_typed, common_ty)
             | None ->
                 error_at (pos_of_prog body_prog)
                   ("match cases have incompatible types: "
                    ^ string_of_type body_ty ^ " and " ^ string_of_type rest_ty))
      in
      let* (typed_cases, result_ty) = check_cases r.cases in
      Some (Match { r with prop = result_ty; expr = expr_prog; cases = typed_cases })

  (* Single function definition: def f args = body *)
  | (DefFunc r : unit prog) ->
      let arg_names = List.map fst r.args in
      if not (validate_func_header r.pos r.name arg_names []) then None
      else
        let arg_bindings = List.map (fun a -> (a, Any)) arg_names in
        let self_placeholder = fn_placeholder_type arg_names in
        let body_env = (r.name, self_placeholder) :: (arg_bindings @ env) in
        let* typed_body = typecheck_with_env body_env r.body in
        let ret_ty = prop_of_prog typed_body in
        let fn_ty =
          match arg_names with
          | [] -> Function (Unit, ret_ty)
          | _ -> List.fold_right (fun _ acc -> Function (Any, acc)) arg_names ret_ty
        in
        let (typed_fn : ty prog) = DefFunc {
          prop = fn_ty;
          pos = r.pos;
          name = r.name;
          args = cast_args r.args;
          return_type = cast_ret r.return_type;
          body = typed_body;
        } in
        Some typed_fn

  (* Program (sequence of declarations / expressions in a file) - Two-Pass *)
  | Prog r ->
      (* Pass 1: Collect placeholders for all top-level functions *)
      let rec pass1 cur_env seen items =
        match items with
        | [] -> Some cur_env
        | (DefFunc fr : unit prog) :: ds ->
            let arg_names = List.map fst fr.args in
            if not (validate_func_header fr.pos fr.name arg_names seen) then None
            else
              let placeholder = fn_placeholder_type arg_names in
              pass1 ((fr.name, placeholder) :: cur_env) (fr.name :: seen) ds
        | _ :: ds -> pass1 cur_env seen ds
      in
      (* Pass 2: Typecheck bodies & expressions with the populated environment *)
      let rec pass2 cur_env items acc =
        match items with
        | [] -> Some (List.rev acc)
        | (DefFunc fr : unit prog) :: ds ->
            let arg_names = List.map fst fr.args in
            let arg_bindings = List.map (fun a -> (a, Any)) arg_names in
            let self_placeholder = fn_placeholder_type arg_names in
            let body_env = (fr.name, self_placeholder) :: (arg_bindings @ cur_env) in
            let* typed_body = typecheck_with_env body_env fr.body in
            let ret_ty = prop_of_prog typed_body in
            let fn_ty =
              match arg_names with
              | [] -> Function (Unit, ret_ty)
              | _ -> List.fold_right (fun _ acc -> Function (Any, acc)) arg_names ret_ty
            in
            let (typed_fr : ty prog) = DefFunc {
              prop = fn_ty;
              pos = fr.pos;
              name = fr.name;
              args = cast_args fr.args;
              return_type = cast_ret fr.return_type;
              body = typed_body;
            } in
            let next_env = (fr.name, fn_ty) :: cur_env in
            pass2 next_env ds (typed_fr :: acc)
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

(* ========================================================================= *)
(* 7. Top-Level Entry Points                                                 *)
(* ========================================================================= *)

let typecheck_prog (p : unit prog) : ty prog option =
  typecheck_with_env initial_env p

let typecheck (p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false

(* Legacy / backwards-compatible entry points *)
let typecheck_decl_program (_ : unit decl) : bool = true
let typecheck_decls_program (_ : unit decl list) : bool = true
