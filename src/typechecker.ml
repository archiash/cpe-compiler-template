open Utils
open Ast

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
  | StringConcat, String, String -> Ok String
  | StringConcat, List l1, List l2 -> (
      match types_match l1 l2 with
      | Some res -> Ok (List res)
      | None -> Error "matching list"
    )
  | StringConcat, elem_ty, List list_elem_ty -> (
      match types_match elem_ty list_elem_ty with
      | Some res -> Ok (List res)
      | None -> Error ("list of " ^ string_of_type elem_ty)
    )
  | _ -> Error "404"

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
  
let pos_of_pat (p : 't pat) : pos =
  match p with
  | PatWildcard r -> r.pos
  | PatVar r -> r.pos
  | PatInt r -> r.pos
  | PatBool r -> r.pos
  | PatEmptyList r -> r.pos
  | PatCons r -> r.pos

type env = (string * ty) list

let rec typecheck_pat (p : unit pat) (expected_ty : ty) : (ty pat * env) option =
  match p with
  | PatWildcard r ->
      Some (PatWildcard { prop = expected_ty; pos = r.pos }, [])
  | PatVar r ->
      Some (PatVar { prop = expected_ty; pos = r.pos; name = r.name }, [(r.name, expected_ty)])
  | PatInt r -> (
      match types_match Int expected_ty with
      | Some _ -> Some (PatInt { prop = Int; pos = r.pos; value = r.value }, [])
      | None ->
          prerr_string
            (string_of_positions r.pos
            ^ ": expected pattern of type " ^ string_of_type expected_ty
            ^ ", but found int pattern\n");
          None
    )
  | PatBool r -> (
      match types_match Bool expected_ty with
      | Some _ -> Some (PatBool { prop = Bool; pos = r.pos; value = r.value }, [])
      | None ->
          prerr_string
            (string_of_positions r.pos
            ^ ": expected pattern of type " ^ string_of_type expected_ty
            ^ ", but found bool pattern\n");
          None
    )
  | PatEmptyList r -> (
      match types_match (List Any) expected_ty with
      | Some matched_list_ty ->
          Some (PatEmptyList { prop = matched_list_ty; pos = r.pos }, [])
      | None ->
          prerr_string
            (string_of_positions r.pos
            ^ ": expected pattern of type " ^ string_of_type expected_ty
            ^ ", but found empty list pattern []\n");
          None
    )
  | PatCons r -> (
      match expected_ty with
      | List elem_ty -> (
          match typecheck_pat r.head elem_ty, typecheck_pat r.tail (List elem_ty) with
          | Some (head_pat, env_h), Some (tail_pat, env_t) ->
              Some (PatCons { prop = expected_ty; pos = r.pos; head = head_pat; tail = tail_pat }, env_h @ env_t)
          | _ -> None
        )
      | Any -> (
          match typecheck_pat r.head Any, typecheck_pat r.tail (List Any) with
          | Some (head_pat, env_h), Some (tail_pat, env_t) ->
              Some (PatCons { prop = List Any; pos = r.pos; head = head_pat; tail = tail_pat }, env_h @ env_t)
          | _ -> None
        )
      | other ->
          prerr_string
            (string_of_positions r.pos
            ^ ": expected pattern of type " ^ string_of_type other
            ^ ", but found cons pattern (::)\n");
          None
    )

let rec typecheck_with_env (env : env) (p : unit prog) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit { r with prop = Int })
  | BoolLit r -> Some (BoolLit { r with prop = Bool })
  | FloatLit r -> Some (FloatLit { r with prop = Float })
  | CharLit r -> Some (CharLit { r with prop = Char })
  | UnitLit r -> Some (UnitLit { r with prop = Unit })
  | BinExpr r -> (
      match typecheck_with_env env r.left, typecheck_with_env env r.right with
      | Some left_prog, Some right_prog -> (
          let left_ty = prop_of_prog left_prog in
          let right_ty = prop_of_prog right_prog in
          match type_of_binop r.op left_ty right_ty with
          | Ok result_ty ->
              Some (BinExpr { r with prop = result_ty; left = left_prog; right = right_prog })
          | Error expected ->
              prerr_string
                (string_of_positions r.pos
                ^ ": expected " ^ expected ^ " operands, but found "
                ^ string_of_type left_ty ^ ", "
                ^ string_of_type right_ty ^ "\n");
              None
        )
      | _ -> None
    )
  | UnaryExpr r -> (
      match typecheck_with_env env r.operand with
      | Some operand_prog -> (
          let operand_ty = prop_of_prog operand_prog in
          match type_of_unop r.op operand_ty with
          | Ok result_ty ->
              Some (UnaryExpr { r with prop = result_ty; operand = operand_prog })
          | Error expected ->
              prerr_string
                (string_of_positions r.pos
                ^ ": expected " ^ expected ^ " operand, but found "
                ^ string_of_type operand_ty ^ "\n");
              None
        )
      | None -> None
    )
  | Var r -> (
      match List.assoc_opt r.name env with
      | Some ty -> Some (Var { r with prop = ty })
      | None ->
          prerr_string
            (string_of_positions r.pos
            ^ ": '" ^ r.name ^ "' is not defined\n");
          None
    )
  | If r -> (
      match typecheck_with_env env r.condition with
      | Some cond_prog -> (
          let cond_ty = prop_of_prog cond_prog in
          if cond_ty <> Bool then (
            prerr_string
              (string_of_positions (pos_of_prog cond_prog)
              ^ ": expected bool condition, but found "
              ^ string_of_type cond_ty ^ "\n");
            None
          ) else (
            match typecheck_with_env env r.consequent, typecheck_with_env env r.alternative with
            | Some cons_prog, Some alt_prog ->
                let cons_ty = prop_of_prog cons_prog in
                let alt_ty = prop_of_prog alt_prog in
                (match types_match cons_ty alt_ty with
                 | Some resolved_ty ->
                     Some (If { r with prop = resolved_ty; condition = cond_prog; consequent = cons_prog; alternative = alt_prog })
                 | None ->
                     prerr_string
                       (string_of_positions r.pos
                       ^ ": expected branches to have same type, but found "
                       ^ string_of_type cons_ty ^ ", "
                       ^ string_of_type alt_ty ^ "\n");
                     None)
            | _ -> None
          )
        )
      | None -> None
    )
  | Let r -> (
      match typecheck_with_env env r.value with
      | Some val_prog ->
          let val_ty = prop_of_prog val_prog in
          let env' = (r.name, val_ty) :: env in
          (match typecheck_with_env env' r.body with
           | Some body_prog ->
               Some (Let { r with prop = prop_of_prog body_prog; value = val_prog; body = body_prog })
           | None -> None)
      | None -> None
    )
  | StringLit r -> Some (StringLit {r with prop = String})
  | EmptyList r -> Some (EmptyList {r with prop = List Any})
  | App r -> (
      match typecheck_with_env env r.func, typecheck_with_env env r.arg with
      | Some func_prog, Some arg_prog -> (
          let func_ty = prop_of_prog func_prog in
          let arg_ty = prop_of_prog arg_prog in
          match func_ty with
          | Function (param_ty, ret_ty) -> (
              match types_match param_ty arg_ty with
              | Some _ ->
                  Some (App { r with prop = ret_ty; func = func_prog; arg = arg_prog })
              | None ->
                  prerr_string
                    (string_of_positions r.pos
                    ^ ": expected argument of type " ^ string_of_type param_ty
                    ^ ", but found " ^ string_of_type arg_ty ^ "\n");
                  None
            )
          | _ ->
              prerr_string
                (string_of_positions r.pos
                ^ ": expected function type, but found "
                ^ string_of_type func_ty ^ "\n");
              None
        )
      | _ -> None
    )
  | Match r -> (
      match typecheck_with_env env r.expr with
      | None -> None
      | Some expr_prog -> (
          let expr_ty = prop_of_prog expr_prog in
          let rec check_cases (cases : (unit pat * unit prog) list) : ((ty pat * ty prog) list * ty) option =
            match cases with
            | [] ->
                prerr_string (string_of_positions r.pos ^ ": empty match cases\n");
                None
            | [(pat, body)] -> (
                match typecheck_pat pat expr_ty with
                | None -> None
                | Some (pat_prog, pat_env) -> (
                    let case_env = pat_env @ env in
                    match typecheck_with_env case_env body with
                    | None -> None
                    | Some body_prog ->
                        Some ([(pat_prog, body_prog)], prop_of_prog body_prog)
                  )
              )
            | (pat, body) :: rest -> (
                match typecheck_pat pat expr_ty with
                | None -> None
                | Some (pat_prog, pat_env) -> (
                    let case_env = pat_env @ env in
                    match typecheck_with_env case_env body with
                    | None -> None
                    | Some body_prog -> (
                        let body_ty = prop_of_prog body_prog in
                        match check_cases rest with
                        | None -> None
                        | Some (rest_typed, rest_ty) -> (
                            match types_match body_ty rest_ty with
                            | Some common_ty ->
                                Some ((pat_prog, body_prog) :: rest_typed, common_ty)
                            | None ->
                                prerr_string
                                  (string_of_positions (pos_of_prog body_prog)
                                  ^ ": match cases have incompatible types: "
                                  ^ string_of_type body_ty ^ " and "
                                  ^ string_of_type rest_ty ^ "\n");
                                None
                          )
                      )
                  )
              )
          in
          match check_cases r.cases with
          | Some (typed_cases, result_ty) ->
              Some (Match { r with prop = result_ty; expr = expr_prog; cases = typed_cases })
          | None -> None
        )
    )
  | _ -> failwith "unimplemented"


let is_lowercase_ident (s : string) : bool =
  String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z'

let has_duplicates (lst : string list) : bool =
  let rec check seen = function
    | [] -> false
    | x :: xs -> if List.mem x seen then true else check (x :: seen) xs
  in
  check [] lst

let prop_of_decl (decl : 't decl) : 't =
  match decl with
  | DefFunc r -> r.prop
  | Expr e -> prop_of_prog e

let pos_of_decl (decl : 't decl) : pos =
  match decl with
  | DefFunc r -> r.pos
  | Expr e -> pos_of_prog e

let initial_env : env = [
  ("true", Bool);
  ("false", Bool);
  ("error", Function (String, Any));
  ("ord", Function (Char, Int));
  ("chr", Function (Int, Char));
]

let typecheck_decl (env : env) (d : unit decl) : (ty decl * env) option =
  match d with
  | Expr e -> (
      match typecheck_with_env env e with
      | Some typed_e -> Some (Expr typed_e, env)
      | None -> None
    )
  | DefFunc r ->
      if not (is_lowercase_ident r.name) then (
        prerr_string
          (string_of_positions r.pos
          ^ ": function name '" ^ r.name ^ "' must start with a lowercase letter\n");
        None
      ) else if has_duplicates r.args then (
        prerr_string
          (string_of_positions r.pos
          ^ ": duplicate formal parameter name in function '" ^ r.name ^ "'\n");
        None
      ) else if List.exists (fun a -> not (is_lowercase_ident a)) r.args then (
        prerr_string
          (string_of_positions r.pos
          ^ ": parameter names in function '" ^ r.name ^ "' must start with a lowercase letter\n");
        None
      ) else (
        let arg_bindings = List.map (fun a -> (a, Any)) r.args in
        let self_placeholder =
          match r.args with
          | [] -> Function (Unit, Any)
          | _ -> List.fold_right (fun _ acc -> Function (Any, acc)) r.args Any
        in
        let body_env = (r.name, self_placeholder) :: (arg_bindings @ env) in
        match typecheck_with_env body_env r.body with
        | Some typed_body ->
            let ret_ty = prop_of_prog typed_body in
            let fn_ty =
              match r.args with
              | [] -> Function (Unit, ret_ty)
              | _ -> List.fold_right (fun _ acc -> Function (Any, acc)) r.args ret_ty
            in
            let new_env = (r.name, fn_ty) :: env in
            Some (DefFunc { r with prop = fn_ty; body = typed_body }, new_env)
        | None -> None
      )

let rec typecheck_decls (env : env) (decls : unit decl list) : (ty decl list * env) option =
  match decls with
  | [] -> Some ([], env)
  | d :: ds -> (
      match typecheck_decl env d with
      | Some (typed_d, env') -> (
          match typecheck_decls env' ds with
          | Some (typed_ds, final_env) -> Some (typed_d :: typed_ds, final_env)
          | None -> None
        )
      | None -> None
    )

let typecheck_prog (p : unit prog) : ty prog option =
  typecheck_with_env initial_env p

let typecheck (p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false

let typecheck_decl_program (d : unit decl) : bool =
  match typecheck_decl initial_env d with
  | Some _ -> true
  | None -> false

let typecheck_decls_program (decls : unit decl list) : bool =
  match typecheck_decls initial_env decls with
  | Some _ -> true
  | None -> false
