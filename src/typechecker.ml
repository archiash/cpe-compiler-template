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
  | Div, (Float | Int), (Float | Int) -> Ok Float
  | IntDiv, (Float | Int), Int -> Ok Int
  | Mod, Int, Int -> Ok Int
  | Mod, Float, Int -> Ok Float
  | (Add | Sub | Mult | Div | IntDiv | Mod), _, _ -> Error "int"

  | (LOr | LAnd | Eq | Neq), Bool, Bool -> Ok Bool
  | (Eq | Neq | Lt | Leq | Gt | Geq), Int, Int -> Ok Bool
  | (Eq | Neq | Lt | Leq | Gt | Geq), Float, Float -> Ok Bool
  | (Eq | Neq), List l1, List l2 -> (
      match types_match l1 l2 with
      | Some _ -> Ok Bool
      | None -> Error "matching list"
    )
  | (LOr | LAnd | Eq | Neq | Lt | Leq | Gt | Geq), _, _ -> Error "bool"
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
  | Match _ -> None

let typecheck_prog (p : unit prog) : ty prog option =
  typecheck_with_env [] p

let typecheck (p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
