open Utils
open Ast

type ty =
  | Int
  | Bool
  | Char
  | Double
  | Unit
  | List of ty
  | Pair of ty * ty
  | Function of ty * ty

let rec string_of_type (t : ty) : string =
  match t with
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Double -> "double"
  | Unit -> "()"
  | List a -> "[" ^ string_of_type a ^ "]"
  | Pair (a, b) -> "(" ^ string_of_type a ^ ", " ^ string_of_type b ^ ")"
  | Function (a, b) -> string_of_type a ^ " -> " ^ string_of_type b

let type_of_binop (op : op) (left_ty : ty) (right_ty : ty) : (ty, string) result =
  match op, left_ty, right_ty with
  | (Add | Sub | Mult), Int, Int -> Ok Int
  | (Add | Sub | Mult), (Double | Int), (Double | Int) -> Ok Double
  | Div, (Double | Int), (Double | Int) -> Ok Double
  | IntDiv, (Double | Int), Int -> Ok Int
  | Mod, Int, Int -> Ok Int
  | Mod, Double, Int -> Ok Double
  | (Add | Sub | Mult | Div | IntDiv | Mod), _, _ -> Error "int"

  | (LOr | LAnd | Eq | Neq), Bool, Bool -> Ok Bool
  | (Eq | Neq | Lt | Leq | Gt | Geq), Int, Int -> Ok Bool
  | (Eq | Neq | Lt | Leq | Gt | Geq), Double, Double -> Ok Bool
  | (LOr | LAnd | Eq | Neq | Lt | Leq | Gt | Geq), _, _ -> Error "bool"
  | StringConcat, _, _ -> failwith "unimplemented"

let type_of_unop (op : unop) (operand_ty : ty) : (ty, string) result =
  match op, operand_ty with
  | Neg, Int -> Ok Int
  | Neg, Double -> Ok Double
  | Neg, _ -> Error "int"

let pos_of_prog (p : 't prog) : pos =
  match p with
  | IntLit r -> r.pos
  | BoolLit r -> r.pos
  | DoubleLit r -> r.pos
  | CharLit r -> r.pos
  | UnitLit r -> r.pos
  | BinExpr r -> r.pos
  | UnaryExpr r -> r.pos
  | StringLit r -> r.pos
  | Var r -> r.pos
  | If r -> r.pos
  | Let r -> r.pos

type env = (string * ty) list

let rec typecheck_with_env (env : env) (p : unit prog) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit { r with prop = Int })
  | BoolLit r -> Some (BoolLit { r with prop = Bool })
  | DoubleLit r -> Some (DoubleLit { r with prop = Double })
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
                if cons_ty = alt_ty then
                  Some (If { r with prop = cons_ty; condition = cond_prog; consequent = cons_prog; alternative = alt_prog })
                else (
                  prerr_string
                    (string_of_positions r.pos
                    ^ ": expected branches to have same type, but found "
                    ^ string_of_type cons_ty ^ ", "
                    ^ string_of_type alt_ty ^ "\n");
                  None
                )
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
  | StringLit _ -> failwith "unimplemented"

let typecheck_prog (p : unit prog) : ty prog option =
  typecheck_with_env [] p

let typecheck (p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
