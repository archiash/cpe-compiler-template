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

let type_of_unop (op : unop) (operand_ty : ty) : (ty, string) result =
  match op, operand_ty with
  | Neg, Int -> Ok Int
  | Neg, Double -> Ok Double
  | Neg, _ -> Error "int"

let rec typecheck_prog (p : unit prog) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit { r with prop = Int })
  | BoolLit r -> Some (BoolLit { r with prop = Bool })
  | DoubleLit r -> Some (DoubleLit { r with prop = Double })
  | CharLit r -> Some (CharLit { r with prop = Char })
  | UnitLit r -> Some (UnitLit { r with prop = Unit })
  | BinExpr r -> (
      match typecheck_prog r.left, typecheck_prog r.right with
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
      match typecheck_prog r.operand with
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

let typecheck (p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false