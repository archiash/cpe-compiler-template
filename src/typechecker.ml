open Utils
open Ast

(* TODO: define your own types *)
type ty = Int | Bool | Char | Double | Unit | List of ty | Pair of ty * ty | Function of ty * ty

let rec string_of_type(t: ty) : string =
  match t with
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Double -> "double"
  | Unit -> "()"
  | List a -> "[" ^ (string_of_type a) ^ "]"
  | Pair (a, b) -> "(" ^ string_of_type a ^ ", " ^ string_of_type b ^ ")"
  | Function (a, b) -> string_of_type a ^ " -> " ^ string_of_type b

(* TODO: define your own type checking *)
let rec typecheck_prog(p : unit prog) : ty prog option =
  match p with
  | IntLit r -> Some (IntLit {r with prop = Int})
  | BoolLit r -> Some (BoolLit {r with prop = Bool})
  | BinExpr ({op = Add; _} as r)
  | BinExpr ({op = Sub; _} as r)
  | BinExpr ({op = Mult; _} as r) -> (
    (* at initial child type is unknown*)
    (* so we typecheck it to know result which maybe None or Some type*)
    (* this case is Some Int so we replace it with Int instead*)
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Int, Int) -> Some (BinExpr {r with
            prop = Int;
            left = left';
            right = right';
          })
        | (Double, Double) -> Some (BinExpr {r with
            prop = Double;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected int operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )
  | BinExpr ({op = Mod; _} as r) -> (
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Int, Int) -> Some (BinExpr {r with
            prop = Int;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected int operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )
  | BinExpr ({op = Div; _} as r) -> (
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Int, Int) -> Some (BinExpr {r with
            prop = Double;
            left = left';
            right = right';
          })
        | (Double, Double) -> Some (BinExpr {r with
            prop = Double;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected int operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )
  | BinExpr ({op = IDiv; _} as r) -> (
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Int, Int) -> Some (BinExpr {r with
            prop = Int;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected int operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )
  | BinExpr ({op = LOr; _} as r)
  | BinExpr ({op = LAnd; _} as r) -> (
    match (typecheck_prog r.left, typecheck_prog r.right) with
    | (Some left', Some right') -> (
        match (prop_of_prog left', prop_of_prog right') with
        | (Bool, Bool) -> Some (BinExpr {r with
            prop = Bool;
            left = left';
            right = right';
          })
        | (t1, t2) -> prerr_string
            (string_of_positions r.pos 
              ^ ": expected bool operands, but found "
              ^ string_of_type t1 ^ ", "
              ^ string_of_type t2 ^ "\n");
          None
    )
    | _ -> None
  )

let typecheck(p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
