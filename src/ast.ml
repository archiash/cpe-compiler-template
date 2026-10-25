type pos = Lexing.position * Lexing.position

type 't prog =
  | IntLit of {
      prop : 't;
      pos : pos;
      value : int;
    }
  | BoolLit of {
      prop : 't;
      pos : pos;
      value : bool;
    }
  | DoubleLit of {
      prop : 't;
      pos : pos;
      value : float;
    }
  | CharLit of {
      prop : 't;
      pos : pos;
      value : char;
    }
  | UnitLit of {
      prop : 't;
      pos : pos;
  }
  | BinExpr of {
      prop : 't;
      pos : pos;
      op : op;
      left : 't prog;
      right : 't prog;
    }
  | UnaryExpr of {
      prop : 't;
      pos : pos;
      op : unop;
      operand : 't prog;
    }
  | StringLit of {
      prop: 't;
      pos: pos;
      value: string;
  }
  | Var of {
      prop: 't;
      pos: pos;
      name: string;
  }
  | If of {
      prop: 't;
      pos: pos;
      condition: 't prog;
      consequent: 't prog;
      alternative: 't prog;
  }
  | Let of {
      prop: 't;
      pos: pos;
      name: string;
      value: 't prog;
      body: 't prog;
  }
and op =
  | Add
  | Sub
  | Mult
  | Div
  | IntDiv
  | Mod
  | LOr
  | LAnd
  | Neq
  | Eq
  | Geq
  | Leq
  | Lt
  | Gt
  | StringConcat
and unop =
  | Neg

let prop_of_prog (prog : 't prog) : 't =
  match prog with
  | IntLit r -> r.prop
  | BoolLit r -> r.prop
  | DoubleLit r -> r.prop
  | CharLit r -> r.prop
  | UnitLit r -> r.prop
  | BinExpr r -> r.prop
  | UnaryExpr r -> r.prop
  | StringLit r -> r.prop
  | Var r -> r.prop
  | If r -> r.prop
  | Let r -> r.prop
let rec string_of_prog prog : string =
  match prog with
  | IntLit r -> "(" ^ string_of_int r.value ^ ")"
  | BoolLit r -> "(" ^ string_of_bool r.value ^ ")"
  | DoubleLit r -> "(" ^ string_of_float r.value ^ ")"
  | CharLit r -> "('" ^ Char.escaped r.value ^ "')"
  | UnitLit _ -> "(())"
  | BinExpr r ->
      "("
      ^ String.concat " " [string_of_op r.op; string_of_prog r.left; string_of_prog r.right]
      ^ ")"
  | UnaryExpr r -> "(" ^ string_of_unop r.op ^ string_of_prog r.operand ^ ")"
  | StringLit r -> "\"" ^ r.value ^ "\""
  | Var r -> r.name
  | If r -> "(if " ^ string_of_prog r.consequent ^ " then " ^ string_of_prog r.consequent ^ " else " ^ string_of_prog r.alternative ^ ")"
  | Let r -> "(let " ^ r.name ^ " = " ^ string_of_prog r.value ^ " in " ^ string_of_prog r.body ^ ")"

and string_of_op op : string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | IntDiv -> "//"
  | Mod -> "%"
  | LOr -> "||"
  | LAnd -> "&&"
  | Neq -> "!="
  | Eq -> "=="
  | Leq -> "<="
  | Geq -> ">="
  | Lt -> "<"
  | Gt -> ">"
  | StringConcat -> "++"

and string_of_unop op : string =
  match op with
  | Neg -> "-"