type pos = Lexing.position * Lexing.position

type 't pat =
  | PatWildcard of { prop : 't; pos : pos }
  | PatVar of { prop : 't; pos : pos; name : string }
  | PatInt of { prop : 't; pos : pos; value : int }
  | PatBool of { prop : 't; pos : pos; value : bool }
  | PatEmptyList of { prop : 't; pos : pos }
  | PatCons of { prop : 't; pos : pos; head : 't pat; tail : 't pat }

type 't typ =
| TyBase of string
| TyArrow of 't typ * 't typ
| TyList of 't typ
| TyVar of string

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
  | FloatLit of {
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
  | EmptyList of {
      prop: 't;
      pos: pos;
  }
  | App of {
    prop: 't;
    pos: pos;
    func: 't prog;
    arg: 't prog;
  }
  | Match of {
      prop : 't;
      pos : pos;
      expr : 't prog;
      cases : ('t pat * 't prog) list;
    }
  | DefFunc of {
      prop : 't;
      pos : pos;
      name : string;
      args : (string * 't typ option) list;
      return_type : 't typ option;
      body : 't prog;
    }
  | Prog of {
    prop : 't;
    pos : pos;
    decls : 't prog list;
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
  | Concat
  | Cons
and unop =
  | Neg

type 't decl =
  | DefFunc of {
      prop : 't;
      pos : pos;
      name : string;
      args : string list;
      body : 't prog;
    }
  | Expr of 't prog

let prop_of_prog (prog : 't prog) : 't =
  match prog with
  | IntLit r -> r.prop
  | BoolLit r -> r.prop
  | FloatLit r -> r.prop
  | CharLit r -> r.prop
  | UnitLit r -> r.prop
  | BinExpr r -> r.prop
  | UnaryExpr r -> r.prop
  | StringLit r -> r.prop
  | Var r -> r.prop
  | If r -> r.prop
  | Let r -> r.prop
  | EmptyList r -> r.prop
  | App r -> r.prop
  | Match r -> r.prop
  | DefFunc r -> r.prop
  | Prog r -> r.prop

let rec string_of_pat (pat : 't pat) : string =
  match pat with
  | PatWildcard _ -> "_"
  | PatVar r -> r.name
  | PatInt r -> string_of_int r.value
  | PatBool r -> string_of_bool r.value
  | PatEmptyList _ -> "[]"
  | PatCons r -> "(" ^ string_of_pat r.head ^ " :: " ^ string_of_pat r.tail ^ ")"

let rec string_of_typ (t : 't typ) : string =
  match t with
  | TyBase name -> name
  | TyArrow (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TyList t_inner -> "[" ^ string_of_typ t_inner ^ "]"
  | TyVar name -> name
let rec string_of_prog prog : string =
  match prog with
  | IntLit r -> "(" ^ string_of_int r.value ^ ")"
  | BoolLit r -> "(" ^ string_of_bool r.value ^ ")"
  | FloatLit r -> "(" ^ string_of_float r.value ^ ")"
  | CharLit r -> "('" ^ Char.escaped r.value ^ "')"
  | UnitLit _ -> "(())"
  | BinExpr r ->
      "("
      ^ String.concat " " [string_of_op r.op; string_of_prog r.left; string_of_prog r.right]
      ^ ")"
  | UnaryExpr r -> "(" ^ string_of_unop r.op ^ string_of_prog r.operand ^ ")"
  | StringLit r -> "\"" ^ r.value ^ "\""
  | Var r -> r.name
  | If r -> "(if " ^ string_of_prog r.condition ^ " then " ^ string_of_prog r.consequent ^ " else " ^ string_of_prog r.alternative ^ ")"
  | Let r -> "(let " ^ r.name ^ " = " ^ string_of_prog r.value ^ " in " ^ string_of_prog r.body ^ ")"
  | EmptyList _ -> "[]"
  | App r -> "(App " ^ string_of_prog r.func ^ " " ^ string_of_prog r.arg ^ ")"
  | Match r -> let string_of_case (p, e) = "| " ^ string_of_pat p ^ " -> " ^ string_of_prog e in
      let cases_str = String.concat " " (List.map string_of_case r.cases) in
      "(match " ^ string_of_prog r.expr ^ " with " ^ cases_str ^ ")"
  | DefFunc r -> 
      let string_of_arg (name, opt_t) = 
        match opt_t with
        | None -> name
        | Some t -> "(" ^ name ^ " : " ^ string_of_typ t ^ ")"
      in
      let args_str = if r.args = [] then "" else " " ^ String.concat " " (List.map string_of_arg r.args) in
      let ret_str = match r.return_type with None -> "" | Some t -> " : " ^ string_of_typ t in
      "(def " ^ r.name ^ args_str ^ ret_str ^ " " ^ string_of_prog r.body ^ ")"
  | Prog r -> 
      "(prog " ^ String.concat " " (List.map string_of_prog r.decls) ^ ")"

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
  | Concat -> "++"
  | Cons -> "::"

and string_of_unop op : string =
  match op with
  | Neg -> "-"

let prop_of_decl (decl : 't decl) : 't =
  match decl with
  | DefFunc r -> r.prop
  | Expr e -> prop_of_prog e

let string_of_decl (decl : 't decl) : string =
  match decl with
  | Expr e -> string_of_prog e
  | DefFunc r -> 
      let args_str = if r.args = [] then "" else " " ^ String.concat " " r.args in
      "(def " ^ r.name ^ args_str ^ " = " ^ string_of_prog r.body ^ ")"