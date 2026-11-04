type pos = Lexing.position * Lexing.position

type 't pat =
  | PatWildcard of { prop : 't; pos : pos }
  | PatVar of { prop : 't; pos : pos; name : string }
  | PatInt of { prop : 't; pos : pos; value : int }
  | PatBool of { prop : 't; pos : pos; value : bool }
  | PatFloat of { prop : 't; pos : pos; value : float }
  | PatChar of { prop : 't; pos : pos; value : char }
  | PatString of { prop : 't; pos : pos; value : string }
  | PatUnit of { prop : 't; pos : pos }
  | PatEmptyList of { prop : 't; pos : pos }
  | PatCons of { prop : 't; pos : pos; head : 't pat; tail : 't pat }
  | PatPair of { prop : 't; pos : pos; fst : 't pat; snd : 't pat }
  | PatConstructor of { prop : 't; pos : pos; name : string; args : 't pat list }

type 't typ =
  | TyBase of string
  | TyVar of string
  | TyUnit
  | TyList of 't typ
  | TyPair of 't typ * 't typ
  | TyArrow of 't typ * 't typ
  | TyApp of 't typ * 't typ

type 't param =
  | ParamUnit
  | ParamUntyped of string
  | ParamTyped of string * 't typ

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
  | StringLit of {
      prop : 't;
      pos : pos;
      value : string;
    }
  | UnitLit of {
      prop : 't;
      pos : pos;
    }
  | EmptyList of {
      prop : 't;
      pos : pos;
    }
  | Var of {
      prop : 't;
      pos : pos;
      name : string;
    }
  | Constructor of {
      prop : 't;
      pos : pos;
      name : string;
    }
  | Namespace of {
      prop : 't;
      pos : pos;
      ns : 't prog;
      member : string;
    }
  | Pair of {
      prop : 't;
      pos : pos;
      fst : 't prog;
      snd : 't prog;
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
  | App of {
      prop : 't;
      pos : pos;
      func : 't prog;
      arg : 't prog;
    }
  | If of {
      prop : 't;
      pos : pos;
      condition : 't prog;
      consequent : 't prog;
      alternative : 't prog;
    }
  | Let of {
      prop : 't;
      pos : pos;
      name : string;
      value : 't prog;
      body : 't prog;
    }
  | LetBindings of {
      prop : 't;
      pos : pos;
      bindings : 't let_binding list;
      body : 't prog;
    }
  | Lambda of {
      prop : 't;
      pos : pos;
      params : 't param list;
      return_type : 't typ option;
      body : 't prog;
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
      args : string list;
      body : 't prog;
    }
  | DefFuncTyped of {
      prop : 't;
      pos : pos;
      name : string;
      params : 't param list;
      return_type : 't typ option;
      body : 't prog;
    }
  | Import of {
      prop : 't;
      pos : pos;
      path : string;
      alias : string option;
    }
  | TypeDef of {
      prop : 't;
      pos : pos;
      name : string;
      type_params : string list;
      typ : 't typ;
    }
  | DataDef of {
      prop : 't;
      pos : pos;
      name : string;
      type_params : string list;
      constructors : (string * 't typ list) list;
    }
  | Prog of {
      prop : 't;
      pos : pos;
      decls : 't prog list;
    }

and 't let_binding =
  | LetVarBind of {
      name : string;
      return_type : 't typ option;
      value : 't prog;
    }
  | LetFuncBind of {
      name : string;
      params : 't param list;
      return_type : 't typ option;
      body : 't prog;
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
  | Compose
  | Dollar

and unop =
  | Neg

let prop_of_prog (prog : 't prog) : 't =
  match prog with
  | IntLit r -> r.prop
  | BoolLit r -> r.prop
  | FloatLit r -> r.prop
  | CharLit r -> r.prop
  | StringLit r -> r.prop
  | UnitLit r -> r.prop
  | EmptyList r -> r.prop
  | Var r -> r.prop
  | Constructor r -> r.prop
  | Namespace r -> r.prop
  | Pair r -> r.prop
  | BinExpr r -> r.prop
  | UnaryExpr r -> r.prop
  | App r -> r.prop
  | If r -> r.prop
  | Let r -> r.prop
  | LetBindings r -> r.prop
  | Lambda r -> r.prop
  | Match r -> r.prop
  | DefFunc r -> r.prop
  | DefFuncTyped r -> r.prop
  | Import r -> r.prop
  | TypeDef r -> r.prop
  | DataDef r -> r.prop
  | Prog r -> r.prop

let rec string_of_typ (t : 't typ) : string =
  match t with
  | TyBase name -> name
  | TyVar name -> name
  | TyUnit -> "()"
  | TyList inner -> "([] " ^ string_of_typ inner ^ ")"
  | TyPair (t1, t2) -> "(, " ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ ")"
  | TyArrow (t1, t2) -> "(-> " ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ ")"
  | TyApp (t1, t2) -> "(" ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ ")"

let string_of_param (p : 't param) : string =
  match p with
  | ParamUnit -> "()"
  | ParamUntyped id -> id
  | ParamTyped (id, t) -> "(" ^ id ^ " " ^ string_of_typ t ^ ")"

let string_of_typedecl (name : string) (type_params : string list) : string =
  match type_params with
  | [] -> name
  | params -> "(" ^ name ^ " " ^ String.concat " " params ^ ")"

let rec string_of_pat (pat : 't pat) : string =
  match pat with
  | PatWildcard _ -> "_"
  | PatVar r -> r.name
  | PatInt r -> string_of_int r.value
  | PatBool r -> if r.value then "True" else "False"
  | PatFloat r -> string_of_float r.value
  | PatChar r -> "'" ^ Char.escaped r.value ^ "'"
  | PatString r -> "\"" ^ String.escaped r.value ^ "\""
  | PatUnit _ -> "()"
  | PatEmptyList _ -> "[]"
  | PatCons r -> "(:: " ^ string_of_pat r.head ^ " " ^ string_of_pat r.tail ^ ")"
  | PatPair r -> "(, " ^ string_of_pat r.fst ^ " " ^ string_of_pat r.snd ^ ")"
  | PatConstructor r ->
      if r.args = [] then r.name
      else "(" ^ r.name ^ " " ^ String.concat " " (List.map string_of_pat r.args) ^ ")"

let rec string_of_prog (prog : 't prog) : string =
  match prog with
  | IntLit r -> string_of_int r.value
  | BoolLit r -> if r.value then "True" else "False"
  | FloatLit r -> string_of_float r.value
  | CharLit r -> "'" ^ Char.escaped r.value ^ "'"
  | StringLit r -> "\"" ^ String.escaped r.value ^ "\""
  | UnitLit _ -> "()"
  | EmptyList _ -> "[]"
  | Var r -> r.name
  | Constructor r -> r.name
  | Namespace r -> "(! " ^ string_of_prog r.ns ^ " " ^ r.member ^ ")"
  | Pair r -> "(, " ^ string_of_prog r.fst ^ " " ^ string_of_prog r.snd ^ ")"
  | BinExpr r ->
      "(" ^ string_of_op r.op ^ " " ^ string_of_prog r.left ^ " " ^ string_of_prog r.right ^ ")"
  | UnaryExpr r ->
      "(" ^ string_of_unop r.op ^ " " ^ string_of_prog r.operand ^ ")"
  | App r ->
      "(" ^ string_of_prog r.func ^ " " ^ string_of_prog r.arg ^ ")"
  | If r ->
      "(if " ^ string_of_prog r.condition ^ " " ^ string_of_prog r.consequent ^ " " ^ string_of_prog r.alternative ^ ")"
  | Let r ->
      "(let ((" ^ r.name ^ " " ^ string_of_prog r.value ^ ")) " ^ string_of_prog r.body ^ ")"
  | LetBindings r ->
      let string_of_binding b =
        match b with
        | LetVarBind vb ->
            "(" ^ vb.name ^ " " ^ string_of_prog vb.value ^ ")"
        | LetFuncBind fb ->
            let params_str = "(" ^ String.concat " " (List.map string_of_param fb.params) ^ ")" in
            let ret_str =
              match fb.return_type with
              | Some t -> " " ^ string_of_typ t
              | None -> ""
            in
            "(" ^ fb.name ^ " " ^ params_str ^ ret_str ^ " " ^ string_of_prog fb.body ^ ")"
      in
      "(let (" ^ String.concat " " (List.map string_of_binding r.bindings) ^ ") " ^ string_of_prog r.body ^ ")"
  | Lambda r ->
      let params_str = "(" ^ String.concat " " (List.map string_of_param r.params) ^ ")" in
      let ret_str =
        match r.return_type with
        | Some t -> " " ^ string_of_typ t
        | None -> ""
      in
      "(\\ " ^ params_str ^ ret_str ^ " " ^ string_of_prog r.body ^ ")"
  | Match r ->
      let string_of_case (p, e) = "(-> " ^ string_of_pat p ^ " " ^ string_of_prog e ^ ")" in
      "(match " ^ string_of_prog r.expr ^ " (" ^ String.concat " " (List.map string_of_case r.cases) ^ "))"
  | DefFunc r ->
      let params_str =
        if r.args = [] then ""
        else " (" ^ String.concat " " r.args ^ ")"
      in
      "(def " ^ r.name ^ params_str ^ " " ^ string_of_prog r.body ^ ")"
  | DefFuncTyped r ->
      let params_str =
        if r.params = [] then ""
        else " (" ^ String.concat " " (List.map string_of_param r.params) ^ ")"
      in
      let ret_str =
        match r.return_type with
        | Some t -> " " ^ string_of_typ t
        | None -> ""
      in
      "(def " ^ r.name ^ params_str ^ ret_str ^ " " ^ string_of_prog r.body ^ ")"
  | Import r ->
      "(import " ^ r.path ^ (match r.alias with Some a -> " " ^ a | None -> "") ^ ")"
  | TypeDef r ->
      "(type " ^ string_of_typedecl r.name r.type_params ^ " " ^ string_of_typ r.typ ^ ")"
  | DataDef r ->
      let string_of_ctor (name, args) =
        if args = [] then name
        else "(" ^ name ^ " " ^ String.concat " " (List.map string_of_typ args) ^ ")"
      in
      "(data " ^ string_of_typedecl r.name r.type_params ^ " (" ^ String.concat " " (List.map string_of_ctor r.constructors) ^ "))"
  | Prog r ->
      let is_import d = match d with Import _ -> true | _ -> false in
      let imports, defns = List.partition is_import r.decls in
      let imports_str = "(" ^ String.concat " " (List.map string_of_prog imports) ^ ")" in
      let defns_str = "(" ^ String.concat " " (List.map string_of_prog defns) ^ ")" in
      "(" ^ imports_str ^ " " ^ defns_str ^ ")"

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
  | Compose -> "."
  | Dollar -> "$"

and string_of_unop op : string =
  match op with
  | Neg -> "-"