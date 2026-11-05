(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string> IDENT
%token <string> CONSTRUCTOR

// symbol
%token PLUS "+" TIMES "*"
%token PLUSPLUS "++"
%token MINUS "-"
%token DIV "/" INT_DIV "//" MOD "%"
%token GREATER ">" LESSTHAN "<" GREATEREQUAL ">=" LESSTHANEQUAL "<=" EQUALEQUAL "=="
%token OROR "||" ANDAND "&&"
%token OR "|" WILDCARD "_"
%token BACKSLASH "\\"
%token COMMA "," DOT "." EXCLAMATION "!"
%token COLON ":"
%token LPAREN "(" RPAREN ")"
%token RARROW "->"
%token IMPLIES "=>"
%token NOTEQUAL "!="
%token ASSIGN "="
%token DOLLAR "$"

// keyword
%token DEF "def"
%token DATA "data" TYPE "type" IMPORT "import"
%token IF "if" THEN "then" ELSE "else"
%token LET "let" IN "in"
%token AS "as"
%token AND "and"
%token MATCH "match" WITH "with"

%token INTEGER "Integer" BOOLEAN "Bool" FLOAT "Double" CHAR "Char"

%token LBRACKET "[" RBRACKET "]"
%token CONS "::"

%token EOF

(* precedence and associativity declarations (ordered from lowest to highest) *)
%nonassoc DECL
%nonassoc BOOL_LIT INT_LIT FLOAT_LIT CHAR_LIT STRING_LIT IDENT CONSTRUCTOR LBRACKET LPAREN
%right IN IMPLIES
%nonassoc ELSE
%right OR
%right RARROW
%right DOLLAR
%right OROR
%right ANDAND
%nonassoc EQUALEQUAL NOTEQUAL GREATER LESSTHAN GREATEREQUAL LESSTHANEQUAL
%right PLUSPLUS CONS
%left PLUS MINUS
%left TIMES DIV INT_DIV MOD
%nonassoc UMINUS
%right DOT

(* starting nonterminal *)
%start <unit prog> start
%%

(** rules section **)
start:
  | decls = decl_list; EOF { Prog { prop = (); pos = $loc; decls = decls } }

decl_list:
  | /* empty file */ { [] }
  | d = decl; ds = decl_list { d :: ds }

param:
  | "("; ")" { ParamUnit }
  | "("; id = IDENT; ":"; t = typ; ")" { ParamTyped (id, t) }

param_list:
  | /* empty */ { [] }
  | p = param; ps = param_list { p :: ps }

nonempty_param_list:
  | p = param { [p] }
  | p = param; ps = nonempty_param_list { p :: ps }

mod_name:
  | id = IDENT { id }
  | c = CONSTRUCTOR { c }
  | s = STRING_LIT { s }

id_list:
  | /* empty */ { [] }
  | id = IDENT; ids = id_list { id :: ids }

decl:
  | e = expr %prec DECL { e }

  | "def"; id = IDENT; params = param_list; ":"; ret = typ; "="; body = expr %prec DECL {
      DefFunc {
        prop = ();
        pos = $loc;
        name = id;
        params = params;
        return_type = ret;
        body = body;
      }
    }

  | "import"; path = mod_name {
      Import { prop = (); pos = $loc; path = path; alias = None }
    }
  | "import"; path = mod_name; "as"; a = mod_name {
      Import { prop = (); pos = $loc; path = path; alias = Some a }
    }

  | "type"; name = CONSTRUCTOR; tparams = id_list; "="; t = typ {
      TypeDef { prop = (); pos = $loc; name = name; type_params = tparams; typ = t }
    }

  | "data"; name = CONSTRUCTOR; tparams = id_list; "="; constructors = data_constructors {
      DataDef { prop = (); pos = $loc; name = name; type_params = tparams; constructors = constructors }
    }

simple_expr:
  | n = BOOL_LIT { BoolLit { prop = (); pos = $loc; value = n } }
  | n = INT_LIT { IntLit { prop = (); pos = $loc; value = n } }
  | n = FLOAT_LIT { FloatLit { prop = (); pos = $loc; value = n } }
  | n = CHAR_LIT { CharLit { prop = (); pos = $loc; value = n } }
  | s = STRING_LIT { StringLit { prop = (); pos = $loc; value = s } }
  | id = IDENT { Var { prop = (); pos = $loc; name = id } }
  | c = CONSTRUCTOR { Constructor { prop = (); pos = $loc; name = c } }
  | ns = simple_expr; "!"; m = IDENT { Namespace { prop = (); pos = $loc; ns = ns; member = m } }
  | ns = simple_expr; "!"; m = CONSTRUCTOR { Namespace { prop = (); pos = $loc; ns = ns; member = m } }
  | "["; "]" { EmptyList { prop = (); pos = $loc } }
  | "("; ")" { UnitLit { prop = (); pos = $loc } }
  | "("; e = expr; ")" { e }
  | "("; e1 = expr; ","; e2 = expr; ")" { Pair { prop = (); pos = $loc; fst = e1; snd = e2 } }

app_expr:
  | e = simple_expr { e }
  | func = app_expr; arg = simple_expr {
      App { prop = (); pos = $loc; func = func; arg = arg }
    }

expr:
  | e = app_expr %prec DECL { e }

  | "if"; c = expr; "then"; e1 = expr; "else"; e2 = expr {
      If {
        prop = ();
        pos = $loc;
        condition = c;
        consequent = e1;
        alternative = e2;
      }
    }

  | "let"; bindings = let_bindings; "in"; body = expr {
      Let {
        prop = ();
        pos = $loc;
        bindings = bindings;
        body = body;
      }
    }

  | "\\"; params = nonempty_param_list; ":"; ret = typ; "=>"; body = expr {
      Lambda {
        prop = ();
        pos = $loc;
        params = params;
        return_type = ret;
        body = body;
      }
    }

  | e1 = expr; "."; e2 = expr { BinExpr { prop = (); pos = $loc; op = Compose; left = e1; right = e2 } }
  | "-"; e1 = expr %prec UMINUS { UnaryExpr { prop = (); pos = $loc; op = Neg; operand = e1 } }
  | e1 = expr; "*"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Mult; left = e1; right = e2 } }
  | e1 = expr; "/"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Div; left = e1; right = e2 } }
  | e1 = expr; "//"; e2 = expr { BinExpr { prop = (); pos = $loc; op = IntDiv; left = e1; right = e2 } }
  | e1 = expr; "%"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Mod; left = e1; right = e2 } }
  | e1 = expr; "+"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Add; left = e1; right = e2 } }
  | e1 = expr; "-"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Sub; left = e1; right = e2 } }
  | e1 = expr; "++"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Concat; left = e1; right = e2 } }
  | left = expr; "::"; right = expr { BinExpr { prop = (); pos = $loc; op = Cons; left = left; right = right } }
  | e1 = expr; "=="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Eq; left = e1; right = e2 } }
  | e1 = expr; "!="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Neq; left = e1; right = e2 } }
  | e1 = expr; ">="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Geq; left = e1; right = e2 } }
  | e1 = expr; "<="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Leq; left = e1; right = e2 } }
  | e1 = expr; ">"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Gt; left = e1; right = e2 } }
  | e1 = expr; "<"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Lt; left = e1; right = e2 } }
  | e1 = expr; "&&"; e2 = expr { BinExpr { prop = (); pos = $loc; op = LAnd; left = e1; right = e2 } }
  | e1 = expr; "||"; e2 = expr { BinExpr { prop = (); pos = $loc; op = LOr; left = e1; right = e2 } }
  | e1 = expr; "$"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Dollar; left = e1; right = e2 } }

  | "match"; e = expr; "with"; cases = match_cases {
      Match {
        prop = ();
        pos = $loc;
        expr = e;
        cases = cases;
      }
    }

let_bindings:
  | b = let_binding { [b] }
  | b = let_binding; "and"; bs = let_bindings { b :: bs }

let_binding:
  | id = IDENT; "="; e = expr {
      LetVarBind { name = id; value = e }
    }
  | id = IDENT; params = nonempty_param_list; ":"; ret = typ; "="; e = expr {
      LetFuncBind { name = id; params = params; return_type = ret; body = e }
    }

simple_pat:
  | "_" { PatWildcard { prop = (); pos = $loc } }
  | id = IDENT { PatVar { prop = (); pos = $loc; name = id } }
  | n = INT_LIT { PatInt { prop = (); pos = $loc; value = n } }
  | n = BOOL_LIT { PatBool { prop = (); pos = $loc; value = n } }
  | n = FLOAT_LIT { PatFloat { prop = (); pos = $loc; value = n } }
  | n = CHAR_LIT { PatChar { prop = (); pos = $loc; value = n } }
  | s = STRING_LIT { PatString { prop = (); pos = $loc; value = s } }
  | "["; "]" { PatEmptyList { prop = (); pos = $loc } }
  | "("; ")" { PatUnit { prop = (); pos = $loc } }
  | "("; p = pat; ")" { p }
  | "("; p1 = pat; ","; p2 = pat; ")" { PatPair { prop = (); pos = $loc; fst = p1; snd = p2 } }
  | c = CONSTRUCTOR { PatConstructor { prop = (); pos = $loc; name = c; args = [] } }

ctor_pat_args:
  | p = simple_pat { [p] }
  | p = simple_pat; ps = ctor_pat_args { p :: ps }

app_pat:
  | p = simple_pat { p }
  | c = CONSTRUCTOR; args = ctor_pat_args {
      PatConstructor { prop = (); pos = $loc; name = c; args = args }
    }

pat:
  | p = app_pat { p }
  | head = app_pat; "::"; tail = pat {
      PatCons { prop = (); pos = $loc; head = head; tail = tail }
    }

match_cases:
  | c = match_case %prec OR { [c] }
  | c = match_case; cases = match_cases { c :: cases }

match_case:
  | "|"; p = pat; "->"; e = expr { (p, e) }

simple_typ:
  | "Integer" { TyBase "Integer" }
  | "Bool" { TyBase "Bool" }
  | "Double" { TyBase "Double" }
  | "Char" { TyBase "Char" }
  | id = IDENT { TyVar id }
  | c = CONSTRUCTOR { TyBase c }
  | "("; ")" { TyUnit }
  | "["; t = typ; "]" { TyList t }
  | "("; t = typ; ")" { t }
  | "("; t1 = typ; ","; t2 = typ; ")" { TyPair (t1, t2) }

app_typ:
  | t = simple_typ { t }
  | t1 = app_typ; t2 = simple_typ { TyApp (t1, t2) }

typ:
  | t = app_typ %prec DECL { t }
  | t1 = app_typ; "->"; t2 = typ { TyArrow (t1, t2) }

data_constructors:
  | c = data_constructor { [c] }
  | c = data_constructor; "|"; cs = data_constructors { c :: cs }

data_constructor:
  | name = CONSTRUCTOR; args = simple_typ_list { (name, args) }

simple_typ_list:
  | /* empty */ %prec DECL { [] }
  | t = simple_typ; ts = simple_typ_list { t :: ts }