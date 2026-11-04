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
// %token <string> CONSTRUCTOR

// symbol
%token PLUS "+" TIMES "*"
%token PLUSPLUS "++"
%token MINUS "-"
%token DIV "/" INT_DIV "//" MOD "%"
%token GREATER ">" LESSTHAN "<" GREATEREQUAL ">=" LESSTHANEQUAL "<=" EQUALEQUAL "=="
%token OROR "||" ANDAND "&&" 
%token OR "|" WILDCARD "_" 
// %token BACKSLASH "\\"
// %token COMMA "," DOT "." EXCLAMATION "!"
%token LPAREN "(" RPAREN ")" 
%token RARROW "->" 
//%token IMPLIES "=>"
%token NOTEQUAL "!="
%token ASSIGN "="
// %token TRUE "True"
// %token FALSE "False"
// %token DOLLAR "$"

// keyword
%token DEF "def" 
// %token DATA "data" TYPE "type" IMPORT "import"
%token IF "if" THEN "then" ELSE "else"
%token LET "let" IN "in"
// %token AS "as" AND "and"
%token MATCH "match" WITH "with"

// %token INTEGER "Integer" BOOLEAN "Bool" FLOAT "Double" CHAR "Char"


%token LBRACKET "[" RBRACKET "]"
%token CONS "::"

%token EOF

(* precedence and associativity declarations *)
%nonassoc DECL
%nonassoc BOOL_LIT INT_LIT FLOAT_LIT CHAR_LIT STRING_LIT IDENT LBRACKET LPAREN
%right IN
%nonassoc ELSE
%right OR          /* Changed to %right to prioritize shifting new cases */
%right RARROW      
%right OROR
%right ANDAND
%nonassoc EQUALEQUAL NOTEQUAL GREATER LESSTHAN GREATEREQUAL LESSTHANEQUAL
%right PLUSPLUS CONS
%left PLUS MINUS
%left TIMES DIV INT_DIV MOD
%nonassoc UMINUS

(* starting nonterminal *)
%start <unit prog> start
%%

(** rules section **)
start:
| decls = decl_list; EOF { Prog { prop = (); pos = $loc; decls = decls } }

decl_list:
  | /* empty file */       { [] }
  | d = decl; ds = decl_list { d :: ds }

decl:
  | e = expr %prec DECL { e }
  
  | "def"; id = IDENT; args = list(IDENT); "="; body = expr %prec DECL { DefFunc {
      prop = ();
      pos = $loc;
      name = id;
      args = args;
      body = body;
    }}


simple_expr:
  | n = BOOL_LIT { BoolLit { prop = (); pos = $loc; value = n } }
  | n = INT_LIT { IntLit { prop = (); pos = $loc; value = n } }
  | n = FLOAT_LIT { FloatLit { prop = (); pos = $loc; value = n } }
  | n = CHAR_LIT { CharLit { prop = (); pos = $loc; value = n } }
  | s = STRING_LIT { StringLit { prop = (); pos = $loc; value = s } }
  | id = IDENT { Var { prop = (); pos = $loc; name = id } }
  | "["; "]" { EmptyList { prop = (); pos = $loc } }
  | "("; e = expr; ")" { e }

app_expr:
  | e = simple_expr { e }
  | func = app_expr; arg = simple_expr { App { 
      prop = (); 
      pos = $loc; 
      func = func; 
      arg = arg 
    }}

expr:
  | e = app_expr %prec DECL { e }
  
  | "if"; c = expr; "then"; e1 = expr; "else"; e2 = expr { If {
      prop = ();
      pos = $loc;
      condition = c;
      consequent = e1;
      alternative = e2;
    }}

  | "let"; id = IDENT; "="; e1 = expr; "in"; e2 = expr { Let {
      prop = ();
      pos = $loc;
      name = id;
      value = e1;
      body = e2;
    }}

  | e1 = expr; "+"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Add; left = e1; right = e2 } }
  | e1 = expr; "-"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Sub; left = e1; right = e2 } }
  | e1 = expr; "*"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Mult; left = e1; right = e2 } }
  | e1 = expr; "/"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Div; left = e1; right = e2 } }
  | e1 = expr; "//"; e2 = expr { BinExpr { prop = (); pos = $loc; op = IntDiv; left = e1; right = e2 } }
  | e1 = expr; "%"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Mod; left = e1; right = e2 } }
  | e1 = expr; "=="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Eq; left = e1; right = e2 } }
  | e1 = expr; "!="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Neq; left = e1; right = e2 } }
  | e1 = expr; ">="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Geq; left = e1; right = e2 } }
  | e1 = expr; "<="; e2 = expr { BinExpr { prop = (); pos = $loc; op = Leq; left = e1; right = e2 } }
  | e1 = expr; ">"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Gt; left = e1; right = e2 } }
  | e1 = expr; "<"; e2 = expr { BinExpr { prop = (); pos = $loc; op = Lt; left = e1; right = e2 } }
  | e1 = expr; "||"; e2 = expr { BinExpr { prop = (); pos = $loc; op = LOr; left = e1; right = e2 } }
  | e1 = expr; "&&"; e2 = expr { BinExpr { prop = (); pos = $loc; op = LAnd; left = e1; right = e2 } }
  | e1 = expr; "++"; e2 = expr { BinExpr { prop = (); pos = $loc; op = StringConcat; left = e1; right = e2 } }
  | left = expr; "::"; right = expr { BinExpr { prop = (); pos = $loc; op = Cons; left = left; right = right } }
  
  | "-"; e1 = expr %prec UMINUS { UnaryExpr { prop = (); pos = $loc; op = Neg; operand = e1 } }
  | "match"; e = expr; "with"; cases = match_cases { Match {
      prop = ();
      pos = $loc;
      expr = e;
      cases = cases;
    }}

simple_pat:
  | "_" { PatWildcard { prop = (); pos = $loc } }
  | id = IDENT { PatVar { prop = (); pos = $loc; name = id } }
  | n = INT_LIT { PatInt { prop = (); pos = $loc; value = n } }
  | n = BOOL_LIT { PatBool { prop = (); pos = $loc; value = n } }
  | "["; "]" { PatEmptyList { prop = (); pos = $loc } }
  | "("; p = pat; ")" { p }

pat:
  | p = simple_pat { p }
  | head = simple_pat; "::"; tail = pat { PatCons {
      prop = (); pos = $loc; head = head; tail = tail
    }}

match_cases:
  | c = match_case %prec OR { [c] }
  | c = match_case; cases = match_cases { c :: cases }

match_case:
  | "|"; p = pat; "->"; e = expr { (p, e) }