(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> IDENT

%token TRUE "true"
%token FALSE "false"
%token PLUS "+" TIMES "*"
%token PLUSPLUS "++"
%token MINUS "-"
%token DIV "/" INT_DIV "//" MOD "%"
%token GREATER ">" LESSTHAN "<" GREATEREQUAL ">=" LESSTHANEQUAL "<=" EQUALEQUAL "=="
%token OROR "||" ANDAND "&&"
%token LPAREN "(" RPAREN ")"
%token NOTEQUAL "!="

%token IF "if" THEN "then" ELSE "else"
%token LET "let" IN "in" ASSIGN "="
%token EOF

(* precedence and associativity declarations *)
%right IN
%nonassoc ELSE
%right OROR
%right ANDAND
%nonassoc EQUALEQUAL NOTEQUAL GREATER LESSTHAN GREATEREQUAL LESSTHANEQUAL
%right PLUSPLUS
%left PLUS MINUS
%left TIMES DIV INT_DIV MOD
%nonassoc UMINUS

(* starting nonterminal *)
%start <unit prog> start
%%

(** rules section **)
start:
  | prog = prog; EOF { prog }

prog:
  | e = expr { e }

expr:
  | e1 = expr; "+"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Add;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "-"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Sub;
      left = e1;
      right = e2;}}
  | e1 = expr; "*"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Mult;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "/" ; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Div;
      left = e1;
      right = e2;
  }}
  |  e1 = expr; "//"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = IntDiv;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "%" ; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Mod;
      left = e1;
      right = e2;
  }}
  | e1  = expr; "==" ; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Eq;
      left = e1;
      right = e2;
  }}
  | e1  = expr; "!=" ; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Neq;
      left = e1;
      right = e2;
  }}  
  | e1 = expr; ">=" ; e2 = expr { BinExpr{
      prop = ();
      pos = $loc;
      op = Geq;
      left = e1;
      right = e2;
  }}
  | e1 = expr; "<=" ; e2 = expr { BinExpr{
      prop = ();
      pos = $loc;
      op = Leq;
      left = e1;
      right = e2;
  }}
  | e1 = expr; ">" ; e2 = expr { BinExpr{
      prop = ();
      pos = $loc;
      op = Gt;
      left = e1;
      right = e2;
  }}
  | e1 = expr; "<" ; e2 = expr { BinExpr{
      prop = ();
      pos = $loc;
      op = Lt;
      left = e1;
      right = e2;
  }}
  | e1 = expr; "||"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = LOr;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "&&"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = LAnd;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "++"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = StringConcat;
      left = e1;
      right = e2;
    }}
  | "-"; e1 = expr %prec UMINUS { UnaryExpr {
      prop = ();
      pos = $loc;
      op = Neg;
      operand = e1;
    }}
  | "true" { BoolLit {
      prop = ();
      pos = $loc;
      value = true;
    }}
  | "false" { BoolLit {
      prop = ();
      pos = $loc;
      value = false;
    }}
  | n = INT_LIT { IntLit {
      prop = ();
      pos = $loc;
      value = n;
    }}
  | n = FLOAT_LIT { FLOAT_LIT {
      prop = ();
      pos = $loc;
      value = n;
    }}
  | "("; e = expr; ")" { 
      e 
    }
  
  | s = STRING_LIT { StringLit {
      prop = ();
      pos = $loc;
      value = s;
    }}

  | id = IDENT { Var {
      prop = ();
      pos = $loc;
      name = id;
    }}

  | "if"; cond = expr; "then"; e1 = expr; "else"; e2 = expr { If {
      prop = ();
      pos = $loc;
      condition = cond;
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
