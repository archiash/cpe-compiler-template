(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
%token <int> INT_LIT
%token TRUE "true"
%token FALSE "false"
%token PLUS "+" TIMES "*"
%token MINUS "-"
%token DIV "/" INT_DIV "//" MOD "%"
%token OROR "||" ANDAND "&&"
%token LPAREN "(" RPAREN ")"
%token EOF

(* precedence and associativity declarations *)
%left OROR
%left ANDAND
%left PLUS MINUS
%left TIMES DIV INT_DIV MOD

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
  | "("; e = expr; ")" { e }