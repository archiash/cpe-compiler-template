(** header: optional imports & setup **)
{
open Parser

exception SyntaxError of string * Lexing.position
let error msg lexbuf =
  raise (SyntaxError(msg, Lexing.lexeme_start_p lexbuf))

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = ['a' - 'z'] (letter | digit)*

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "-" { MINUS }
  | "/" { DIV }
  | "//" { INT_DIV }
  | "%" { MOD }
  | ">" { GREATER }
  | "<" { LESSTHAN }
  | ">=" { GREATEREQUAL }
  | "<=" { LESSTHANEQUAL }
  | "==" { EQUALEQUAL }
  | "||" { OROR }
  | "&&" { ANDAND }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "!=" { NOTEQUAL }
  | digit+ as x { INT_LIT (int_of_string x) }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | ident as i { IDENT (i)}
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
