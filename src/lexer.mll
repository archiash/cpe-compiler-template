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

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "-" { MINUS }
  | "/" { DIV }
  | "//" { INT_DIV }
  | "%" { MOD }
  | "||" { OROR }
  | "&&" { ANDAND }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | digit+ as x { INT_LIT (int_of_string x) }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
