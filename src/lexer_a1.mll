(** header: optional imports & setup **)
{
open Token

let error msg lexbuf =
  Error(msg, Lexing.lexeme_start_p lexbuf)

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let keywords= "def" | "if" | "then" | "else" | "data" | "type" | "import" | "as" | "and" | "in" | "match" | "with"
let symbols = "*" | "/" | "//" | "%" | "+" | "-" | "::" | "++" | "==" | "<" | "<=" | ">=" | ">" | "&&" | "||" | "$" 
            | "(" | ")" | "[" | "]" | "::" | "->" | "|" | "_" | "\\" | "=>"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let upper_ident = ['A' - 'Z'] (letter | digit)*
let lower_ident = ['a' - 'z'] (letter | digit)*
let escape   = '\\' ['\\' '\'' '"' 'n' 'r' 't' 'b'] 
let bool_lit = "True" | "False"
let int_lit = digit+
let float_lit = digit+ '.' digit+

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | symbols as s { Symbol(s)}
  | keywords as k { Keyword(k)}
  | int_lit as i {IntLit (int_of_string i) }
  | float_lit as f {FloatLit (float_of_string f)}
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
