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
let float_lit = digit+ '.' digit+
let ident = ['a' - 'z'] (letter | digit)*
let constructor = ['A' - 'Z'] (letter | digit)*
let character = [^ '\\' '\'' '\r' '\n']

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | digit+ as x { INT_LIT (int_of_string x) }
  | float_lit as f { FLOAT_LIT (float_of_string f) }
  | '#' [^ '\r' '\n' ]* { read lexbuf }
  | "(*" { read_comment lexbuf }
  | "def" { DEF }
  (* | "data" { DATA }
  | "type" { TYPE }
  | "import" { IMPORT } *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "match" { MATCH }
  | "with" { WITH }
  (* | "as" { AS }
  | "and" { AND } *)
  | "Interger" { INTEGER }
  | "Bool" { BOOLEAN }
  | "Double" { FLOAT }
  | "Char" { CHAR }
  | "True" { BOOL_LIT(true) }
  | "False" { BOOL_LIT(false) } 
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
  | "=" { ASSIGN }
  | "::" { CONS}
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "->" { RARROW }
  | "|" { OR }
  | "_" { WILDCARD }
  | ":" { COLON }
  (*
  | "\\" { BACKSLASH }
  | "=>" { IMPLIES }
  | "$" { DOLLAR }
  | "," { COMMA }
  | "." { DOT }
  | "!" { EXCLAMATION } *)
  | ident as i { IDENT (i)}
  | constructor as c { CONSTRUCTOR (c)}
  | '\'' (character as c) '\'' { CHAR_LIT c }
  | '\'' '\\' (_ as escape) '\'' { 
      let c = match escape with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | 'b' -> '\b'
        | '\\' -> '\\'
        | '\'' -> '\''
        | '"' -> '"'
        | _ -> failwith "Invalid escape"
      in 
    CHAR_LIT c 
    }
  | '"' { Buffer.clear string_buf; read_string lexbuf }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }

and read_string = parse
  | '"' { STRING_LIT (Buffer.contents string_buf) }
  | '\\' (_ as escape) {       
      let c = match escape with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | 'b' -> '\b'
        | '\\' -> '\\'
        | '\'' -> '\''
        | '"' -> '"'
        | _ -> failwith "Invalid escape"
      in 
      Buffer.add_char string_buf c; read_string lexbuf
      }
  | line_terminator { Lexing.new_line lexbuf;
              Buffer.add_string string_buf (Lexing.lexeme lexbuf);
              read_string lexbuf }
  | eof { error "unterminated string literal" lexbuf }
  | _ as c { Buffer.add_char string_buf c; read_string lexbuf }

and read_comment = parse
  | "*)" { read lexbuf }
  | "(*" { error "nested comment not allowed" lexbuf }
  | line_terminator { Lexing.new_line lexbuf; read_comment lexbuf }
  | eof { error "unterminated comment" lexbuf }
  | _ { read_comment lexbuf }