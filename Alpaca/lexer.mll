{
  open Parser
  exception Eof
  exception Unrecognized
}

let any = _
let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']
let sign = ['+' '-']
let frac = '.' digit+
let exp = ['e' 'E'] sign? digit+
let white = [' ' '\t' '\n' '\r']+ | "//" ([^ '\n' '\r'])*
let newline = '\n' | '\r' | "\r\n"
let dblsemi = ";;"
let comment = "(*" _* "*)"
(*let comment = "(*"+ ['a' - 'z' 'A' - 'Z' '0' - '9']+  "*)" *)
let float = (digit+ '.'? | digit* frac) exp?
let true = "true" | "#t"
let false = "false" | "#f"
let list = "list"
let tuple = "pair" | "tuple" | "triple"
let lett = "let"
let comp = ">" | ">=" | "<" | "<="
let id = alpha (alpha | digit)*

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | comment     { token lexbuf }
  | dblsemi     { DBLSEMI }
  | float as x  { FLOAT (float_of_string x) }
  | true        { TRUE }
  | false       { FALSE }
  | lett        { LET }
  | list        { LIST }
  | tuple       { TUPLE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "and"       { AND }
  | "or"        { OR }
  | "not"       { NOT }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIVIDE }
  | comp as s   { COMPOP s }
  | "=="        { EQ }
  | "!="        { NEQ }
  | "="         { EQS }
  | id as s     { IDENTIFIER s }
  | eof         { raise Eof }
  | any         { raise Unrecognized }
