{
  open Scanf
  open Parser

  let keyword x =
      let table = Hashtbl.create 53 in
      List.iter (fun (k, v) -> Hashtbl.add table k v)
      	[ "true", TRUE
	; "false", FALSE
	; "if", IF
	; "then", THEN
	; "else", ELSE
	; "let", LET
	; "be", BE
	; "in", IN
	; "str", STRING_TYPE
	; "num", NUMBER_TYPE
	; "bool", BOOLEAN_TYPE
	; "case", CASE
	];
    try Hashtbl.find table x
    with Not_found -> VAR x

  let strip_quotes s = String.sub s 1 ((String.length s) - 2)
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let ws = [' ' '\t' '\r']+
let num = ['0'-'9']+
let arrow = '-' '>'
let cons = ':' ':'
let emptylist = '[' ']'
let quoted_string = '"' [^'"']* '"'

rule token = parse
  | ws { token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\\' { LAMBDA }
  | '.' { SUCHTHAT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '+' '+' { CONCAT }
  | '+' { PLUS }
  | '*' { TIMES }
  | '|' { BAR }
  | ';' { SEMI }
  | ',' { COMMA }
  | cons { CONS }
  | emptylist { EMPTY_LIST }
  | arrow { ARROW }
  | quoted_string as s { QUOTED_STRING (strip_quotes s) }
  | num as x { INT (int_of_string x) }
  | id as x { keyword x }
  | eof { EOF }
  | _ as x { Printf.eprintf "Unexpected character: '%c'\n" x; raise Error }
