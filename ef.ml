open AST
open Interp

exception Error of exn * (int * int * string * string)

let parse lexbuf =
  try 
    Parser.expr Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol + 1 in
      let tok = Lexing.lexeme lexbuf in
      Printf.eprintf "Unexpected token '%s' at %d:%d\n" tok line cnum;
      let tail = "" in
      raise (Error (exn,(line,cnum,tok,tail)))
    end

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = parse lexbuf in
  let t = Types.type_check Types.empty_env ast in
  let result = Interp.interp Interp.empty_env ast in
  Format.fprintf Format.std_formatter "%a : %a\n" AST.pp_val result Types.pp_type t
