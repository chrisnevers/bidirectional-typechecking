{
  open Ast.Token
  open Ast.Pos
  open Lexing

  let pos buf =
    let line = buf.lex_start_p.pos_lnum in
    let col = buf.lex_start_p.pos_cnum - buf.lex_start_p.pos_bol in
    Pos (line, col)
}

let id_start = ['a'-'z''_']
let id_rest  = ['a'-'z''A'-'Z''_'''''0'-'9']
let id_regex = id_start id_rest*

let int_regex = ['0'-'9']+

rule token = parse
  | '\n'      { Lexing.new_line lexbuf; token lexbuf }
  | [' ''\t'] { token lexbuf }
  | "()"      { HasPos (TUNIT, pos lexbuf) }
  | '('       { HasPos (TLPAREN, pos lexbuf) }
  | ')'       { HasPos (TRPAREN, pos lexbuf) }
  | "fun"     { HasPos (TFUN, pos lexbuf) }
  | "rec"     { HasPos (TREC, pos lexbuf) }
  | "->"      { HasPos (TARROW, pos lexbuf) }
  | "let"     { HasPos (TLET, pos lexbuf) }
  | '='       { HasPos (TEQ, pos lexbuf) }
  | '+'       { HasPos (TADD, pos lexbuf) }
  | '-'       { HasPos (TSUB, pos lexbuf) }
  | '*'       { HasPos (TMUL, pos lexbuf) }
  | '/'       { HasPos (TDIV, pos lexbuf) }
  | "if"      { HasPos (TIF, pos lexbuf) }
  | "else"    { HasPos (TELSE, pos lexbuf) }
  | "True"    { HasPos (TBOOL true, pos lexbuf) }
  | "False"   { HasPos (TBOOL false, pos lexbuf) }
  | ','       { HasPos (TCOMMA, pos lexbuf) }
  | '$'       { HasPos (TDOLLAR, pos lexbuf) }
  | id_regex as id
              { HasPos (TVAR id, pos lexbuf) }
  | int_regex as i
              { HasPos (TINT (int_of_string i), pos lexbuf) }
  | eof       { HasPos (TEOF, pos lexbuf) }

