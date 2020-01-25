open Lexer
open Ast
open Ast.Pos
open Ast.Token

let rec lex fn buf =
  let token = fn buf in
  match token with
  | HasPos (TEOF, _)  -> token :: []
  | _                 -> token :: lex fn buf

let lex_file filename =
  let chan = open_in filename in
  let buf  = Lexing.from_channel chan in
  let tokens = lex token buf in
  tokens

type indent =
  | SameLine of Pos.t
  | NextLineEqCol of Pos.t
  | NextLineGtCol of Pos.t
  | SameLineOrGt  of Pos.t
  | Any

let show_indent = function
  | SameLine p -> "SameLine " ^ Pos.show p
  | NextLineEqCol p -> "NextLineEqCol " ^ Pos.show p
  | NextLineGtCol p -> "NextLineGtCol " ^ Pos.show p
  | SameLineOrGt p -> "SameLineOrGt " ^ Pos.show p
  | Any -> "Any"

let compare_indent actual expected =
  let Pos (new_line, new_col) = actual in
  match expected with
  | SameLine (Pos (last_line, _)) -> new_line = last_line
  | NextLineEqCol (Pos (last_line, last_col)) -> new_line > last_line && new_col = last_col
  | NextLineGtCol (Pos (last_line, last_col)) -> new_line > last_line && new_col > last_col
  | SameLineOrGt (Pos (last_line, last_col)) -> new_line = last_line || new_col > last_col
  | Any -> true

exception ParseError of string
let error msg = raise @@ ParseError ("Parse Error: " ^ msg)

let consume_token tokens =
  tokens := List.tl !tokens

let next_token tokens =
  let open List in
  let token = hd !tokens in
  consume_token tokens;
  token

let peek_token tokens =
  List.hd !tokens

let expect_token expected tokens =
  let actual = next_token tokens in
  let HasPos (actual_tok, _) = actual in
  if actual_tok <> expected then
    Format.sprintf "Expected %s but received %s"
    (show expected) (show actual_tok) |> error;
  ()

let mt = Pos.Pos (0, 0)

let rec curry_app hd e =
  let open Exp in
  match e with
  | App (l, r) -> App (curry_app hd l, r)
  | ow -> App (hd, ow)

let rec curry_abs e = function
  | [] -> e
  | id :: t -> Exp.Abs (id, curry_abs e t)

let parse_id tokens indent =
  let HasPos (tok, pos) = next_token tokens in
  match tok with
  | TVAR id when compare_indent pos indent -> Ident.ident id
  | _ -> Format.sprintf "Expected id, but received %s" (show tok) |> error

let rec parse_exp_0_head tokens indent =
  let next = next_token tokens in
  let HasPos (tok, pos) = next in
  match tok with
  | TINT i  when compare_indent pos indent -> Exp.Int i, pos
  | TBOOL b when compare_indent pos indent -> Exp.Bool b, pos
  | TVAR i  when compare_indent pos indent -> Exp.Var (Ident.ident i), pos
  | TSTRING s when compare_indent pos indent -> Exp.String s, pos
  | TLPAREN when compare_indent pos indent ->
    let e = parse_exp tokens indent in
    let _ = expect_token TRPAREN tokens in
    e, pos
  | _ -> Format.sprintf "Expected an int, variable, or (, but received %s (at %s)"
        (show tok) (show_indent indent) |> error

and parse_exp_0 tokens indent =
  let hd, l_pos = parse_exp_0_head tokens indent in
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  (* Apply *)
  | TINT _ | TVAR _ | TLPAREN
  | TBOOL _ when compare_indent pos (SameLine l_pos) ->
    let tl = parse_exp_0 tokens (SameLine l_pos) in curry_app hd tl
  | _ -> hd

and parse_args tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TVAR id when compare_indent pos indent  ->
    consume_token tokens;
    Ident.ident id :: parse_args tokens indent
  | TARROW -> []
  | _ -> "Error parsing function arguments. Expected identifiers." |> error

and parse_exp_1 tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TFUN when compare_indent pos indent ->
    consume_token tokens;
    let args = parse_args tokens (SameLine pos) in
    expect_token TARROW tokens;
    let e = parse_exp tokens (SameLineOrGt pos) in
    curry_abs e args
  | _ -> parse_exp_0 tokens indent

and parse_exp tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TLET when compare_indent pos indent ->
    consume_token tokens;
    let id = parse_id tokens (SameLine pos) in
    expect_token TEQ tokens;
    let e = parse_exp tokens (SameLine pos) in
    let b = parse_exp tokens (NextLineEqCol pos) in
    App (Abs (id, b), e)
  | _ -> parse_exp_1 tokens indent

let parse_tokens tokens =
  parse_exp (ref tokens) Any
