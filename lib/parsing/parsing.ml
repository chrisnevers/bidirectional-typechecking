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

let lex_string s =
  let buf  = Lexing.from_string s in
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
  print_endline @@ "hd: " ^ Exp.show hd;
  print_endline @@ "e: " ^ Exp.show e;
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
    let e = parse_exp tokens (SameLineOrGt pos) in
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
    let tl = parse_exp_0 tokens (SameLine l_pos) in
    begin match tl with
    | Exp.App (l, r) -> FApp (hd :: l :: r :: [])
    | _ -> Exp.App (hd, tl)
    end
  | _ -> hd

and parse_args tokens indent delim =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TVAR id when compare_indent pos indent  ->
    consume_token tokens;
    Ident.ident id :: parse_args tokens indent delim
  | t when t = delim -> []
  | _ -> "Error parsing function arguments. Expected identifiers." |> error

and parse_exp_1 tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TFUN when compare_indent pos indent ->
    consume_token tokens;
    let args = parse_args tokens (SameLine pos) TARROW in
    expect_token TARROW tokens;
    let e = parse_exp tokens (SameLineOrGt pos) in
    curry_abs e args
  | _ -> parse_exp_0 tokens indent

and parse_exp tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TLET when compare_indent pos indent ->
    consume_token tokens;
    let ids = parse_args tokens (SameLine pos) TEQ in
    expect_token TEQ tokens;
    let id = List.hd ids in
    let args = List.tl ids in
    let e_indent = if List.length ids > 1 then (SameLineOrGt pos) else (SameLine pos) in
    let e = parse_exp tokens e_indent in
    let b = parse_exp tokens (NextLineEqCol pos) in
    let res = List.fold_left (fun acc id -> Exp.Abs (id, acc)) e args in
    App (Abs (id, b), res)
  | _ -> parse_exp_1 tokens indent

let rec curry exp =
  let open Exp in
  match exp with
  | FApp es -> curry @@ List.fold_left (fun e acc -> App (e, acc)) (List.hd es) (List.tl es)
  | App (l, r) -> App (curry l, curry r)
  | Abs (id, e) -> Abs (id, curry e)
  | HasType (e, t) -> HasType (curry e, t)
  | _ -> exp

let parse_tokens tokens =
  curry @@ parse_exp (ref tokens) Any

open Gensym

let test s =
  Gensym.reset();
  lex_string s |> parse_tokens

let%test "parse id" =test "a" = Exp.Var ("a", 0)
let%test "parse False" = test "False" = Exp.Bool false
let%test "parse True" = test "True" = Exp.Bool true
let%test "parse Int" = test "52" = Exp.Int 52
let%test "parse Paren" = test "(True)" = Exp.Bool true
let%test "parse apply" = test "add 1 2" = Exp.App (Exp.App (Exp.Var ("add", 0), Exp.Int 1), Exp.Int 2)
let%test "parse let" = test "let five = 5\nfive" = Exp.App (Exp.Abs (("five", 0), Exp.Var ("five", 0)), Exp.Int 5)
let%test "parse let" = test "let id x = x\nid" =
  let id = Ident.ident "id" in
  let x = Ident.ident "x" in
  Exp.App (Exp.Abs (id, Exp.Var id), Abs (x, Exp.Var x))
