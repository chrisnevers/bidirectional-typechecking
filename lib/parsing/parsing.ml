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
  | NextLine of Pos.t
  | NextLineEqCol of Pos.t
  | NextLineGtCol of Pos.t
  | SameLineOrGt  of Pos.t
  | Any

let show_indent = function
  | SameLine p -> "SameLine " ^ Pos.show p
  | NextLine p -> "NextLine " ^ Pos.show p
  | NextLineEqCol p -> "NextLineEqCol " ^ Pos.show p
  | NextLineGtCol p -> "NextLineGtCol " ^ Pos.show p
  | SameLineOrGt p -> "SameLineOrGt " ^ Pos.show p
  | Any -> "Any"

let compare_indent actual expected =
  let Pos (new_line, new_col) = actual in
  match expected with
  | SameLine (Pos (last_line, _)) -> new_line = last_line
  | NextLine (Pos (last_line, _)) -> new_line > last_line
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

let expect_token expected tokens indent =
  let actual = next_token tokens in
  let HasPos (actual_tok, pos) = actual in
  if actual_tok <> expected then
    Format.sprintf "Expected %s but received %s"
    (show expected) (show actual_tok) |> error;
  if compare_indent pos indent |> not then
    Format.sprintf "Expected %s to be indented %s, but received %s"
    (show actual_tok) (show_indent indent) (Pos.show pos) |> error;
  ()

let is_app = function
  | Exp.App _ -> true
  | _ -> false

let rec curry_abs e = function
  | [] -> e
  | id :: t -> Exp.Abs (id, curry_abs e t)

let parse_id tokens indent =
  let HasPos (tok, pos) = next_token tokens in
  match tok with
  | TVAR id when compare_indent pos indent -> Ident.ident id
  | _ -> Format.sprintf "Expected id, but received %s" (show tok) |> error

let rec insert e = function
  | Exp.App (l, r) -> Exp.App (insert e l, r)
  | l -> Exp.App (e, l)

let rec parse_exp_0_head tokens indent =
  let next = next_token tokens in
  let HasPos (tok, pos) = next in
  match tok with
  | TINT i  when compare_indent pos indent -> Exp.Int i, pos
  | TUNIT  when compare_indent pos indent -> Exp.Unit, pos
  | TBOOL b when compare_indent pos indent -> Exp.Bool b, pos
  | TVAR i  when compare_indent pos indent -> Exp.Var (Ident.ident i), pos
  | TSTRING s when compare_indent pos indent -> Exp.String s, pos
  | TLPAREN when compare_indent pos indent ->
    let e = parse_exp tokens (SameLineOrGt pos) in
    let _ = expect_token TRPAREN tokens Any in
    e, pos
  | TDOLLAR when compare_indent pos indent ->
    let e = parse_exp tokens (SameLineOrGt pos) in
    e, pos
  | _ -> Format.sprintf "Expected an int, variable, or (, but received %s (at %s)"
        (show tok) (show_indent indent) |> error

and parse_tail l_pos tokens =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  (* Apply *)
  | TINT _ | TVAR _ | TBOOL _ | TDOLLAR | TLPAREN
    when compare_indent pos (SameLine l_pos) ->
    let tl, _ = parse_exp_0_head tokens (SameLine l_pos) in
    tl :: parse_tail pos tokens
  | _ -> []

and make fn = function
  | [] -> fn
  | h ::  [] -> Exp.App (fn, h)
  | h :: t -> App (make fn t, h)

and parse_exp_0 tokens indent =
  let hd, l_pos = parse_exp_0_head tokens indent in
  let rst = parse_tail l_pos tokens in
  make hd (List.rev rst), l_pos

and parse_args tokens indent delim =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TVAR id when compare_indent pos indent  ->
    consume_token tokens;
    Ident.ident id :: parse_args tokens indent delim
  | t when t = delim -> []
  | _ -> "Error parsing function arguments. Expected identifiers." |> error

and parse_exp_1 tokens indent =
  let hd, l_pos = parse_exp_0 tokens indent in
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TMUL when compare_indent pos (SameLine l_pos) ->
    consume_token tokens;
    let tl, _ = parse_exp_1 tokens (SameLine l_pos) in
    Exp.Binop (Mul, hd, tl), pos
  | TDIV when compare_indent pos (SameLine l_pos) ->
    consume_token tokens;
    let tl, _ = parse_exp_1 tokens (SameLine l_pos) in
    Exp.Binop (Div, hd, tl), pos
  | _ -> hd, pos

and parse_exp_2 tokens indent =
  let hd, l_pos = parse_exp_1 tokens indent in
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TADD when compare_indent pos (SameLine l_pos) ->
    consume_token tokens;
    let tl, _ = parse_exp_2 tokens (SameLine l_pos) in
    Exp.Binop (Add, hd, tl), pos
  | TSUB when compare_indent pos (SameLine l_pos) ->
    consume_token tokens;
    let tl, _ = parse_exp_2 tokens (SameLine l_pos) in
    Exp.Binop (Sub, hd, tl), pos
  | _ -> hd, pos

and parse_exp_3 tokens indent =
  let hd, l_pos = parse_exp_2 tokens indent in
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TEQ when compare_indent pos (SameLine l_pos) ->
    consume_token tokens;
    let tl = parse_exp_3 tokens (SameLine l_pos) in
    Exp.Binop (Eq, hd, tl)
  | _ -> hd

and parse_exp_4 tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TFUN when compare_indent pos indent ->
    consume_token tokens;
    let args = parse_args tokens (SameLine pos) TARROW in
    expect_token TARROW tokens (SameLine pos);
    let e = parse_exp tokens (SameLineOrGt pos) in
    curry_abs e args
  | _ -> parse_exp_3 tokens indent

and parse_exp_5 tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TIF when compare_indent pos indent ->
    consume_token tokens;
    let cnd = parse_exp tokens (SameLine pos) in
    let thn = parse_exp tokens (NextLineGtCol pos) in
    expect_token TELSE tokens (NextLineEqCol pos);
    let els = parse_exp tokens (NextLineGtCol pos) in
    Exp.If (cnd, thn, els)
  | _ -> parse_exp_4 tokens indent

and parse_exp tokens indent =
  let HasPos (tok, pos) = peek_token tokens in
  match tok with
  | TLET when compare_indent pos indent ->
    consume_token tokens;
    let ids = parse_args tokens (SameLine pos) TEQ in
    expect_token TEQ tokens (SameLine pos);
    let id = List.hd ids in
    let args = List.tl ids in
    let e_indent = if List.length ids > 1 then (SameLineOrGt pos) else (SameLine pos) in
    let e = parse_exp tokens e_indent in
    let b = parse_exp tokens (NextLineEqCol pos) in
    let res = List.fold_left (fun acc id -> Exp.Abs (id, acc)) e (List.rev args) in
    App (Abs (id, b), res)
  | TREC when compare_indent pos indent ->
    consume_token tokens;
    let ids = parse_args tokens (SameLine pos) TEQ in
    expect_token TEQ tokens (SameLine pos);
    let id = List.hd ids in
    let args = List.tl ids in
    let e_indent = if List.length ids > 1 then (SameLineOrGt pos) else (SameLine pos) in
    let e = parse_exp tokens e_indent in
    let b = parse_exp tokens (NextLineEqCol pos) in
    let res = List.fold_left (fun acc id -> Exp.Abs (id, acc)) e (List.rev args) in
    App (Abs (id, b), Fix (id, res))
  | _ -> parse_exp_5 tokens indent

let rec curry exp =
  let open Exp in
  match exp with
  | FApp es -> curry @@ List.fold_left (fun e acc -> App (e, acc)) (List.hd es) (List.tl es)
  | Fix (id, e) -> Fix (id, curry e)
  (* Turn show into unary operator *)
  | App (Var ("show", 0), r) -> Unop (Show, curry r)
  | App (l, r) -> App (curry l, curry r)
  | Abs (id, e) -> Abs (id, curry e)
  | HasType (e, t) -> HasType (curry e, t)
  | If (c, t, e) -> If (curry c, curry t, curry e)
  | Let (id, e, b) -> Let (id, curry e, curry b)
  | Binop (op, l, r) -> Binop (op, curry l, curry r)
  | Unop (op, e) -> Unop (op, curry e)
  | Clos (e, env) -> Clos (curry e, env)
  | Int i -> Int i
  | Bool b -> Bool b
  | String s -> String s
  | Var id -> Var id
  | Unit -> Unit

let parse_tokens tokens =
  let e = parse_exp (ref tokens) Any in
  (* print_endline @@ "parse: " ^ Exp.show e; *)
  curry e

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
let%test "parse app parens" =
  let x = Ident.ident "x" in
  test "x (5) 3" = Exp.App (App (Var x, Int 5), Int 3)
let%test "parse application" =
  let x = Ident.ident "x" in
  let fac = Ident.ident "factorial" in
  let mul = Ident.ident "mul" in
  let sub = Ident.ident "sub" in
  let actual = test "mul x (factorial (sub x 1))" in
  let expected = Exp.App (App (Var mul, Var x), App (Var fac, App (App (Var sub, Var x), Int 1))) in
  actual = expected
let%test "parse add" =
  let add = Exp.Var (Ident.ident "add") in
  let actual = test "(add 1 (add 2 3))" in
  let expected = Exp.App (Exp.App (add, Int 1), Exp.App (Exp.App (add, Int 2), Int 3)) in
  actual = expected
let%test "parse add" =
  test "4 + 5 + 9" = Exp.Binop (Add, Int 4, Binop (Add, Int 5, Int 9))
let%test "parse sub" =
  test "4 + 5 - 9" = Exp.Binop (Add, Int 4, Binop (Sub, Int 5, Int 9))
let%test "parse mul" =
  let actual = test "5 * 3 + 2" in
  actual = Exp.Binop (Add, Binop (Mul, Int 5, Int 3), Int 2)
let%test "parse eq" =
  let actual = test "2 + 2 = 2 * 2" in
  actual = Exp.Binop (Eq, Binop (Add, Int 2, Int 2), Binop (Mul, Int 2, Int 2))
