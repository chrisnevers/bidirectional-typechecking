open Ast
open Parsing
open Uniquify
open Typecheck
open Eval

let work file = try
  let tokens = lex_file file in
  let ast = parse_tokens tokens in
  (* print_endline @@ "Parsed: " ^ (Exp.show ast |> Chalk.light_green |> Chalk.bold); *)
  let uniq = uniquify ast in
  (* print_endline @@ "Uniquify: " ^ (Exp.show uniq |> Chalk.light_green |> Chalk.bold); *)
  let ty = synth uniq in
  let res = eval uniq in
  print_endline @@ Exp.show res ^ " : " ^ (Type.show ty)
  with
  | UniquifyError msg  -> print_endline (msg |> Chalk.bold |> Chalk.red)
  | TypecheckError msg -> print_endline (msg |> Chalk.bold |> Chalk.red)

let () = Sys.argv.(1) |> work

