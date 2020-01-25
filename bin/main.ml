open Ast
open Parsing
open Typecheck
open Eval

let files = [
  (* "./examples/1_id.fs";
  "./examples/2_int.fs";
  "./examples/3_paren.fs";
  "./examples/4_app.fs";
  "./examples/5_fun.fs";
  "./examples/6_add.fs";
  "./examples/7_show.fs";
  "./examples/8_bool.fs";
  "./examples/9_let.fs"; *)
  (* "./examples/10_eq.fs"; *)
  "./examples/11_nested.fs";
]


let work file = try
  (* file |> Chalk.bold |> print_endline; *)
  let tokens = lex_file file in
  let ast = parse_tokens tokens in
  (* print_endline @@ "Parsed: " ^ (Exp.show ast |> Chalk.light_green |> Chalk.bold); *)
  let ty = synth ast in
  let res = eval ast in
  print_endline @@ Exp.show res ^ " : " ^ (Type.show ty)
  with
  | TypecheckError msg -> print_endline (msg |> Chalk.bold |> Chalk.red)

let () =
  work (Sys.argv.(1))

