open Ast
open Parsing
open Uniquify
open Typecheck
open Eval

let use_lazy = ref false
let filename = ref ""

(* Use list *)
let set_filename arg = filename := arg

let usage = Format.sprintf "Usage: %s [-l] <filename>" Sys.argv.(0)

let specs = [
  "-l", Arg.Set use_lazy, ": Use lazy evaluation";
  "", Arg.Set_string filename, ": Filename";
]

let work file = try
  let tokens = lex_file file in
  let ast = parse_tokens tokens in
  (* print_endline @@ "Parsed: " ^ (Exp.show ast |> Chalk.light_green |> Chalk.bold); *)
  let uniq = uniquify ast in
  (* print_endline @@ "Uniquify: " ^ (Exp.show uniq |> Chalk.light_green |> Chalk.bold); *)
  let ty = synth uniq in
  let res = if !use_lazy then eval_lazy uniq else eval_strict uniq in
  print_endline @@ Exp.show res ^ " : " ^ (Type.show ty)
  with
  | UniquifyError msg  -> print_endline (msg |> Chalk.bold |> Chalk.red)
  | TypecheckError msg -> print_endline (msg |> Chalk.bold |> Chalk.red)
  | EvalLazyError msg -> print_endline (msg |> Chalk.bold |> Chalk.red)

let () =
  Arg.parse specs set_filename usage;
  !filename |> work

