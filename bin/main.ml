open Ast
open Typecheck

let () =
  let open Exp in
  let x = Ident.ident "x" in
  let exp = App (Abs (x, Var x), Unit) in
  let ty = synth exp in
  print_endline @@ show exp ^ " : " ^ Type.show ty
