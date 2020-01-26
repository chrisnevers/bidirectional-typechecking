open Hashtbl
open Ast.Exp

exception UniquifyError of string
let error msg = raise @@ UniquifyError ("Uniquify Error: " ^ msg)

let default_env =
  let env = create 10 in
  List.iter (fun id -> add env id 0) ["add"; "sub"; "mul"; "div"; "eq"; "show"];
  env

let lookup env (id, _) =
  match find_opt env id with
  | Some n -> id, n
  | None -> Format.sprintf "Identifier not bound: %s" id |> error

let uniquify_id env (id, _) =
  match find_opt env id with
  | Some n -> replace env id (n + 1); id, n + 1
  | None -> replace env id 0; id, 0

let rec uniquify_exp env exp =
  let _rec = uniquify_exp env in
  match exp with
  | Var id -> Var (lookup env id)
  | Abs (id, e) -> let id' = uniquify_id env id in Abs (id', _rec e)
  | App (l, r) -> let l' = _rec l in App (l', _rec r)
  | Fix (id, e) -> Fix (lookup env id, _rec e)
  | HasType (e, t) -> HasType (_rec e, t)
  | If (c, t, e) ->
    let c' = _rec c in
    let t' = _rec t in
    If (c', t', _rec e)
  | Let (id, e, b) ->
    let e' = _rec e in
    let id' = uniquify_id env id in
    Let (id', e', _rec b)
  | Unit -> Unit
  | Int i -> Int i
  | Bool b -> Bool b
  | String s -> String s
  | FApp _ | Clos _ ->
    Format.sprintf "Unexpected expression: %s" (show exp) |> error

let uniquify exp =
  uniquify_exp default_env exp
