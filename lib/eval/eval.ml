open Ast
open Stdlibrary

exception EvalError of string
let error msg = raise (EvalError ("Eval Error: " ^ msg))

let default_env =
  let env = Hashtbl.create 10 in
  Hashtbl.add env (Ident.ident "add") std_add;
  Hashtbl.add env (Ident.ident "sub") std_sub;
  Hashtbl.add env (Ident.ident "mul") std_mul;
  Hashtbl.add env (Ident.ident "div") std_div;
  Hashtbl.add env (Ident.ident "show") std_show;
  Hashtbl.add env (Ident.ident "eq") std_eq;
  env

let rec eval_exp env exp =
  match exp with
  | Exp.Int i -> Exp.Int i
  | Exp.Bool b -> Exp.Bool b
  | Exp.String s -> Exp.String s
  | Exp.Abs (id, e) -> Exp.Abs (id, e)
  | Exp.Var id when Hashtbl.mem env id -> Hashtbl.find env id
  | Exp.App (l, r) ->
    let l' = eval_exp env l in
    let r' = eval_exp env r in
    eval_fun env l' r'
  | Exp.If (c, t, e) ->
    begin
      match eval_exp env c with
      | Bool true -> eval_exp env t
      | Bool false -> eval_exp env e
      | _ -> "Fatal: eval if" |> error
    end
  | _ -> "Unknown exp: " ^ Exp.show exp |> error

and eval_fun env fn arg =
  match fn with
  | Exp.Abs (id, e) ->
    let env' = Hashtbl.copy env in
    Hashtbl.add env' id arg;
    eval_exp env' e
  | Exp.Var id ->
    begin match Hashtbl.find_opt env id with
    | Some (Clos f) -> f arg
    | _ -> error ""
    end
  | Exp.Clos f -> f arg
  | _ -> "Unknown expression while applying: " ^ Exp.show fn |> error

let eval exp =
  eval_exp default_env exp
