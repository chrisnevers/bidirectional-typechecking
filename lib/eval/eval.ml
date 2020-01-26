open Ast
(* open Stdlibrary *)

exception EvalError of string
let error msg = raise (EvalError ("Eval Error: " ^ msg))

let default_env =
  let env = Hashtbl.create 10 in
  (* Hashtbl.add env (Ident.ident "show") (Exp.Var ("show", 0)); *)
  env

let show_env env =
  print_endline "ENV start";
  Hashtbl.iter (fun k v ->
    Format.sprintf "%s = %s;" (Ident.show k) (Exp.show v) |> print_endline
  ) env;
  print_endline "ENV end"

let rec eval_exp env exp =
  let _rec =  eval_exp env in
  match exp with
  | Exp.Unit -> Exp.Unit
  | Exp.Int i -> Exp.Int i
  | Exp.Bool b -> Exp.Bool b
  | Exp.String s -> Exp.String s
  | Exp.Abs (id, e) -> Exp.Abs (id, e)
  | Exp.Var id when Hashtbl.mem env id -> Hashtbl.find env id
  | Exp.App (l, r) ->
    let l' = _rec l in
    let r' = _rec r in
    eval_fun env l' r'
  | Exp.If (c, t, e) ->
    begin
      match eval_exp env c with
      | Bool true  -> _rec t
      | Bool false -> _rec e
      | _ -> "Fatal: eval if" |> error
    end
  | Exp.Fix (_, e) -> e
  | Exp.Unop (Show, e) -> String (Exp.show @@ _rec e)
  | Exp.Binop (Add, l, r) ->
    begin match _rec l, _rec r with
    | Int l, Int r -> Int (l + r)
    | String l, String r -> String (l ^ r)
    | _ -> error ""
    end
  | Exp.Binop (Eq, l, r) -> Bool (_rec l = _rec r)
  | Exp.Binop (Sub, l, r) ->
    begin match _rec l, _rec r with
    | Int l, Int r -> Int (l - r)
    | _ -> error ""
    end
  | Exp.Binop (Mul, l, r) ->
    begin match _rec l, _rec r with
    | Int l, Int r -> Int (l * r)
    | _ -> error ""
    end
  | Exp.Binop (Div, l, r) ->
    begin match _rec l, _rec r with
    | Int l, Int r -> Int (l / r)
    | _ -> error ""
    end
  | _ -> "Unknown exp: " ^ Exp.show exp |> error

and eval_fun env fn arg =
  match fn with
  | Exp.Abs (id, e) ->
    Hashtbl.replace env id arg;
    eval_exp env e
  | _ -> "Unknown expression while applying: " ^ Exp.show fn |> error

let eval_strict exp =
  eval_exp default_env exp


type m = Ast.Exp.t
type e = Ast.Exp.e

type k =
  | Mt
  | Fn of m * e * k
  | Ar of m * k
  | Pr of Op.t * m list * m list * k
  | KIf of m * m * k

let rec show_k = function
  | Mt -> "⊥"
  | Fn (n, e, k) -> Format.sprintf "Fn (%s, %s, %s)" (Exp.show n) (Exp.show_e e) (show_k k)
  | Ar (v, k) -> Format.sprintf "Ar (%s, %s)" (Exp.show v) (show_k k)
  | Pr (op, vs, ns, k) -> Format.sprintf "op (%s, [%s], [%s], %s)" (Op.show op)
    (String.concat ", " @@ List.map Exp.show vs)
    (String.concat ", " @@ List.map Exp.show ns)
    (show_k k)
  | KIf (t, e, k) -> Format.sprintf "KIf (%s, %s, %s)" (Exp.show t) (Exp.show e) (show_k k)
type st =
  | ST of m * e * k

let show_st = function
  | ST (m, e, k) -> Format.sprintf "ST (%s, %s, %s)" (Exp.show m) (Exp.show_e e) (show_k k)

exception EvalLazyError of string
let lerror msg = raise @@ EvalLazyError ("Lazy Eval Error: " ^ msg)

open Ast.Exp

let is_v = function
  | Int _ | Bool _ | String _ | Unit | Abs _ | Clos _ -> true
  | _ -> false

let is_var = function
  | Var _ -> true
  | _ -> false

let get_clos = function
  | EMt -> Hashtbl.create 5
  | EClo c -> c

let rec get_clos_info = function
  | Clos (Abs (x, m), e') -> x, m, e'
  | Clos (e, e'') ->
    let x, m, _ = get_clos_info e in
    (* let clo = get_clos e' in *)
    (* Hashtbl.iter (fun k v -> Hashtbl.replace clo k v) (get_clos e''); *)
    x, m, e''
  | _ -> lerror ""

let is_clos = function
  | Clos _ -> true
  | _ -> false

let eval_lazy_exp st =
  (* print_endline @@ show_st st; *)
  match st with
  | ST (x, e, k) when is_var x ->
    ST (Hashtbl.find (get_clos e) x, e, k)
  | ST (Fix (id, m), e, k) ->
    let env' = get_clos e in
    Hashtbl.replace env' (Var id) m;
    ST (m, EClo env', k)
  | ST (If (c, t, el), e, k) ->
    ST (c, e, KIf (t, el, k))
  | ST (c, e, KIf (t, el, k)) when is_v c ->
    begin
      match c with
      | Bool true  -> ST (t, e, k)
      | Bool false -> ST (el, e, k)
      | _ -> lerror @@ "Not true or false: " ^ (Exp.show c)
    end
  | ST (App (m, n), e, k)      ->
    (* print_endline "2"; *)
    ST (m, e, Fn (n, e, k))
  | ST (v, e, Fn (n, e', k)) when is_v v ->
    (* print_endline "3"; *)
    ST (n, e', Ar (Clos (v, e), k))
  | ST (Abs (x, m), e, k)      ->
    (* print_endline "4"; *)
    ST (Clos (Abs (x, m), e), e, k)
  | ST (v, _, Ar (ar, k)) when is_v v && is_clos ar ->
    let x, m, e' = get_clos_info ar in
    (* print_endline "5"; *)
    let env' = get_clos e' in
    Hashtbl.replace env' (Var x) v;
    ST (m, EClo env', k)
  | ST (Binop (op, m, n), e, k)   ->
    (* print_endline "6"; *)
    ST (m, e, Pr (op, [], [n], k))
  | ST (Unop (op, m), e, k)   ->
    (* print_endline "6"; *)
    ST (m, e, Pr (op, [], [], k))
  | ST (v, e, Pr (o, u, m::n, k)) ->
    (* print_endline "7"; *)
    ST (m, e, Pr (o, u@[v], n, k))
  | ST (b, e, Pr (o, bs, [], k))  ->
    (* print_endline "8"; *)
    begin match o with
    | Eq  -> ST (Bool (b = List.hd bs), e, k)
    | Show  -> ST (String (Exp.show b), e, k)
    | Add ->
      begin match b with
      | Int r ->
        let l = match List.hd bs with Int i -> i | _ -> lerror "" in
        ST (Int (l + r), e, k)
      | String r ->
        let l = match List.hd bs with String i -> i | _ -> lerror "" in
        ST (String (l ^ r), e, k)
      | _ -> lerror ""
      end
    | Sub ->
      begin match b with
      | Int r ->
        let l = match List.hd bs with Int i -> i | _ -> lerror "" in
        ST (Int (l - r), e, k)
      | _ -> lerror ""
      end
    | Mul ->
      begin match b with
      | Int r ->
        let l = match List.hd bs with Int i -> i | _ -> lerror "" in
        ST (Int (l * r), e, k)
      | _ -> lerror ""
      end
    | Div ->
      begin match b with
      | Int r ->
        let l = match List.hd bs with Int i -> i | _ -> lerror "" in
        ST (Int (l / r), e, k)
      | _ -> lerror ""
      end
    (* | _ -> lerror "Unknown primitive operation" *)
    end
  | st -> Format.sprintf "Unknown state: %s " (show_st st) |> lerror

let rec eval_lazy_program = function
  | ST (v, _, Mt) when is_v v -> v
  | s -> eval_lazy_program (eval_lazy_exp s)

let eval_lazy e =
  eval_lazy_program @@ ST (e, EMt, Mt)