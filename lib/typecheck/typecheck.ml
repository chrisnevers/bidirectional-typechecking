open Ast
open Gensym

let rec is_well_formed ctx ty =
  let open Type in
  match ty with
  | TyUnit | TyInt | TyBool | TyString -> true
  | TyVar id -> Context.has_variable id ctx
  | TyFun (l, r) -> is_well_formed ctx l && is_well_formed ctx r
  | TyForAll (id, t) -> is_well_formed (Context.VarDecl id :: ctx) t
  | TyExist id -> Context.has_existential id ctx ||
                  Context.get_solved id ctx != None

exception TypecheckError of string
let error msg = raise (TypecheckError msg)

let rec occurs_in alpha ty =
  let open Type in
  match ty with
  | TyUnit | TyInt | TyBool | TyString -> false
  | TyVar id -> id = alpha
  | TyFun (l, r) -> occurs_in alpha l || occurs_in alpha r
  | TyForAll (id, t) -> id = alpha || occurs_in alpha t
  | TyExist id -> id = alpha

let rec substitution a alpha b =
  let open Type in
  match a with
  | TyUnit -> TyUnit
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyString -> TyString
  | TyVar id when id = alpha -> b
  | TyVar _ -> a
  | TyForAll (id, _) when id = alpha -> TyForAll (id, b)
  | TyForAll (id, t) -> TyForAll (id, substitution t alpha b)
  | TyExist id when id = alpha -> b
  | TyExist _ -> a
  | TyFun (l, r) -> TyFun (substitution l alpha b, substitution r alpha b)

let rec apply_context a ctx =
  let open Type in
  match a with
  | TyUnit | TyInt | TyBool | TyVar _ | TyString -> a
  | TyExist alpha ->
    let tau = Context.get_solved alpha ctx in
    begin match tau with
    | None -> a
    | Some tau -> apply_context tau ctx
    end
  | TyFun (a, b) -> TyFun (apply_context a ctx, apply_context b ctx)
  | TyForAll (alpha, t) -> TyForAll (alpha, apply_context t ctx)

let rec check_against ctx exp ty =
  if is_well_formed ctx ty |> not then
    "check_against: type is not well formed" |> error;
  let open Exp in
  match exp, ty with
  | Unit, TyUnit
  | Int _, TyInt
  | Bool _, TyBool
  | String _, TyString -> ctx
  | Abs (x, e), TyFun (l, r) ->
    let typed_var = Context.TypedVar (x, l) in
    let ctx' = typed_var :: ctx in
    Context.drop typed_var @@ check_against ctx' e r
  | _, TyForAll (id, t) ->
    let var = Context.VarDecl id in
    let ctx' = var :: ctx in
    Context.drop var @@ check_against ctx' exp t
  | _, _ ->
    let a, theta = synthesizes_to ctx exp in
    subtype theta (apply_context a theta) (apply_context ty theta)

and subtype ctx a b =
  if is_well_formed ctx a |> not || is_well_formed ctx b |> not then
    "subtype: a | b is not well formed" |> error;
  let open Context in
  match a, b with
  | TyUnit, TyUnit
  | TyInt, TyInt
  | TyBool, TyBool
  | TyString, TyString -> ctx
  | TyVar alpha1, TyVar alpha2 when is_well_formed ctx a && alpha1 = alpha2 -> ctx
  | TyExist exist1, TyExist exist2 when exist1 = exist2 && is_well_formed ctx a -> ctx
  | TyFun (a1, a2), TyFun (b1, b2) ->
    let theta = subtype ctx a1 b1 in
    subtype theta (apply_context a2 theta) (apply_context b2 theta)
  | TyForAll (alpha, a), _ ->
    let r1 = Ident.gen "α" in
    let gamma = Marker r1 :: ExistDecl r1 :: ctx in
    let sub_a = substitution a alpha (Type.TyExist r1) in
    let delta = subtype gamma sub_a b in
    drop (Marker r1) delta
  | _, TyForAll (alpha, b) ->
    let theta = VarDecl alpha :: ctx in
    let delta = subtype theta a b in
    drop (VarDecl alpha) delta
  | TyExist alpha, _ when occurs_in alpha b |> not -> instantiate_l ctx alpha b
  | _, TyExist alpha when occurs_in alpha a |> not -> instantiate_r ctx a alpha
  | l, r -> Format.sprintf "Expected type %s ≤ %s" (Type.show l) (Type.show r)
            |> error

and synthesizes_to ctx exp =
  let open Type in
  (* print_endline @@ Context.show ctx ^ "\n"; *)
  match exp with
  | Unit -> TyUnit, ctx
  | Int _ -> TyInt, ctx
  | Bool _ -> TyBool, ctx
  | String _ -> TyString, ctx
  | Var id -> begin
    match Context.get_annotation id ctx with
    | Some t -> t, ctx
    | None -> Format.sprintf "synthesizes_to: %s not in context" (Ident.show id) |> error
    end
  | If (c, t, e) ->
    let ctx' = check_against ctx c TyBool in
    let c', ctx' = synthesizes_to ctx' c in
    let t', ctx' = synthesizes_to ctx' t in
    let e', ctx' = synthesizes_to ctx' e in
    if apply_context c' ctx != TyBool then
      "Expected if condition to be of type Bool, but received: " ^ show (apply_context c' ctx) ^ " in " ^ Context.show ctx' |> error;
    if t' != e' then
      "Expected if expression to have same type for if and else branch" |> error;
    t', ctx'
  | HasType (e, t) when is_well_formed ctx t ->
    let delta = check_against ctx e t in
    t, delta
  | Fix (id, e) ->
    let alpha = Ident.gen "α" in
    let beta  = Ident.gen "β" in
    let typed_var = Context.TypedVar (id, TyExist alpha) in
    let gamma = Context.ExistDecl alpha :: Context.ExistDecl beta ::
                typed_var :: ctx in
    let delta = check_against gamma e (TyExist beta) in
    TyExist alpha, Context.drop typed_var delta
  | Abs (id, e) ->
    let alpha = Ident.gen "α" in
    let beta  = Ident.gen "β" in
    let typed_var = Context.TypedVar (id, TyExist alpha) in
    let gamma = Context.ExistDecl alpha :: Context.ExistDecl beta ::
                typed_var :: ctx in
    let delta = check_against gamma e (TyExist beta) in
    TyFun (TyExist alpha, TyExist beta), Context.drop typed_var delta
  | App (l, r) ->
    let a, theta = synthesizes_to ctx l in
    application_synthesizes_to theta (apply_context a theta) r
  | _ -> "synthesizes_to: fail: " ^ (Exp.show exp) |> error

and application_synthesizes_to ctx ty exp =
  let open Type in
  let open Context in
  match ty with
  | TyExist id ->
    let alpha1 = Ident.gen "α" in
    let alpha2 = Ident.gen "α" in
    let gamma = insert (ExistDecl id) [
      ExistDecl alpha2;
      ExistDecl alpha1;
      Solved (id, TyFun (TyExist alpha1, TyExist alpha2))
    ] ctx
    in
    let delta = check_against gamma exp (TyExist alpha1) in
    TyExist alpha2, delta
  | TyForAll (id, t) ->
    let alpha = Ident.gen "α" in
    let gamma = ExistDecl alpha :: ctx in
    let sub_a = substitution t id (TyExist alpha) in
    application_synthesizes_to gamma sub_a exp
  | TyFun (l, r) -> r, check_against ctx exp l
  | _ -> "application_synthesizes_to: fail: " ^ Type.show ty |> error

and instantiate_l ctx alpha ty =
  let open Context in
  let l_ctx, r_ctx = split_at (ExistDecl alpha) ctx in
  let ctx =  if Type.is_monotype ty && is_well_formed l_ctx ty
              then insert (ExistDecl alpha) [Solved (alpha, ty)] ctx
              else ctx in
  match ty with
  | TyFun (a1, a2) ->
    let alpha1 = Ident.gen "α" in
    let alpha2 = Ident.gen "α" in
    let gamma = insert (ExistDecl alpha) [
      ExistDecl alpha2;
      ExistDecl alpha1;
      Solved (alpha, TyFun (TyExist alpha1, TyExist alpha2))
    ] ctx in
    let theta = instantiate_r gamma a1 alpha1 in
    let delta = instantiate_l theta alpha2 (apply_context a2 theta) in
    delta
  | TyForAll (beta, b) ->
    let delta = instantiate_l (VarDecl beta :: ctx) alpha b in
    drop (VarDecl beta) delta
  | TyExist beta when is_well_formed r_ctx ty ->
    insert (ExistDecl beta) [Solved (beta, TyExist alpha)] ctx
  | _ -> ctx

and instantiate_r ctx ty alpha =
  let open Context in
  let l_ctx, r_ctx = split_at (ExistDecl alpha) ctx in
  let ctx =  if Type.is_monotype ty && is_well_formed l_ctx ty
              then insert (ExistDecl alpha) [Solved (alpha, ty)] ctx
              else ctx in
  match ty with
  | TyFun (a1, a2) ->
    let alpha1 = Ident.gen "α" in
    let alpha2 = Ident.gen "α" in
    let gamma = ExistDecl alpha2 :: ExistDecl alpha1 ::
      Solved (alpha, TyFun (TyExist alpha1, TyExist alpha2)):: ctx in
    let theta = instantiate_l gamma alpha1 a1 in
    let delta = instantiate_r theta (apply_context a2 theta) alpha2 in
    delta
  | TyForAll (beta, b) ->
    let beta1 = Ident.gen "β" in
    let gamma = Marker beta1 :: ExistDecl beta1 :: ctx in
    let delta = instantiate_r gamma (substitution b beta (TyExist beta1)) alpha in
    drop (Marker beta1) delta
  | TyExist beta when is_well_formed r_ctx ty -> Solved (beta, TyExist alpha) :: ctx
  | _ -> ctx

let synth exp =
  let id = Ident.ident "α" in
  let t, c = synthesizes_to [
    TypedVar (Ident.ident "add", TyForAll (id, TyFun (TyVar id, TyFun (TyVar id, TyVar id))));
    TypedVar (Ident.ident "sub", TyFun (TyInt, TyFun (TyInt, TyInt)));
    TypedVar (Ident.ident "mul", TyFun (TyInt, TyFun (TyInt, TyInt)));
    TypedVar (Ident.ident "div", TyFun (TyInt, TyFun (TyInt, TyInt)));
    TypedVar (Ident.ident "show", TyForAll (id, TyFun (TyVar id, TyString)));
    TypedVar (Ident.ident "eq", TyForAll (id, TyFun (TyVar id, TyFun (TyVar id, TyBool))));
  ] exp in
  apply_context t c

let%test "unit" =
  let ty = synth Unit in
  ty = TyUnit

let%test "int" =
  let ty = synth (Int 5) in
  ty = TyInt

let%test "bool" =
  let ty = synth (Bool false) in
  ty = TyBool

let%test "abs" =
  Gensym.reset();
  let x = Ident.gen "x" in
  let ty = synth (Abs(x, Var x)) in
  let alpha = "α", 1 in
  ty = TyFun (TyExist alpha, TyExist alpha)

let%test "app unit" =
  Gensym.reset();
  let x = Ident.gen "x" in
  let ty = synth (App (Abs(x, Var x), Unit)) in
  ty = TyUnit

let%test "app int" =
  Gensym.reset();
  let x = Ident.gen "x" in
  let ty = synth (Abs(x, Int 5)) in
  let alpha = "α", 1 in
  ty = TyFun (TyExist alpha, TyInt)

let%test "app id" =
  Gensym.reset();
  let x = Ident.gen "x" in
  let ty = synth (App (Abs(x, Var x), Abs (x, Var x))) in
  let alpha = "α", 6 in
  ty = TyFun (TyExist alpha, TyExist alpha)
