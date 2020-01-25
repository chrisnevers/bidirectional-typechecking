include Ident
include Type
include Context
include Token

module Exp = struct
  type t =
  | Var of Ident.t
  | Unit
  | Int of int
  | Bool of bool
  | String of string
  | Abs of Ident.t * t
  | App of t * t
  | FApp of t list
  | HasType of t * Type.t
  | If of t * t * t
  | Let of Ident.t * t * t
  | Clos of (t -> t)

  let rec show = function
  | Var id -> Ident.show id
  | Unit -> "()"
  | Int i -> string_of_int i
  | String s -> s
  | Bool true -> "True"
  | Bool false -> "False"
  | Abs (id, e) -> Format.sprintf "(λ %s . %s)" (Ident.show id) (show e)
  | App (l, r) -> Format.sprintf "(%s %s)" (show l) (show r)
  | FApp es -> Format.sprintf "(%s)" (String.concat ", " (List.map show es))
  | HasType (e, ty) -> Format.sprintf "%s : %s" (show e) (Type.show ty)
  | If (cnd, thn, els) -> Format.sprintf "if %s then %s else %s" (show cnd) (show thn) (show els)
  | Let _ -> "let"
  | Clos _ -> "<closure>"

end

