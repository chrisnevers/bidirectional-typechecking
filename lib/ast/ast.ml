include Ident
include Type
include Context
include Token


module Op = struct
  type t = Add | Sub | Mul | Div | Eq | Show

  let show = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq  -> "="
  | Show -> "show"
end

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
  | Binop of Op.t * t * t
  | Unop of Op.t * t
  | Fix of Ident.t * t
  | Clos of t * e

  and e =
  | EMt
  | EClo of (t, t) Hashtbl.t

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
  | Let (id, e, b) -> Format.sprintf "let %s = %s in %s" (Ident.show id) (show e) (show b)
  | Binop (op, l, r) -> Format.sprintf "(%s %s %s)" (show l) (Op.show op) (show r)
  | Fix (id, e) -> Format.sprintf "fix %s . %s" (Ident.show id) (show e)
  | Clos (e, _) -> Format.sprintf "%s" (show e)
  | Unop (op, e) -> Format.sprintf "%s %s" (Op.show op) (show e)

  and show_e = function
  | EMt -> "∘"
  | EClo _ -> "{ }"
          (* ^ Hashtbl.fold (fun k v acc -> acc ^ Format.sprintf
              "(%s = %s)" (show k) (show v)) c "" ^
               "}" *)

end

