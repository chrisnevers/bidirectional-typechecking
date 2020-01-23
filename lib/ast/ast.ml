include Ident
include Type
include Context

module Exp = struct
  type t =
  | Var of Ident.t
  | Unit
  | Int of int
  | Bool of bool
  | Abs of Ident.t * t
  | App of t * t
  | HasType of t * Type.t

  let rec show = function
  | Var id -> Ident.show id
  | Unit -> "()"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Abs (id, e) -> Format.sprintf "λ %s . %s" (Ident.show id) (show e)
  | App (l, r) -> Format.sprintf "%s %s" (show l) (show r)
  | HasType (e, ty) -> Format.sprintf "%s : %s" (show e) (Type.show ty)

end

