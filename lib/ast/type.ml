open Ident

module Type = struct
  type t =
  (* Referred to as type 1 *)
  | TyUnit
  | TyInt
  | TyBool
  | TyString
  (* Referred to as α *)
  | TyVar of Ident.t
  | TyExist of Ident.t
  (* Referred to as ∀α. A *)
  | TyForAll of Ident.t * t
  (* Reffered to as A → B *)
  | TyFun of t * t

  let rec show = function
  | TyUnit -> "()"
  | TyInt -> "Int"
  | TyBool -> "Bool"
  | TyString -> "String"
  | TyVar id -> "'" ^ Ident.show id
  | TyExist id -> "∃'" ^ Ident.show id
  | TyForAll (id, t) -> Format.sprintf "∀%s. %s" (Ident.show id) (show t)
  | TyFun (l, r) -> Format.sprintf "%s → %s" (show l) (show r)

  let rec is_monotype = function
  | TyForAll _ -> false
  | TyFun (l, r) -> is_monotype l && is_monotype r
  | _ -> true
end
