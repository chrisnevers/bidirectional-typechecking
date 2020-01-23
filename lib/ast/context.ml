open Ident
open Type

module Context = struct
  type kind =
  | VarDecl of Ident.t
  | ExistDecl of Ident.t
  | Marker of Ident.t
  | Solved of Ident.t * Type.t
  | TypedVar of Ident.t * Type.t

  type t = kind list

  let show_kind = function
  | VarDecl id -> Ident.show id
  | ExistDecl id -> Ident.show id ^ "^"
  | Marker id -> "<|" ^ Ident.show id
  | Solved (id, t) -> Format.sprintf "%s^ = %s" (Ident.show id) (Type.show t)
  | TypedVar (id, t) -> Format.sprintf "%s : %s" (Ident.show id) (Type.show t)

  let rec show = function
  | [] -> "·"
  | h :: t -> show_kind h ^ ", " ^ show t

  let rec get_solved alpha = function
  | [] -> None
  | Solved (h, t) :: _ when h = alpha -> Some t
  | _ :: t -> get_solved alpha t

  let rec has_existential alpha = function
  | [] -> false
  | ExistDecl h :: _ when h = alpha -> true
  | _ :: t -> has_existential alpha t

  let rec has_variable alpha = function
  | [] -> false
  | VarDecl h :: _ when h = alpha -> true
  | _ :: t -> has_variable alpha t

  let rec get_annotation alpha = function
  | [] -> None
  | TypedVar (h, t) :: _ when h = alpha -> Some t
  | _ :: t -> get_annotation alpha t

  let rec insert el inserts = function
  | [] -> []
  | h :: t when h = el -> h :: inserts @ t
  | h :: t -> h :: insert el inserts t

  let split_at el l =
    let rec aux el = function
    | [] -> [], []
    | h :: t ->
      match el with
      | None -> let _, rst_r = aux None t in [], h :: rst_r
      | Some id when id = h -> let _, rst_r = aux None t in [h], rst_r
      | _ -> let rst_l, rst_r = aux el t in h :: rst_l, rst_r
    in
    aux (Some el) l

  let drop el l = List.filter (fun e -> e != el) l

end
