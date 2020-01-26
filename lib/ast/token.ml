module Pos = struct
  type line   = int
  type column = int
  type t =
    | Pos of line * column

  let show = function
  | Pos (l, c) -> Format.sprintf "Pos (line=%s,col=%s)" (string_of_int l)
                  (string_of_int c)
end

module Token = struct

  type t =
    | TVAR of string
    | TINT of int
    | TBOOL of bool
    | TSTRING of string
    | TLPAREN
    | TRPAREN
    | TFUN
    | TARROW
    | TLET
    | TEQ
    | TIF
    | TELSE
    | TCOMMA
    | TREC
    | TDOLLAR
    | TADD
    | TSUB
    | TMUL
    | TDIV
    | TEOF

  type t_pos =
    | HasPos of t * Pos.t

  let show = function
  | TVAR i -> Format.sprintf "TVAR %s" i
  | TINT i -> Format.sprintf "TINT %s" (string_of_int i)
  | TBOOL b -> Format.sprintf "TBOOL %s" (string_of_bool b)
  | TSTRING s -> Format.sprintf "TSTRING %s" s
  | TLPAREN -> "TLPAREN"
  | TRPAREN -> "TRPAREN"
  | TFUN -> "TFUN"
  | TARROW -> "TARROW"
  | TLET -> "TLET"
  | TEQ -> "TEQ"
  | TIF -> "TIF"
  | TELSE -> "TELSE"
  | TCOMMA -> "TCOMMA"
  | TREC -> "TREC"
  | TDOLLAR -> "TDOLLAR"
  | TADD -> "TADD"
  | TSUB -> "TSUB"
  | TMUL -> "TMUL"
  | TDIV -> "TDIV"
  | TEOF -> "TEOF"

  let show_t_pos = function
    | HasPos (t, p) -> Format.sprintf "HasPos (%s, %s)" (show t) (Pos.show p)

end
