include Math
include Exn

open Ast.Exp

let std_show = Clos (fun a -> String (show a))

let std_eq = Clos (fun a -> Clos (fun b -> Bool (a = b)))
