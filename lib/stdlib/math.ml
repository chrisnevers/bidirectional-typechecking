open Ast.Exp
open Exn

let std_add =
  Clos (fun a ->
  Clos (fun b ->
    match a, b with
    | Int a, Int b -> Int (a + b)
    | _ -> raise StdlibError))

let std_sub =
  Clos (fun a ->
  Clos (fun b ->
    match a, b with
    | Int a, Int b -> Int (a - b)
    | _ -> raise StdlibError))

let std_mul =
  Clos (fun a ->
  Clos (fun b ->
    match a, b with
    | Int a, Int b -> Int (a * b)
    | _ -> raise StdlibError))

let std_div =
  Clos (fun a ->
  Clos (fun b ->
    match a, b with
    | Int a, Int b -> Int (a / b)
    | _ -> raise StdlibError))

