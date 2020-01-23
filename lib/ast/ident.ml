open Gensym

module Ident = struct
  type t = string * int

  let show = function
    | name, 0 -> name
    | name, uid -> name ^ "/" ^ string_of_int uid

  let ident name = name, 0

  let gen name = name, Gensym.gen_int ()
end
