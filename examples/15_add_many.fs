let addMany a b c d e f =
  add a (add b (add c (add d (add e f))))

let d = 5
let d = addMany 1 2 3 4 5 6
d
