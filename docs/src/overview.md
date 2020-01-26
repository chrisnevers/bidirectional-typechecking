# Overview

This language has a syntax like OCaml/F#. It's type system is based off of
[Complete and Easy Bidirectional Typechecking
for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf).
It is indentation sensitive.

# Supports

## Expressions

### Let

`let` expressions expect an expression on the right hand side and a body
expression (`in`) that is aligned directly under the `let` keyword.

```OCaml
let fiftyFour = 54
fiftyFour
```

### If Else

`if` `else` expressions must contain a condition expression of type bool, on the
same line as the `if` keyword. The `then` branch must be indented on the next
line more than the `if` keyword. The `else` keyword must be aligned to the same
column as the `if` keyword and it's branch expression must be indented more than
the `if` keyword.

```OCaml
if eq 4 4
  True
else
  False
```

### Functions

Functions are curried and may be written in a number of ways. The body expression
must be on the same line, or indented more than the `let` keyword on a subsequent
line. Functions are introduced using the `fun` keyword or by adding arguments to
a `let` assignment.

```OCaml
let add a b =
  add a b

let add = fun a b -> add a b

let add = fun a -> fun b -> add a b
```
