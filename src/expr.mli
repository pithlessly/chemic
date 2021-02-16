type op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons

type expr =
  | Int of int64
  | String of string
  | Var of string
  | Define of string * expr
  | Builtin of op * expr list

val build: Parse.form -> expr
