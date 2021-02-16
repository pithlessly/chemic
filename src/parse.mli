type op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Define

type form =
  | Int of int64
  | String of string
  | Ident of string
  | Op of op * form list

val parse: string -> form list
