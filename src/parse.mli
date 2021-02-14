type op =
  | Plus
  | Minus
  | Times

type form =
  | Int of int64
  | Op of op * form list

val parse: string -> form list
