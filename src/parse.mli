type op =
  | Plus
  | Minus
  | Times
  | Len
  | Print

type form =
  | Int of int64
  | String of string
  | Op of op * form list

val parse: string -> form list
