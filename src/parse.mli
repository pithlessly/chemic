type form =
  | Int of int64
  | String of string
  | Ident of string
  | List of form list

val parse: string -> form list
