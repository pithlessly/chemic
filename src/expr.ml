type form = Parse.form =
  | Int of int64
  | String of string
  | Ident of string
  | List of form list

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

let get_op = function
  | Ident "+" -> Plus
  | Ident "-" -> Minus
  | Ident "*" -> Times
  | Ident "len" -> Len
  | Ident "display" -> Print
  | Ident "cons" -> Cons
  | Ident s ->
    raise (Invalid_argument (Printf.sprintf "invalid operator: \"%s\""
                               (String.escaped s)))
  | _ ->
    raise (Invalid_argument "non-identifier in function position")

let rec build = function
  | List [Ident "define"; Ident i; expr] ->
    Define (i, build expr)

  | List (f :: args) ->
    Builtin (get_op f, List.map build args)

  | List [] ->
    raise (Invalid_argument "nil cannot be evaluated")

  | Ident i -> Var i
  | Int i -> Int i
  | String s -> String s
