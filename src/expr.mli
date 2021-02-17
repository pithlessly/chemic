(* stores relevant information created while parsing *)
type ctx
(* a token identifying a variable *)
type var_id
(* a token identifying a string literal *)
type string_id

type op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons

type expr =
  | Int of int64
  | String of string_id
  | Var of var_id
  | Define of var_id * expr
  | Builtin of op * expr list

val build: Parse.form list -> ctx * expr list

val write_string: string_id -> Writer.t
(* return a pair of writers that emit:
 * - a series of statements
 * - an identifier that, after those statements have been executed,
 *   contains the desired variable
 *)
val write_access_var: var_id -> Writer.t * Writer.t
(* return a pair of writers that emit:
 * - a series of statements
 * - an identifier that, after those statements have been executed,
 *   can be assigned to in order to modify the desired variable
 *)
val write_assign_var: var_id -> Writer.t * Writer.t

(* return a pair of writers that emit statements that should be placed at the
 * start and end of the program to initialize and deinitialize the context *)
val write_ctx: ctx -> Writer.t * Writer.t
