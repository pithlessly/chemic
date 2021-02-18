(* Functionality for translating parsed forms into ASTs and collecting other
 * information into symbol tables, etc. in the process. *)

(* stores globally relevant information created while parsing *)
type global_ctx
(* stores information created while parsing that is relevant to only one procedure *)
type local_ctx

(* a token identifying a global variable *)
type global_var_id
(* a token identifying a local variable *)
type local_var_id

type var_id =
  | Global of global_var_id
  | Local of local_var_id

(* a token identifying a string literal *)
type string_id
(* a token identifying a procedure literal *)
type proc_id

type op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons
  | Call

type expr =
  | Int of int64
  | String of string_id
  | Var of var_id
  | Define of var_id * expr
  | Proc of proc_id
  | Builtin of op * expr list

val build: Parse.form list -> global_ctx * expr list

val write_string: string_id -> Writer.t
val write_proc: proc_id -> Writer.t
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
val write_ctx:
  global_ctx ->
  write_proc_body:(write_final:Writer.t -> expr list -> Writer.t) ->
  Writer.t * Writer.t
