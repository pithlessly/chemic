(* Functionality for translating parsed forms into ASTs and collecting other
 * information into symbol tables, etc. in the process. *)

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
  | Let of { lhs: local_var_id; rhs: expr; body: expr list }
  | Lambda of proc_id
  | Builtin of op * expr list

(* writer data common to both procedures and the top-level *)
type local_writers = {
  (* declarations to be placed at the start of the procedure *)
  before: Writer.t;
  (* statements to run before returning from the procedure *)
  after: Writer.t;
  (* the body of the procedure *)
  body: expr list;
}

(* writer data needed for procedures *)
type proc_writers = {
  (* an identifier that can be used as the name of the procedure *)
  name: Writer.t;
  (* the number of parameters that the procedure takes *)
  num_params: int;
  local: local_writers;
}

type global_writers = {
  (* declarations to be placed before main() *)
  decls: Writer.t;
  (* information needed to define procedures *)
  procs: proc_writers list;
  (* information needed in main() *)
  main: local_writers;
  (* additional statements to be placed at the end of main() *)
  more_after_main: Writer.t;
}

val build: Parse.form list -> global_writers

val write_access_string: string_id -> Writer.t
val write_access_proc: proc_id -> Writer.t
val write_access_var: var_id -> Writer.t
