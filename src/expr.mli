(* Functionality for translating parsed forms into ASTs and collecting other
 * information into symbol tables, etc. in the process. *)

(* a token identifying a global variable *)
type global_var_id = int
(* a token identifying a local variable *)
type local_var_id = int

type var_id =
  | Global of global_var_id
  | Local of local_var_id

(* a token identifying a string literal *)
type string_id
(* a token identifying a procedure literal *)
type proc_id

type expr =
  | Int of int64
  | String of string_id
  | Var of var_id
  | OperatorArg of Operator.t
  | Define of var_id * expr
  | Let of { lhs: local_var_id; rhs: expr; body: expr list }
  | Lambda of proc_id
  | If of { condition: expr; true_case: expr; false_case: expr }
  | Call of expr * expr list
  | Operator of Operator.t * expr list

(* writer data common to both procedures and the top-level *)
type local_writers = {
  (* expressions to initialize local variables, to be placed
   * at the start of the procedure *)
  local_decls: string Seq.t;
  (* the length of `local_decls` *)
  num_decls: int;
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
  after_main: Writer.t;
}

val build: Parse.form list -> global_writers

val write_access_string: string_id -> Writer.t
val write_access_proc: proc_id -> Writer.t
