type expr = Expr.expr =
  | Int of int64
  | String of Expr.string_id
  | Var of Expr.var_id
  | OperatorArg of Operator.t
  | Define of Expr.var_id * expr
  | Let of { lhs: Expr.local_var_id; rhs: expr; body: expr list }
  | Lambda of Expr.proc_id
  | If of { condition: expr; true_case: expr; false_case: expr }
  | Call of expr * expr list
  | Operator of Operator.t * expr list

let bprintf = Printf.bprintf

module Register: sig
  (* tracks information related to generated registers *)
  type ctx
  type t

  (* create a new context *)
  val init: unit -> ctx
  (* the initial register associated with a context *)
  val zero: ctx -> t
  (* return the next register *)
  val next: ctx -> t -> t
  (* get the total number of registers currently being used *)
  val allocated: ctx -> int
  (* write as a C lvalue *)
  val write: t -> Writer.t
end = struct
  (* stores the highest register not yet used *)
  type ctx = int ref
  type t = int

  let init () = ref 0
  let next ctx r =
    ctx := max !ctx (r + 2);
    r + 1
  let zero ctx = next ctx (-1)

  let allocated ctx = !ctx
  let write r buf = bprintf buf "r[REG+%d]" r
end

let write_access_var ~is_boxed = function
  | Expr.Global id -> fun buf -> bprintf buf "g[%d]" id
  | Expr.Local id ->
    fun buf -> bprintf buf
        (if is_boxed id then "BOX_CONTENTS(r[%d])" else "r[%d]") id

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given expression and assigning the return value
 * to the given register. *)
let write_expr ~is_boxed ~rctx =
  let rec go ~reg = function
    | Int i -> fun buf ->
      bprintf buf "MAKE_INT(%t,%Ld);" (Register.write reg) i

    | String id ->
      let str = Expr.write_access_string id in
      fun buf -> bprintf buf "MAKE_STRING(%t,%t);" (Register.write reg) str

    | Var id ->
      fun buf -> bprintf buf "%t=%t;"
          (Register.write reg)
          (write_access_var ~is_boxed id)

    | OperatorArg op ->
      fun buf ->
        bprintf buf "MAKE_PROC(%t,&operator_%s);"
          (Register.write reg)
          op.proc_ident

    | Define (id, x) ->
      let expr = go ~reg x in
      let var = write_access_var ~is_boxed id in
      fun buf ->
        bprintf buf "%t%t=%t;MAKE_NIL(%t);"
          expr
          var
          (Register.write reg)
          (Register.write reg)

    | Let { lhs; rhs; body } ->
      let rhs = go ~reg rhs in
      let var = write_access_var ~is_boxed (Expr.Local lhs) in
      let body = List.map (go ~reg) body in
      fun buf ->
        rhs buf;
        bprintf buf "%t=%t;" var (Register.write reg);
        if Utils.null body then
          raise (Invalid_argument "let block is empty");
        body |> List.iter (bprintf buf "%t");
        bprintf buf "MAKE_NIL(%t);" var

    | Lambda id ->
      let proc = Expr.write_access_proc id in
      fun buf -> bprintf buf "MAKE_PROC(%t,%t);" (Register.write reg) proc

    | If { condition; true_case; false_case } ->
      let condition = go ~reg condition in
      let true_case = go ~reg true_case in
      let false_case = go ~reg false_case in
      fun buf ->
        condition buf;
        bprintf buf "if(IS_TRUTHY(%t)){%t}else{%t}"
          (Register.write reg)
          true_case
          false_case

    | Call (fn, args) ->
      (* construct writers for the function and its arguments, as well as
       * the registers into which they are stored *)

      let fn_writer = go ~reg fn in
      let ((args_writers: Writer.t list),
           (args_reg_writers: Writer.t list)) =

        let current_reg = ref reg in
        args |> Utils.unzip_with (fun expr ->
            let reg = Register.next rctx !current_reg in
            current_reg := reg;
            (go ~reg expr, Register.write reg)
          )
      in

      fun buf ->
        fn_writer buf;
        List.iter (fun w -> w buf) args_writers;
        bprintf buf "arg_init(%d);" (List.length args);
        List.iter (bprintf buf "arg_push(%t);") args_reg_writers;
        bprintf buf "%t=call(%t);" (Register.write reg) (Register.write reg)

    | Operator (op, args) ->
      (* check that the right number of arguments are being passed *)
      let num_args = List.length args in
      (match op.args with
       | Operator.Exactly n when num_args <> n ->
         raise (Invalid_argument (Printf.sprintf "operator takes %d arguments" n))
       | Operator.AtLeast n when num_args < n ->
         raise (Invalid_argument (Printf.sprintf "operator takes >= %d arguments" n))
       | _ -> ());

      (* construct a list of writers for the arguments and for
       * the registers into which the expressions are stored *)

      let ((args_writers: Writer.t list),
           (args_reg_writers: Writer.t list)) =

        let current_reg = ref (Lazy.from_val reg) in
        args |> Utils.unzip_with (fun expr ->
            let reg = Lazy.force !current_reg in
            (* only compute this on the next iteration to avoid
             * allocating one more register than we need *)
            current_reg := lazy (Register.next rctx reg);
            (go ~reg expr, Register.write reg)
          )
      in
      let impl_writer = op.impl ~args:args_reg_writers ~out:(Register.write reg) in
      fun buf ->
        List.iter (fun w -> w buf) args_writers;
        impl_writer buf

  in fun expr -> go ~reg:(Register.zero rctx) expr

(* Write the content of a function body which is common to both
 * procedures and the top level *)
let write_local (local: Expr.local_data) =
  let is_boxed id = local.locals.(id).boxed in
  let rctx = Register.init () in

  let body = List.map (write_expr ~is_boxed ~rctx) local.body in

  let num_locals = Array.length local.locals in
  let num_regs = Register.allocated rctx in
  let aux_size = num_locals + num_regs in

  fun buf ->
    bprintf buf "  Obj r[%d];const size_t REG=%d;\n  " aux_size num_locals;
    (* Emit initialization of vars representing local variables to their correct
     * values, and collect a list of all variables that need to be boxed. Boxes
     * can only be allocated after the call to `gc_push_roots` because `make_ref`
     * can allocate and trigger the GC, which needs to update every pointer. *)
    let boxed_vars =
      (* fold state: counter and list of boxed vars *)
      let init_state = (0, []) in
      local.locals
      |> Array.fold_left (fun (i, boxed_vars) (meta: Expr.var_metadata) ->
          bprintf buf "r[%d]=%s;" i
            (match meta.source with
             |`Param -> "UNSAFE_NEXT_ARG"
             | `Internal -> "NIL");
          (i + 1, if meta.boxed then i :: boxed_vars else boxed_vars)
        ) init_state
      |> snd |> List.rev
    in
    (* Emit initialization of vars representing registers to nil *)
    for i = num_locals to aux_size - 1 do
      bprintf buf "r[%d]=NIL;" i
    done;
    (* Emit function which notifies the GC to track these values *)
    bprintf buf "\n  gc_push_roots(r,%d);" aux_size;
    (* Emit allocation of cells for boxed locals *)
    boxed_vars |> List.iter (fun i -> bprintf buf "r[%d]=make_ref(r[%d]);" i i);
    (* Emit the actual function body contents *)
    List.iter (bprintf buf "\n  %t") body;
    (* Emit function which notifies the GC to stop tracking the registers *)
    Buffer.add_string buf "\n  gc_pop_roots();\n"

(* Write a function implementing the body of a procedure *)
let write_proc (proc: Expr.proc_writers) =
  let local = write_local proc.local in
  fun buf ->
    bprintf buf "static Obj %t() {\n" proc.name;
    bprintf buf "  UNSAFE_EXPECT_ARGS(%d);\n" proc.num_params;
    local buf;
    bprintf buf "  return r[REG];\n}\n"

(* Write main(), implementing the top-level procedure *)
let write_main (top_level: Expr.local_data) =
  let local = write_local top_level in
  fun buf ->
    Buffer.add_string buf "int main() {\n";
    Buffer.add_string buf "  initialize();\n";
    Buffer.add_string buf "  gc_push_roots(g,NUM_GLOBALS);\n";
    local buf;
    Buffer.add_string buf "  gc_pop_roots();\n";
    Buffer.add_string buf "  finalize();\n";
    Buffer.add_string buf "  return 0;\n";
    Buffer.add_string buf "}\n"

(* Convert a syntax tree into a C program that evaluates each
 * top-level expression. *)
let gen_code (expr_data: Expr.global_writers) =
  let main = write_main expr_data.main in

  let buf = Buffer.create 2048 in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  Buffer.add_string buf "#include \"operators.h\"\n";
  expr_data.decls buf;
  expr_data.procs |> List.iter (fun proc -> write_proc proc buf);
  main buf;

  Buffer.contents buf
