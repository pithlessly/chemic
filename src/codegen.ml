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

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given expression and assigning the return value
 * to the given register. *)
let write_expr ~write_access_var ~rctx =
  let rec go ~reg = function
    | Int i -> fun buf ->
      bprintf buf "MAKE_INT(%t,%Ld);" (Register.write reg) i

    | String id ->
      let str = Expr.write_access_string id in
      fun buf -> bprintf buf "MAKE_STRING(%t,%t);" (Register.write reg) str

    | Var id ->
      fun buf -> bprintf buf "%t=%t;"
          (Register.write reg)
          (write_access_var id)

    | OperatorArg op ->
      fun buf ->
        bprintf buf "MAKE_PROC(%t,&operator_%s);"
          (Register.write reg)
          op.proc_ident

    | Define (id, x) ->
      let expr = go ~reg x in
      let var = write_access_var id in
      fun buf ->
        bprintf buf "%t%t=%t;MAKE_NIL(%t);"
          expr
          var
          (Register.write reg)
          (Register.write reg)

    | Let { lhs; rhs; body } ->
      let rhs = go ~reg rhs in
      let var = write_access_var (Expr.Local lhs) in
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
  (* create separate sequences of incrementing IDs for boxed
   * and unboxed variables *)
  let num_boxed = ref 0 in
  let num_unboxed = ref 0 in
  let boxed_unboxed_idxs: [`Boxed of int | `Unboxed of int] array =
    local.locals |> Array.map (fun (var: Expr.var_metadata) ->
        if var.boxed then
          let id = !num_boxed in
          num_boxed := id + 1;
          `Boxed id
        else
          let id = !num_unboxed in
          num_unboxed := id + 1;
          `Unboxed id
      )
  in

  (* generate writers for every expr in the function body *)
  let rctx = Register.init () in
  let body =
    (* convert an `Expr.var_id` to an lvalue *)
    let write_access_var = function
      | Expr.Global id -> fun buf -> bprintf buf "g[%d]" id
      | Expr.Local id ->
        match boxed_unboxed_idxs.(id) with
        | `Boxed id ->
          fun buf -> bprintf buf "ENV_LOCAL(e,%d)" id
        | `Unboxed id ->
          fun buf -> bprintf buf "r[%d]" id
    in
    List.map (write_expr ~write_access_var ~rctx) local.body
  in

  (* the `r` array will hold all unboxed variables, followed
   * by temporaries needed for expression evaluation *)
  let num_temps = Register.allocated rctx in
  let num_registers = !num_unboxed + num_temps in

  (* the `e` array will hold all boxed variables *)
  let do_alloc_e = !num_boxed > 0 in

  fun buf ->
    bprintf buf "  Obj r[%d];" num_registers;
    if do_alloc_e then
      (* note that this can heap allocate and trigger GC *)
      bprintf buf "ENV_INIT(e,%d);" !num_boxed;

    (* offset into the register array where temporaries begin *)
    bprintf buf "const size_t REG=%d;\n  " !num_unboxed;

    (* emit initialization of local variables to their correct values *)
    local.locals |> Array.iteri (fun i (meta: Expr.var_metadata) ->
        let init_expr =
          match meta.source with
          | `Param -> "UNSAFE_NEXT_ARG"
          | `Internal -> "NIL"
        in
        match boxed_unboxed_idxs.(i) with
        | `Boxed id ->
          bprintf buf "ENV_LOCAL(e,%d)=%s;" id init_expr
        | `Unboxed id ->
          bprintf buf "r[%d]=%s;" id init_expr
      );

    (* emit initialization of temporaries to nil *)
    for i = 0 to num_temps - 1 do
      bprintf buf "MAKE_NIL(r[REG+%d]);" i
    done;

    (* notify the GC to track `r` *)
    bprintf buf "\n  gc_push_roots(r,%d);" num_registers;
    if do_alloc_e then
      (* notify the GC to track `e` *)
      bprintf buf "\n  gc_push_roots(ENV_ROOTS(e),%d);" !num_boxed;

    (* emit the function body contents *)
    List.iter (bprintf buf "\n  %t") body;

    if do_alloc_e then
      (* notify the GC to stop tracking `e` as roots *)
      bprintf buf "\n  gc_pop_roots();";
    (* notify the GC to stop tracking `r` as roots *)
    bprintf buf "\n  gc_pop_roots();\n"

(* Write a function implementing the body of a procedure *)
let write_proc (proc: Expr.proc_writers) =
  let local = write_local proc.local in
  fun buf ->
    bprintf buf "static Obj %t(void) {\n" proc.name;
    bprintf buf "  UNSAFE_EXPECT_ARGS(%d);\n" proc.num_params;
    local buf;
    bprintf buf "  return r[REG];\n}\n"

(* Write main(), implementing the top-level procedure *)
let write_main (top_level: Expr.local_data) =
  let local = write_local top_level in
  fun buf ->
    Buffer.add_string buf "int main(void) {\n";
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
