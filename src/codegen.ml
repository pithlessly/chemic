type op = Expr.op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons
  | Call
  | Debug

type expr = Expr.expr =
  | Int of int64
  | String of Expr.string_id
  | Var of Expr.var_id
  | Define of Expr.var_id * expr
  | Let of { lhs: Expr.local_var_id; rhs: expr; body: expr list }
  | Lambda of Expr.proc_id
  | Builtin of op * expr list

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

let write_access_var = function
  | Expr.Global id -> fun buf -> bprintf buf "GLO%d" id
  | Expr.Local id -> fun buf -> bprintf buf "r[%d]" id

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given expression and assigning the return value
 * to the given register. *)
let write_expr ~rctx =
  let rec go ~reg = function
    | Int i -> fun buf ->
      bprintf buf "MAKE_INT(%t,%Ld);" (Register.write reg) i

    | String id ->
      let str = Expr.write_access_string id in
      fun buf -> bprintf buf "MAKE_STRING(%t,%t);" (Register.write reg) str

    | Var id ->
      fun buf -> bprintf buf "%t=%t;" (Register.write reg) (write_access_var id)

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

    | Builtin (Plus, []) -> fun buf ->
      bprintf buf "MAKE_INT(%t,0);" (Register.write reg)
    | Builtin (Plus, lhs :: rhss) ->
      write_fold_fun ~reg ~name:"add" lhs rhss

    | Builtin (Minus, [x]) ->
      unary_fun ~reg ~name:"neg" x
    | Builtin (Minus, lhs :: rhss) ->
      write_fold_fun ~reg ~name:"sub" lhs rhss

    | Builtin (Times, []) -> fun buf ->
      bprintf buf "MAKE_INT(%t,1);" (Register.write reg)
    | Builtin (Times, lhs :: rhss) ->
      write_fold_fun ~reg ~name:"mul" lhs rhss

    | Builtin (Len, [x]) ->
      unary_fun ~reg ~name:"len" x

    | Builtin (Print, [x]) ->
      let expr = go ~reg x in
      fun buf -> bprintf buf "%tdisplay(%t);" expr (Register.write reg)

    | Builtin (Cons, [a; b]) ->
      let r_rhs = Register.next rctx reg in
      binary_fun ~reg ~r_rhs ~name:"cons" (go ~reg a) b

    | Builtin (Call, f :: args) ->
      let write_fn = go ~reg f in
      let n_args = List.length args in
      let expr_reg = Register.next rctx reg in
      let args = List.map (go ~reg:expr_reg) args in
      fun buf ->
        write_fn buf;
        bprintf buf "arg_init(%d);" n_args;
        args |> List.iter (fun write_compute_arg ->
            write_compute_arg buf;
            bprintf buf "arg_push(%t);" (Register.write expr_reg)
          );
        bprintf buf "%t=call(%t);"
          (Register.write reg)
          (Register.write reg)

    | Builtin (Debug, [x]) ->
      let expr = go ~reg x in
      fun buf ->
        expr buf;
        Buffer.add_string buf "gc_debug();"

    | Builtin (Minus, [])
    | Builtin (Len, _)
    | Builtin (Print, _)
    | Builtin (Cons, _)
    | Builtin (Call, _)
    | Builtin (Debug, _) ->
      raise (Invalid_argument "invalid expression")

  (* Helper function to construct a writer which emits code to evaluate a function's
   * argument and then call it, assigning its result to the given register. *)
  and unary_fun ~reg ~name expr =
    let expr = go ~reg expr in
    fun buf ->
      bprintf buf "%t%t=%s(%t);"
        expr
        (Register.write reg)
        name
        (Register.write reg)

  (* Helper function which takes a writer emitting code to evaluate a function's first
   * argument to 'reg', then an expression to evaluate and store to 'r_rhs', and returns a
   * writer emiting code that evaluates a two-argument function and stores the result
   * to 'reg. *)
  and binary_fun ~reg ~r_rhs ~name lhs expr_rhs =
    let rhs = go ~reg:r_rhs expr_rhs in
    fun buf ->
      bprintf buf "%t%t%t=%s(%t,%t);"
        lhs
        rhs
        (Register.write reg)
        name
        (Register.write reg)
        (Register.write r_rhs)

  (* Helper function to construct a writer which emits code to evaluate a sequence
   * of arguments and combine them with repeated calls to a 2-argument function,
   * assigning the final result to the given register. *)
  and write_fold_fun ~reg ~name lhs rhss =
    let r_rhs = Register.next rctx reg in
    List.fold_left
      (fun lhs rhs -> binary_fun ~reg ~r_rhs ~name lhs rhs)
      (go ~reg lhs)
      rhss

  in fun expr -> go ~reg:(Register.zero rctx) expr

(* Write the content of a function body which is common to both
 * procedures and the top level *)
let write_local (local: Expr.local_writers) =
  let rctx = Register.init () in
  let body = List.map (write_expr ~rctx) local.body in
  let num_regs = Register.allocated rctx in
  let aux_size = local.num_decls + num_regs in

  fun buf ->
    bprintf buf "  Obj r[%d]={%t};\n"
      aux_size
      (Utils.seq_replicate num_regs "NIL"
       |> Seq.append local.local_decls (* NB - these are prepended, not appended *)
       |> Seq.map (fun s buf -> Buffer.add_string buf s)
       |> Writer.join ',');
    bprintf buf "  gc_push_roots(r,%d);\n" aux_size;
    bprintf buf "  const size_t REG=%d;\n"
      local.num_decls;
    List.iter (bprintf buf "  %t\n") body;
    Buffer.add_string buf "  gc_pop_roots();\n"

(* Write a function implementing the body of a procedure *)
let write_proc (proc: Expr.proc_writers) =
  let local = write_local proc.local in
  fun buf ->
    bprintf buf "Obj %t() {\n" proc.name;
    bprintf buf "  UNSAFE_EXPECT_ARGS(%d);\n" proc.num_params;
    local buf;
    bprintf buf "  return r[REG];\n}\n"

(* Write main(), implementing the top-level procedure *)
let write_main (top_level: Expr.local_writers) =
  let local = write_local top_level in
  fun buf ->
    Buffer.add_string buf "int main() {\n";
    local buf;
    Buffer.add_string buf "  finalize();\n  return 0;\n}\n"

(* Convert a syntax tree into a C program that evaluates each
 * top-level expression. *)
let gen_code (expr_data: Expr.global_writers) =
  let main = write_main expr_data.main in

  let buf = Buffer.create 2048 in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  expr_data.decls buf;
  expr_data.procs |> List.iter (fun proc -> write_proc proc buf);
  main buf;

  Buffer.contents buf
