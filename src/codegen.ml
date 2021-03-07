type op = Expr.op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons
  | Call

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
  (* write any declarations necessary for the registers to be used *)
  val write_ctx: ctx -> Writer.t
  (* write as a C identifier *)
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

  let write r buf = bprintf buf "r%d" r

  let write_ctx ctx =
    if !ctx > 0 then
      (* "Obj r0,r1,r2..." *)
      let reg_list = Writer.join ',' (Utils.seq_init !ctx write) in
      fun buf -> bprintf buf "  Obj %t;\n" reg_list
    else
      Writer.empty
end

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
      let prelude, ident = Expr.write_access_var id in
      fun buf -> bprintf buf "%t%t=%t;" prelude (Register.write reg) ident

    | Define (id, x) ->
      let expr = go ~reg x in
      let prelude, ident = Expr.write_assign_var id in
      fun buf ->
        expr buf;
        prelude buf;
        bprintf buf "%t=%t;MAKE_NIL(%t);"
          ident (Register.write reg) (Register.write reg)

    | Let { lhs; rhs; body } ->
      let rhs = go ~reg rhs in
      let ident, deinit = Expr.write_let_var lhs in
      let body = List.map (go ~reg) body in
      fun buf ->
        rhs buf;
        bprintf buf "%t=%t;" ident (Register.write reg);
        let rec loop = function
          | [] ->
            raise (Invalid_argument "let block is empty")
          | [final] ->
            final buf
          | expr :: rest ->
            expr buf;
            bprintf buf "deinit(%t);" (Register.write reg);
            loop rest
        in
        loop body;
        deinit buf

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
      (* TODO: evaluation order is unspecified according to the standard,
       * but maybe it's still a little strange that the function argument
       * is only evaluated after all the others? *)
      let n_args = List.length args in
      let args = List.map (go ~reg) args in
      fun buf ->
        bprintf buf "arg_init(%d);" n_args;
        args |> List.iter (fun write_compute_arg ->
            write_compute_arg buf;
            bprintf buf "arg_push(%t);" (Register.write reg)
          );
        unary_fun ~reg ~name:"call" f buf

    | Builtin (Minus, [])
    | Builtin (Len, _)
    | Builtin (Print, _)
    | Builtin (Cons, _)
    | Builtin (Call, _) ->
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

(* Convert a list of expressions into a C program that evaluates each one in turn
 * and discards the results. *)
let gen_code (expr_data: Expr.global_writers) =
  let buf = Buffer.create 2048 in

  let write_proc_body (proc: Expr.proc_writers) =
    let rctx = Register.init () in
    let body = List.map (write_expr ~rctx) proc.local.body in
    bprintf buf "Obj %t() {\n" proc.name;
    bprintf buf "  UNSAFE_EXPECT_ARGS(%d);\n" proc.num_params;
    proc.local.before buf;
    Register.write_ctx rctx buf;
    let rec loop = function
      | [] ->
        raise (Invalid_argument "empty function")
      | [final] ->
        bprintf buf "  %t\n  %treturn %t;\n"
          final
          proc.local.after
          (Register.write (Register.zero rctx))
      | expr :: rest ->
        bprintf buf "  %tdeinit(%t);\n"
          expr
          (Register.write (Register.zero rctx));
        loop rest
    in
    loop body;
    Buffer.add_string buf "}\n";
  in

  let rctx = Register.init () in
  let top_level = List.map (write_expr ~rctx) expr_data.main.body in

  Buffer.add_string buf "#include \"chemic.h\"\n";
  expr_data.decls buf;
  expr_data.procs |> List.iter write_proc_body;
  Buffer.add_string buf "int main(){\n";
  expr_data.main.before buf;
  (* emit declaration of necessary registers *)
  Register.write_ctx rctx buf;
  (* emit program statements *)
  top_level |> List.iter (fun writer ->
      bprintf buf "  %tdeinit(%t);\n"
        writer
        (Register.write (Register.zero rctx))
    );
  Buffer.add_string buf "  ";
  expr_data.main.after buf;
  expr_data.more_after_main buf;
  Buffer.add_string buf "finalize();\n  return 0;\n}\n";
  Buffer.contents buf
