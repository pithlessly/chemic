type op = Expr.op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons

type expr = Expr.expr =
  | Int of int64
  | String of Expr.string_id
  | Var of Expr.var_id
  | Define of Expr.var_id * expr
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
      fun buf -> bprintf buf "Obj %t;\n" reg_list
    else
      Writer.empty
end

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given form and assigning the return value
 * to the given register. *)
let write_form ~rctx =
  let rec go ~reg = function
    | Int i -> fun buf ->
      bprintf buf "MAKE_INT(%t,%Ld);" (Register.write reg) i

    | String id ->
      let str = Expr.write_string id in
      fun buf -> bprintf buf "MAKE_STRING(%t,%t);" (Register.write reg) str

    | Var id ->
      let prelude, ident = Expr.write_access_var id in
      fun buf -> bprintf buf "%t%t=%t;" prelude (Register.write reg) ident

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
      let form = go ~reg x in
      fun buf -> bprintf buf "%tdisplay(%t);" form (Register.write reg)

    | Define (id, x) ->
      let form = go ~reg x in
      let prelude, ident = Expr.write_assign_var id in
      fun buf ->
        bprintf buf "%t%t%t=%t;MAKE_NIL(%t);"
          form prelude ident (Register.write reg) (Register.write reg)

    | Builtin (Cons, [a; b]) ->
      let r_rhs = Register.next rctx reg in
      binary_fun ~reg ~r_rhs ~name:"cons" (go ~reg a) b

    | Builtin (Minus, [])
    | Builtin (Len, _)
    | Builtin (Print, _)
    | Builtin (Cons, _) ->
      raise (Invalid_argument "invalid expression")

  (* Helper function to construct a writer which emits code to evaluate a function's
   * argument and then call it, assigning its result to the given register. *)
  and unary_fun ~reg ~name form =
    let form = go ~reg form in
    fun buf ->
      bprintf buf "%t%t=%s(%t);"
        form
        (Register.write reg)
        name
        (Register.write reg)

  (* Helper function which takes a writer emitting code to evaluate a function's first
   * argument to 'reg', then a form to evaluate and store to 'r_rhs', and returns a
   * writer emiting code that evaluates a two-argument function and stores the result
   * to 'reg. *)
  and binary_fun ~reg ~r_rhs ~name lhs form_rhs =
    let rhs = go ~reg:r_rhs form_rhs in
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

  in go

(* Convert a form into a C program. *)
let gen_code ectx forms =
  let rctx = Register.init () in
  let buf = Buffer.create 2048 in
  (* compute each form, but do not write it yet
   * (to allow rctx to be updated) *)
  let form_writers = List.map (fun form ->
      let reg = Register.zero rctx in
      write_form ~rctx ~reg form
    ) forms in
  let init_globals, deinit_globals = Expr.write_ctx ectx in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  init_globals buf;
  Buffer.add_string buf "int main(){\n";
  (* emit declaration of necessary registers *)
  Register.write_ctx rctx buf;
  (* emit program statements *)
  form_writers |> List.iter (fun writer ->
      bprintf buf "%tdeinit(%t);\n"
        writer
        (Register.write (Register.zero rctx))
    );
  deinit_globals buf;
  Buffer.add_string buf "finalize();return 0;}\n";
  Buffer.contents buf
