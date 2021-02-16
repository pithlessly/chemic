type op = Expr.op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons

type expr = Expr.expr =
  | Int of int64
  | String of string
  | Var of string
  | Define of string * expr
  | Builtin of op * expr list

let bprintf = Printf.bprintf

(* Create a sequence of a given length by applying a function to the index. *)
let seq_init (n: int) (f: int -> 'a): 'a Seq.t =
  let rec loop idx () =
    if idx < n then
      let v = f idx in
      Seq.Cons (v, loop (idx + 1))
    else
      Seq.Nil
  in loop 0

(* Map a function over a sequence with access to the index. *)
let seq_mapi f seq =
  let rec loop i seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) -> Seq.Cons (f i x, loop (i + 1) xs)
  in loop 0 seq

(* Encode a string to the buffer as a C string literal *)
let write_string_literal s buf =
  let len = String.length s in
  let rec loop n =
    if n < len then (
      let c = String.get s n in
      if ' ' <= c && c <= '~' then (
        if c = '\\' || c = '"' then
          Buffer.add_char buf '\\';
        Buffer.add_char buf c
      ) else
        bprintf buf "\\x%02x" (Char.code c);
      loop (n + 1)
    )
  in
  Buffer.add_char buf '"';
  loop 0;
  Buffer.add_char buf '"'

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
      let reg_list = Writer.join ',' (seq_init !ctx write) in
      fun buf -> bprintf buf "Obj %t;\n" reg_list
    else
      Writer.empty
end

module Global: sig
  (* keeps track of global program information *)
  type ctx

  val init: unit -> ctx

  (* return a writer that emits an identifier that can be used to
   * access the given string *)
  val create_string: ctx -> string -> Writer.t

  (* return a pair of writers that emit:
   * - a series of statements
   * - an identifier that, after those statements have been executed,
   *   can be assigned to in order to modify the desired variable
   *)
  val assign_var: ctx -> string -> Writer.t * Writer.t

  (* return a pair of writers that emit:
   * - a series of statements
   * - an identifier that, after those statements have been executed,
   *   contains the desired variable
   *)
  val access_var: ctx -> string -> Writer.t * Writer.t

  (* return a pair of writers that emit statements that should be placed at the
   * start and end of the program to initialize and deinitialize the context *)
  val write_ctx: ctx -> Writer.t * Writer.t

end = struct
  module StringMap = Map.Make(String)

  type ctx = {
    mutable string_literals: string list;
    (* should be the same as 'List.length string_literals' *)
    mutable num_strings: int;
    mutable var_idxs: int StringMap.t;
  }

  let init () = {
    string_literals = [];
    num_strings = 0;
    var_idxs = StringMap.empty;
  }

  let create_string ctx s =
    let idx = ctx.num_strings in
    ctx.string_literals <- s :: ctx.string_literals;
    ctx.num_strings <- idx + 1;
    fun buf -> bprintf buf "&STR%d" idx

  let global_var idx = fun buf -> bprintf buf "GLO%d" idx

  let assign_var ctx name =
    (* NOTE: the naive approach used here of "don't deinitialize a variable
     * the first time it's been assigned to" doesn't work in the presence of
     * any nonlinear control flow (procedures, conditionals, loops, etc.) *)
    match StringMap.find_opt name ctx.var_idxs with
    | Some idx ->
      let var = global_var idx in
      ((fun buf -> bprintf buf "deinit(%t);" var), var)
    | None ->
      let new_idx = StringMap.cardinal ctx.var_idxs in
      ctx.var_idxs <- StringMap.add name new_idx ctx.var_idxs;
      (Writer.empty, global_var new_idx)

  let access_var ctx name =
    match StringMap.find_opt name ctx.var_idxs with
    | Some idx ->
      let var = global_var idx in
      ((fun buf -> bprintf buf "clone(%t);" var), var)
    | None ->
      raise (Invalid_argument (Printf.sprintf "undefined variable: '%s'" name))

  let write_ctx ctx =

    (* declarations of strings
     * (e.g. 'Str STR0={...},STR1={...},...;') *)
    let string_decls =
      if ctx.num_strings = 0 then
        Writer.empty
      else
        let decls =
          List.rev ctx.string_literals
          |> List.to_seq
          |> seq_mapi (fun i lit ->
              fun buf ->
                bprintf buf "STR%d={%d,0,%t}"
                  i (* len *)
                  (String.length lit) (* ref_count *)
                  (write_string_literal lit) (* data *)
            )
          |> Writer.join ','
        in
        fun buf -> bprintf buf "Str %t;\n" decls
    in

    let num_vars = StringMap.cardinal ctx.var_idxs in
    (* declarations of variables *)
    let var_decls =
      if num_vars = 0 then
        Writer.empty
      else
        let decls = Writer.join ',' (seq_init num_vars global_var) in
        fun buf -> bprintf buf "Obj %t;\n" decls
    in

    let deinits =
      fun buf ->
        for i = 0 to num_vars - 1 do
          bprintf buf "deinit(%t);" (global_var i);
        done
    in
        
    ((fun buf -> string_decls buf; var_decls buf), deinits)
end

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given form and assigning the return value
 * to the given register. *)
let write_form ~gctx ~rctx =
  let rec go ~reg = function
    | Int i -> fun buf ->
      bprintf buf "MAKE_INT(%t,%Ld);" (Register.write reg) i

    | String s ->
      let str = Global.create_string gctx s in
      fun buf -> bprintf buf "MAKE_STRING(%t,%t);" (Register.write reg) str

    | Var s ->
      let prelude, ident = Global.access_var gctx s in
      fun buf ->
        bprintf buf "%t%t=%t;" prelude (Register.write reg) ident

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

    | Define (i, x) ->
      let form = go ~reg x in
      let prelude, ident = Global.assign_var gctx i in
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
let gen_code forms =
  let gctx = Global.init () in
  let rctx = Register.init () in
  let buf = Buffer.create 2048 in
  (* compute each form, but do not write it yet
   * (to allow rctx to be updated) *)
  let form_writers = List.map (fun form ->
      let reg = Register.zero rctx in
      write_form ~gctx ~rctx ~reg form
    ) forms in
  let init_globals, deinit_globals = Global.write_ctx gctx in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  init_globals buf;
  Buffer.add_string buf "int main(){\n";
  (* emit declaration of necessary registers *)
  Register.write_ctx rctx buf;
  (* emit program statements *)
  form_writers |> List.iter (fun writer ->
      writer buf;
      bprintf buf "deinit(%t);\n" (Register.write (Register.zero rctx))
    );
  deinit_globals buf;
  Buffer.add_string buf "finalize();return 0;}\n";
  Buffer.contents buf
