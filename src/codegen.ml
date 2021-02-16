type op = Parse.op =
  | Plus
  | Minus
  | Times
  | Len
  | Print

type form = Parse.form =
  | Int of int64
  | String of string
  | Op of op * form list

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

module Var: sig
  (* tracks information related to generated variables *)
  type ctx
  type t

  (* create a new context *)
  val init: unit -> ctx
  (* the initial variable associated with a context *)
  val zero: ctx -> t
  (* return the next variable *)
  val next: ctx -> t -> t
  (* write any declarations necessary for the variables to be used *)
  val write_ctx: ctx -> Writer.t
  (* write as a C identifier *)
  val write: t -> Writer.t
end = struct
  (* stores the highest register not yet used *)
  type ctx = int ref
  type t = int

  let init () = ref 0
  let next ctx v =
    ctx := max !ctx (v + 2);
    v + 1
  let zero ctx = next ctx (-1)

  let write v buf = bprintf buf "v%d" v

  let write_ctx ctx =
    if !ctx > 0 then
      (* "Obj v0,v1,v2..." *)
      let var_list = Writer.join ',' (seq_init !ctx write) in
      fun buf -> bprintf buf "Obj %t;\n" var_list
    else
      Writer.empty
end

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
        bprintf buf "\\%02x" (Char.code c);
      loop (n + 1)
    )
  in
  Buffer.add_char buf '"';
  loop 0;
  Buffer.add_char buf '"'

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given form and assigning the return value
 * to the given variable. *)
let rec write_form ~vctx ~var = function
  | Int i -> fun buf ->
    bprintf buf "MAKE_INT(%t,%Ld);" (Var.write var) i

  | String s -> fun buf ->
    bprintf buf "MAKE_STRING(%t,%t,%d);"
      (Var.write var)
      (write_string_literal s)
      (String.length s)

  | Op (Plus, []) -> fun buf ->
    bprintf buf "MAKE_INT(%t,0);" (Var.write var)
  | Op (Plus, lhs :: rhss) ->
    write_fold_fun ~vctx ~var ~name:"add" lhs rhss

  | Op (Minus, [x]) ->
    unary_fun ~vctx ~var ~name:"neg" x
  | Op (Minus, lhs :: rhss) ->
    write_fold_fun ~vctx ~var ~name:"sub" lhs rhss

  | Op (Times, []) -> fun buf ->
    bprintf buf "MAKE_INT(%t,1);" (Var.write var)
  | Op (Times, lhs :: rhss) ->
    write_fold_fun ~vctx ~var ~name:"mul" lhs rhss

  | Op (Len, [x]) ->
    unary_fun ~vctx ~var ~name:"len" x

  | Op (Print, [x]) ->
    let form = write_form x ~vctx ~var in
    fun buf ->
      form buf;
      bprintf buf "print(%t);" (Var.write var)

  | Op (Minus, [])
  | Op (Len, _)
  | Op (Print, _) ->
    raise (Invalid_argument "invalid expression")

(* Helper function to construct a writer which emits code to evaluate a function's
 * argument and then call it, assigning its result to the given variable. *)
and unary_fun ~vctx ~var ~name form =
  let form = write_form form ~vctx ~var in
  fun buf ->
    form buf;
    bprintf buf "%t=%s(%t);" (Var.write var) name (Var.write var)

(* Helper function to construct a writer which emits code to evaluate a sequence
 * of arguments and combine them with repeated calls to a 2-argument function,
 * assigning the final result to the given variable. *)
and write_fold_fun ~vctx ~var ~name lhs rhss =
  let v_rhs = Var.next vctx var in
  List.fold_left (fun lhs rhs ->
      let rhs = write_form rhs ~vctx ~var:v_rhs in
      fun buf ->
        lhs buf;
        rhs buf;
        bprintf buf "%t=%s(%t,%t);"
          (Var.write var)
          name
          (Var.write var)
          (Var.write v_rhs)
    ) (write_form lhs ~vctx ~var) rhss

(* Convert a form into a C program. *)
let gen_code forms =
  let vctx = Var.init () in
  let buf = Buffer.create 2048 in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  Buffer.add_string buf "int main(){\n";
  (* compute each form, but do not write it yet
   * (to allow vctx to be updated) *)
  let form_writers = List.map (fun form ->
      let var = Var.zero vctx in
      write_form ~vctx ~var form
    ) forms in
  (* emit declaration of necessary variables *)
  Var.write_ctx vctx buf;
  (* emit program statements *)
  form_writers |> List.iter (fun writer ->
      writer buf;
      bprintf buf "deinit(%t);\n" (Var.write (Var.zero vctx))
    );
  Buffer.add_string buf "finalize();return 0;}\n";
  Buffer.contents buf
