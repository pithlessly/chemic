type op = Parse.op =
  | Plus
  | Minus
  | Times
  | Len
  | Register

type form = Parse.form =
  | Int of int64
  | String of string
  | Op of op * form list

let bprintf = Printf.bprintf

let seq_init n f =
  let rec loop idx () =
    if idx < n then
      let v = f idx in
      Seq.Cons (v, loop (idx + 1))
    else
      Seq.Nil
  in loop 0

module Var: sig
  type ctx
  type t

  val init: unit -> t * ctx
  val next: ctx -> t -> t
  val write_ctx: ctx -> Writer.t
  val write: t -> Writer.t
end = struct
  (* stores the highest register not yet used *)
  type ctx = int ref
  type t = int

  let init () = (0, ref 1)
  let next ctx v =
    ctx := max !ctx (v + 2);
    v + 1

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

let rec write_form ~vctx ~var = function
  | Int i -> fun buf ->
    let _ = vctx in
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

  | Op (Register, []) ->
    fun buf -> bprintf buf "%t=reg_restore();" (Var.write var)
  | Op (Register, [x]) ->
    let form = write_form x ~vctx ~var in
    fun buf ->
      form buf;
      bprintf buf "reg_save(%t);" (Var.write var)

  | Op (Minus, [])
  | Op (Len, _)
  | Op (Register, _) ->
    raise (Invalid_argument "invalid expression")

and unary_fun ~vctx ~var ~name form =
  let form = write_form form ~vctx ~var in
  fun buf ->
    form buf;
    bprintf buf "%t=%s(%t);" (Var.write var) name (Var.write var)

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

let _ = Var.next

(* Convert a form into a C program. *)
let gen_code forms =
  let var, vctx = Var.init () in
  let buf = Buffer.create 2048 in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  Buffer.add_string buf "int main(){\n";
  (* compute each form, but do not write it yet
   * (to allow vctx to be updated) *)
  let form_writers = List.map (write_form ~vctx ~var) forms in
  (* emit declaration of necessary variables *)
  Var.write_ctx vctx buf;
  (* emit program statements *)
  form_writers |> List.iter (fun writer ->
      writer buf;
      let v = Var.write var in
      bprintf buf "print(%t);deinit(%t);\n" v v;
    );
  Buffer.add_string buf "finalize();return 0;}\n";
  Buffer.contents buf
