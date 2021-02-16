type op = Parse.op =
  | Plus
  | Minus
  | Times
  | Len
  | Print

type form = Parse.form =
  | Int of int64
  | String of string
  | Ident of string
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
        bprintf buf "\\%02x" (Char.code c);
      loop (n + 1)
    )
  in
  Buffer.add_char buf '"';
  loop 0;
  Buffer.add_char buf '"'

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

module Global: sig
  (* keeps track of global program information *)
  type ctx

  val init: unit -> ctx
  val create_string: ctx -> string -> Writer.t
  val write_ctx: ctx -> Writer.t
end = struct
  type ctx = (int * string list) ref

  let init () = ref (0, [])

  let create_string ctx s =
    let i, lits = !ctx in
    ctx := (i + 1, s :: lits);
    fun buf -> bprintf buf "&STR%d" i

  let write_ctx ctx =
    let len, lits = !ctx in
    if len > 0 then
      let decls =
        List.rev lits
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
    else
      Writer.empty
end

(* Return a Writer.t that emits a sequence of statements which have the
 * effect of evaluating the given form and assigning the return value
 * to the given variable. *)
let write_form ~gctx ~vctx =
  let rec go ~var = function
    | Int i -> fun buf ->
      bprintf buf "MAKE_INT(%t,%Ld);" (Var.write var) i

    | String s ->
      let str = Global.create_string gctx s in
      fun buf -> bprintf buf "MAKE_STRING(%t,%t);" (Var.write var) str

    | Op (Plus, []) -> fun buf ->
      bprintf buf "MAKE_INT(%t,0);" (Var.write var)
    | Op (Plus, lhs :: rhss) ->
      write_fold_fun ~var ~name:"add" lhs rhss

    | Op (Minus, [x]) ->
      unary_fun ~var ~name:"neg" x
    | Op (Minus, lhs :: rhss) ->
      write_fold_fun ~var ~name:"sub" lhs rhss

    | Op (Times, []) -> fun buf ->
      bprintf buf "MAKE_INT(%t,1);" (Var.write var)
    | Op (Times, lhs :: rhss) ->
      write_fold_fun ~var ~name:"mul" lhs rhss

    | Op (Len, [x]) ->
      unary_fun ~var ~name:"len" x

    | Op (Print, [x]) ->
      let form = go ~var x in
      fun buf -> bprintf buf "%tdisplay(%t);" form (Var.write var)

    | Ident i ->
      raise (Invalid_argument (Printf.sprintf "variables not yet supported: '%s'"
                                 (String.escaped i)))

    | Op (Minus, [])
    | Op (Len, _)
    | Op (Print, _) ->
      raise (Invalid_argument "invalid expression")

  (* Helper function to construct a writer which emits code to evaluate a function's
   * argument and then call it, assigning its result to the given variable. *)
  and unary_fun ~var ~name form =
    let form = go ~var form in
    fun buf -> bprintf buf "%t%t=%s(%t);" form (Var.write var) name (Var.write var)

  (* Helper function to construct a writer which emits code to evaluate a sequence
   * of arguments and combine them with repeated calls to a 2-argument function,
   * assigning the final result to the given variable. *)
  and write_fold_fun ~var ~name lhs rhss =
    let v_rhs = Var.next vctx var in
    List.fold_left (fun lhs rhs ->
        let rhs = go ~var:v_rhs rhs in
        fun buf ->
          bprintf buf "%t%t%t=%s(%t,%t);"
            lhs
            rhs
            (Var.write var)
            name
            (Var.write var)
            (Var.write v_rhs)
      ) (go ~var lhs) rhss

  in go

(* Convert a form into a C program. *)
let gen_code forms =
  let gctx = Global.init () in
  let vctx = Var.init () in
  let buf = Buffer.create 2048 in
  (* compute each form, but do not write it yet
   * (to allow vctx to be updated) *)
  let form_writers = List.map (fun form ->
      let var = Var.zero vctx in
      write_form ~gctx ~vctx ~var form
    ) forms in
  Buffer.add_string buf "#include \"chemic.h\"\n";
  Global.write_ctx gctx buf;
  Buffer.add_string buf "int main(){\n";
  (* emit declaration of necessary variables *)
  Var.write_ctx vctx buf;
  (* emit program statements *)
  form_writers |> List.iter (fun writer ->
      writer buf;
      bprintf buf "deinit(%t);\n" (Var.write (Var.zero vctx))
    );
  Buffer.add_string buf "finalize();return 0;}\n";
  Buffer.contents buf
