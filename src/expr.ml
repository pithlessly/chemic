type form = Parse.form =
  | Int of int64
  | String of string
  | Ident of string
  | List of form list

type op =
  | Plus
  | Minus
  | Times
  | Len
  | Print
  | Cons
  | Call

type global_var_id = int
type local_var_id = int
type var_id =
  | Global of global_var_id
  | Local of local_var_id

type string_id = int
type proc_id = int

type expr =
  | Int of int64
  | String of string_id
  | Var of var_id
  | Define of var_id * expr
  | Proc of proc_id
  | Builtin of op * expr list

module StringMap = Map.Make(String)

type local_ctx = {
  mutable var_ids: int StringMap.t;
}

type proc = {
  lctx: local_ctx;
  body: expr list;
}

type global_ctx = {
  mutable string_literals: string list;
  (* should be the same as 'List.length string_literals' *)
  mutable num_strings: int;
  mutable procs: proc list;
  (* should be the same as 'List.length procs' *)
  mutable num_procs: int;
  mutable var_ids: int StringMap.t;
}

let add_var_id key map =
  match StringMap.find_opt key map with
  | Some id -> (id, map)
  | None ->
    let id = StringMap.cardinal map in
    (id, StringMap.add key id map)

let get_op = function
  | Ident "+" -> Plus
  | Ident "-" -> Minus
  | Ident "*" -> Times
  | Ident "len" -> Len
  | Ident "display" -> Print
  | Ident "cons" -> Cons
  | Ident "call" -> Call
  | Ident s ->
    raise (Invalid_argument (Printf.sprintf "invalid operator: \"%s\""
                               (String.escaped s)))
  | _ ->
    raise (Invalid_argument "non-identifier in function position")

let build_with ~(gctx: global_ctx) =
  let rec go ~(lctx: local_ctx option) = function
    | List [Ident "define"; Ident name; expr] ->
      let id =
        match lctx with
        | Some lctx ->
          let id, var_ids' = add_var_id name lctx.var_ids in
          lctx.var_ids <- var_ids';
          Local id
        | None ->
          let id, var_ids' = add_var_id name gctx.var_ids in
          gctx.var_ids <- var_ids';
          Global id
      in
      Define (id, go ~lctx expr)

    | List (Ident "proc" :: body) ->
      let id = gctx.num_procs in
      gctx.num_procs <- id + 1;
      let lctx = { var_ids = StringMap.empty } in
      let proc = { lctx; body = List.map (go ~lctx:(Some lctx)) body } in
      gctx.procs <- proc :: gctx.procs;
      Proc id

    | List (f :: args) ->
      Builtin (get_op f, List.map (go ~lctx) args)

    | List [] ->
      raise (Invalid_argument "nil cannot be evaluated")

    | Ident name ->
      let id =
        match lctx with
        | Some lctx ->
          (match StringMap.find_opt name lctx.var_ids with
           | Some id -> Local id
           | None ->
             match StringMap.find_opt name gctx.var_ids with
             | Some id -> Global id
             | None ->
               (* If a procedure references a variable that cannot be found in the
                * global or local scope, we assume that the variable exists in the
                * global scope but has not been initialized. *)
               let id, var_ids' = add_var_id name gctx.var_ids in
               gctx.var_ids <- var_ids';
               Global id)
        | None ->
          match StringMap.find_opt name gctx.var_ids with
          | Some id -> Global id
          | None ->
            (* On the other hand, if an undefined variable is referenced in the
             * global scope, we throw an error. Note that many Scheme implementations
             * seem to allow previously undefined global variables to be created in
             * procedures using 'set!', which would make this error incorrect.
             * However, such usage of 'set!' is not standard-compliant, so we don't
             * have to support it.
             *)
            raise (Invalid_argument (Printf.sprintf "undefined variable: %s" name))
      in
      Var id

    | Int i ->
      Int i

    | String s ->
      let id = gctx.num_strings in
      gctx.num_strings <- id + 1;
      gctx.string_literals <- s :: gctx.string_literals;
      String id
  in go ~lctx:None

let build forms =
  let gctx = { string_literals = [];
               num_strings = 0;
               procs = [];
               num_procs = 0;
               var_ids = StringMap.empty }
  in
  (gctx, List.map (build_with ~gctx) forms)

let bprintf = Printf.bprintf

let write_string id =
  fun buf -> bprintf buf "&STR%d" id

let write_proc id =
  fun buf -> bprintf buf "&PROC%d" id

let write_var = function
  | Global id -> fun buf -> bprintf buf "GLO%d" id
  | Local id -> fun buf -> bprintf buf "LOC%d" id

let write_access_var id =
  let var = write_var id in
  ((fun buf -> bprintf buf "clone(%t);" var), var)

let write_assign_var id =
  let var = write_var id in
  ((fun buf -> bprintf buf "deinit(%t);" var), var)

let write_ctx gctx ~write_proc_body =

  (* declarations of strings
   * (e.g. 'Str STR0={...},STR1={...},...;') *)
  let string_decls =
    if gctx.num_strings = 0 then
      Writer.empty
    else
      let decls =
        List.rev gctx.string_literals
        |> List.to_seq
        |> Utils.seq_mapi (fun i lit ->
            fun buf ->
              bprintf buf "STR%d={%d,0,%t}"
                i (* len *)
                (String.length lit) (* ref_count *)
                (Writer.c_string lit) (* data *)
          )
        |> Writer.join ','
      in
      fun buf -> bprintf buf "Str %t;\n" decls
  in

  let declare_vars ~indent ~prefix num =
    if num = 0 then
      Writer.empty
    else
      let decls =
        Utils.seq_init num
          (fun i -> fun buf -> bprintf buf "%s%d=NIL" prefix i)
        |> Writer.join ','
      in
      fun buf -> bprintf buf "%sObj %t;\n" indent decls
  in

  let deinit_vars prefix num =
    fun buf ->
      for i = 0 to num - 1 do
        bprintf buf "deinit(%s%d);" prefix i;
      done
  in

  let procs =
    fun buf ->
      gctx.procs |> List.iteri (fun i { lctx; body } ->
          let num_locals = StringMap.cardinal lctx.var_ids in
          let write_final = deinit_vars "LOC" num_locals in
          bprintf buf "Obj PROC%d() {\n%t%t}\n"
            (gctx.num_procs - 1 - i)
            (declare_vars ~indent:"  " ~prefix:"LOC" num_locals)
            (write_proc_body ~write_final body)
        )
  in

  let num_globals = StringMap.cardinal gctx.var_ids in
  let prelude =
    fun buf ->
      string_decls buf;
      declare_vars ~indent:"" ~prefix:"GLO" num_globals buf;
      procs buf
  in

  (prelude, deinit_vars "GLO" num_globals)
