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

type var_id = int
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

type ctx = {
  mutable string_literals: string list;
  (* should be the same as 'List.length string_literals' *)
  mutable num_strings: int;
  mutable procs: expr list list;
  (* should be the same as 'List.length procs' *)
  mutable num_procs: int;
  mutable var_ids: int StringMap.t;
}

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

let build_with ctx =
  let rec go = function
    | List [Ident "define"; Ident name; expr] ->
      let id =
        match StringMap.find_opt name ctx.var_ids with
        | Some id -> id
        | None ->
          let id = StringMap.cardinal ctx.var_ids in
          ctx.var_ids <- StringMap.add name id ctx.var_ids;
          id
      in
      Define (id, go expr)

    | List (Ident "proc" :: body) ->
      let id = ctx.num_procs in
      ctx.num_procs <- id + 1;
      ctx.procs <- List.map go body :: ctx.procs;
      Proc id

    | List (f :: args) ->
      Builtin (get_op f, List.map go args)

    | List [] ->
      raise (Invalid_argument "nil cannot be evaluated")

    | Ident name ->
      (match StringMap.find_opt name ctx.var_ids with
       | Some id -> Var id
       | None ->
         (* TODO: error if a variable is accessed before it is assigned *)
         let id = StringMap.cardinal ctx.var_ids in
         ctx.var_ids <- StringMap.add name id ctx.var_ids;
         Var id)

    | Int i ->
      Int i

    | String s ->
      let id = ctx.num_strings in
      ctx.num_strings <- id + 1;
      ctx.string_literals <- s :: ctx.string_literals;
      String id
  in go

let build forms =
  let ctx = { string_literals = [];
              num_strings = 0;
              procs = [];
              num_procs = 0;
              var_ids = StringMap.empty }
  in
  (ctx, List.map (build_with ctx) forms)

let bprintf = Printf.bprintf

let write_string id =
  fun buf -> bprintf buf "&STR%d" id

let write_proc id =
  fun buf -> bprintf buf "&PROC%d" id

let write_var id =
  fun buf -> bprintf buf "GLO%d" id

let write_access_var id =
  let var = write_var id in
  ((fun buf -> bprintf buf "clone(%t);" var), var)

let write_assign_var id =
  let var = write_var id in
  ((fun buf -> bprintf buf "deinit(%t);" var), var)

let write_ctx ctx ~write_proc_body =

  (* declarations of strings
   * (e.g. 'Str STR0={...},STR1={...},...;') *)
  let string_decls =
    if ctx.num_strings = 0 then
      Writer.empty
    else
      let decls =
        List.rev ctx.string_literals
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

  let num_vars = StringMap.cardinal ctx.var_ids in
  (* declarations of variables *)
  let var_decls =
    if num_vars = 0 then
      Writer.empty
    else
      let decls =
        Utils.seq_init num_vars (fun i ->
            fun buf ->
              bprintf buf "%t=NIL" (write_var i)
          )
        |> Writer.join ','
      in
      fun buf -> bprintf buf "Obj %t;\n" decls
  in

  let procs =
    fun buf ->
      ctx.procs |> List.iteri (fun i proc ->
          bprintf buf "Obj PROC%d() {\n%t}\n"
            (ctx.num_procs - 1 - i)
            (write_proc_body proc)
        );
  in

  let deinits =
    fun buf ->
      Buffer.add_string buf "  ";
      for i = 0 to num_vars - 1 do
        bprintf buf "deinit(%t);" (write_var i);
      done;
      Buffer.add_char buf '\n';
  in

  ((fun buf -> string_decls buf; var_decls buf; procs buf), deinits)
