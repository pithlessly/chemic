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
  | Let of { lhs: local_var_id; rhs: expr; body: expr list }
  | Proc of proc_id
  | Builtin of op * expr list

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type local_ctx = {
  (* Tracks whether each variable was declared using 'define' (in
   * which case it should be initialized to NIL because it can be
   * used before it is initialized) or using 'let' (in which case
   * it can be left uninitialized. *)
  mutable var_metadata: bool IntMap.t;
}

type proc = {
  locals: local_ctx;
  body: expr list;
}

type global_ctx = {
  mutable string_literals: string list;
  (* should be the same as 'List.length string_literals' *)
  mutable num_strings: int;

  mutable procs: proc list;
  (* should be the same as 'List.length procs' *)
  mutable num_procs: int;

  mutable globals: global_var_id StringMap.t;
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

let find_defined_vars forms =
  List.to_seq forms
  |> Seq.filter_map
    (function
      | List [Ident "define"; Ident i; _] -> Some i
      | _ -> None)

let build_with ~(gctx: global_ctx) =
  let rec go
      ~(lctx: local_ctx)
      ~(local_scopes: local_var_id StringMap.t list)
      ~(block_level: bool) =
    function

    | List [Ident "define"; Ident ident; expr] ->
      let id =
        if not block_level then
          raise (Invalid_argument "(define) cannot be used as an expression")
        else
          (* the variable being assigned to must already be present in the
           * nearest scope, as the caller will have already scanned through
           * the entire body for (define) expressions and added it in *)
          match local_scopes with
          | scope :: _ -> Local (StringMap.find ident scope)
          | [] -> Global (StringMap.find ident gctx.globals)
      in
      Define (id, go ~lctx ~local_scopes ~block_level:false expr)

    | List (Ident "let" ::
            List [List [Ident lhs; rhs]] ::
            body) ->
      let id_offset = IntMap.cardinal lctx.var_metadata in
      let scope =
        find_defined_vars body
        |> Utils.seq_mapi (fun i var ->
            let id = id_offset + i in
            lctx.var_metadata <- IntMap.add id true lctx.var_metadata;
            (var, id)
          )
        |> StringMap.of_seq
      in
      let var_id = id_offset + StringMap.cardinal scope in
      let scope = StringMap.add lhs var_id scope in
      lctx.var_metadata <- IntMap.add var_id false lctx.var_metadata;
      let recurse_rhs =
        go ~lctx ~local_scopes ~block_level:false
      in
      let recurse_body =
        go ~lctx ~local_scopes:(scope :: local_scopes) ~block_level:true
      in
      Let { lhs = var_id;
            rhs = recurse_rhs rhs;
            body = List.map recurse_body body }

    | List (Ident "proc" :: body) ->
      let lctx = { var_metadata = IntMap.empty } in
      let scope =
        find_defined_vars body
        |> Utils.seq_mapi (fun i var ->
            let id = i in
            lctx.var_metadata <- IntMap.add id true lctx.var_metadata;
            (var, id)
          )
        |> StringMap.of_seq
      in
      let recurse =
        go ~lctx ~local_scopes:[scope] ~block_level:true
      in
      gctx.procs <- { locals = lctx;
                      body = List.map recurse body } :: gctx.procs;
      let id = gctx.num_procs in
      gctx.num_procs <- id + 1;
      Proc id

    | List (f :: args) ->
      let recurse = go ~lctx ~local_scopes ~block_level:false in
      Builtin (get_op f, List.map recurse args)

    | List [] ->
      raise (Invalid_argument "nil cannot be evaluated")

    | Ident name ->
      let id =
        match Utils.search (StringMap.find_opt name) local_scopes with
        | Some id -> Local id
        | None ->
          match StringMap.find_opt name gctx.globals with
          | Some id -> Global id
          | None ->
            (* If an undefined variable is referenced in the global scope, we
             * throw an error. Note that many Scheme implementations allow global
             * variables to be defined using 'set!', which would make this error
             * incorrect. However, such usage of 'set!' is not standard-compliant,
             * so we don't have to support it. *)
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

  in
  fun forms ->
    let lctx = { var_metadata = IntMap.empty } in
    find_defined_vars forms
    |> Utils.seq_iteri (fun i var ->
        let id = i in
        gctx.globals <- StringMap.add var id gctx.globals;
      );
    { locals = lctx;
      body = List.map (go ~lctx ~local_scopes:[] ~block_level:true) forms }

let bprintf = Printf.bprintf

type local_writers = {
  before: Writer.t;
  after: Writer.t;
  body: expr list;
}

let write_local { locals; body } =
  { before =
      (if IntMap.cardinal locals.var_metadata = 0 then
         Writer.empty
       else
         let decls =
           IntMap.to_seq locals.var_metadata
           |> Seq.map (fun (i, is_define) buf ->
               bprintf buf (if is_define then
                              "LOC%d=NIL"
                            else
                              "LOC%d") i
             )
           |> Writer.join ','
         in
         fun buf -> bprintf buf "  Obj %t;\n" decls);

    after =
      (fun buf ->
         IntMap.iter (fun i is_define ->
             if is_define then
               bprintf buf "deinit(LOC%d);" i
           ) locals.var_metadata);

    body }

type global_writers = {
  decls: Writer.t;
  procs: (Writer.t * local_writers) list;
  main: local_writers;
  more_after_main: Writer.t;
}

let build forms =
  let gctx = { string_literals = [];
               num_strings = 0;
               procs = [];
               num_procs = 0;
               globals = StringMap.empty }
  in

  let main = build_with ~gctx forms in

  (* declarations of global static values to hold string literals *)
  let write_string_decls =
    if gctx.num_strings = 0 then
      Writer.empty
    else
      let decls =
        List.rev gctx.string_literals
        |> List.to_seq
        |> Utils.seq_mapi (fun i lit ->
            fun buf ->
              bprintf buf "STR%d={%d,0,%t}"
                i
                (String.length lit) (* 'len' field *)
                (* 'ref_count' field is 0 to indicate it should never be freed *)
                (Writer.c_string lit) (* 'data' field *)
          )
        |> Writer.join ','
      in
      fun buf -> bprintf buf "static Str %t;\n" decls
  in

  let num_globals = StringMap.cardinal gctx.globals in

  (* declarations of global static objects to hold global variables *)
  let write_global_decls =
    if num_globals = 0 then
      Writer.empty
    else
      let decls =
        Utils.seq_init num_globals (fun i buf -> bprintf buf "GLO%d" i)
        |> Writer.join ','
      in
      fun buf -> bprintf buf "static Obj %t;\n" decls
  in

  { decls = (fun buf -> write_string_decls buf; write_global_decls buf);

    procs =
      List.rev gctx.procs
      |> List.mapi (fun i proc ->
          ((fun buf -> bprintf buf "PROC%d" i),
           write_local proc));

    main = write_local main;

    more_after_main =
      (fun buf ->
         for i = 0 to num_globals - 1 do
           bprintf buf "deinit(GLO%d);" i
         done)
  }

let write_access_string id =
  fun buf -> bprintf buf "&STR%d" id

let write_access_proc id =
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

let write_let_var id =
  let var = write_var (Local id) in
  (var, (fun buf -> bprintf buf "deinit(%t);" var))
