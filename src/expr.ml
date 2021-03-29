type form = Parse.form =
  | Int of int64
  | String of string
  | Ident of string
  | List of form list

type global_var_id = int
type local_var_id = int
type var_id =
  | Global of global_var_id
  | Local of { id: local_var_id; locality: int }

type string_id = int
type proc_id = int

type expr =
  | Int of int64
  | String of string_id
  | Var of var_id
  | OperatorArg of Operator.t
  | Define of var_id * expr
  | Let of { lhs: local_var_id; rhs: expr; body: expr list }
  | Lambda of proc_id
  | If of { condition: expr; true_case: expr; false_case: expr }
  | Call of expr * expr list
  | Operator of Operator.t * expr list

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type var_metadata = {
  source: [`Param | `Internal];
  boxed: bool;
}

type local_ctx = {
  mutable vars: var_metadata IntMap.t;
}

let add_local_var id source lctx =
  lctx.vars <- IntMap.add id { source; boxed = false } lctx.vars

let make_local_boxed id lctx =
  let metadata = IntMap.find id lctx.vars in
  lctx.vars <- IntMap.add id { metadata with boxed = true } lctx.vars

type local = {
  lctx: local_ctx;
  body: expr list;
}

type proc = {
  local: local;
  num_params: int;
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

let find_defined_vars forms =
  let present = ref StringMap.empty in
  List.to_seq forms
  |> Seq.filter_map
    (function
      | List [Ident "define"; Ident i; _] ->
        (match StringMap.find_opt i !present with
         | Some _ -> None
         | None ->
           present := StringMap.add i () !present;
           Some i)
      | _ -> None)

let build_with ~(gctx: global_ctx) =
  let rec go
      ~(lctx: local_ctx)
      ~(local_scopes: local_var_id StringMap.t list)
      ~(block_level: bool)
      ~(top_level: bool) =
    function

    | Ident name ->
      (match Utils.search (StringMap.find_opt name) local_scopes with
       | Some (locality, id) -> Var (Local { locality; id })
       | None ->
         match StringMap.find_opt name gctx.globals with
         | Some id -> Var (Global id)
         | None ->
           match Operator.lookup name with
           | Some op -> OperatorArg op
           | None ->
             (* If an undefined variable is referenced in the global scope, we
              * throw an error. Note that many Scheme implementations allow global
              * variables to be defined using 'set!', which would make this error
              * incorrect. However, such usage of 'set!' is not standard-compliant,
              * so we don't have to support it. *)
             raise (Invalid_argument (Printf.sprintf "undefined variable: %s" name)))

    | List [Ident "box"; Ident ident] ->
      (match Utils.search (StringMap.find_opt ident) local_scopes with
       | Some (locality, id) ->
         if locality = 0 then
           lctx |> make_local_boxed id;
         Var (Local { locality; id })
       | None ->
         raise (Invalid_argument "undefined variable"))

    | List [Ident "define"; Ident ident; expr] ->
      let id =
        if not block_level then
          raise (Invalid_argument "(define) cannot be used as an expression")
        else if top_level then
          Global (StringMap.find ident gctx.globals)
        else
          let id = StringMap.find ident (List.hd local_scopes) in
          Local { locality = 0; id }
      in
      Define (id, go ~lctx ~local_scopes ~block_level:false ~top_level expr)

    | List (Ident "let" ::
            List [List [Ident lhs; rhs]] ::
            body) ->
      (* generate IDs for variables (define)d by the body and add them to lctx *)
      let id_offset = IntMap.cardinal lctx.vars in
      let define_scope =
        find_defined_vars body
        |> Utils.seq_mapi (fun i var ->
            let id = id_offset + i in
            lctx |> add_local_var id `Internal;
            (var, id)
          )
        |> StringMap.of_seq
      in
      (* generate an ID for the variable being bound and add it to lctx *)
      let var_id = id_offset + StringMap.cardinal define_scope in
      lctx |> add_local_var var_id `Internal;
      (* pass over the rhs expression using the original scoping rules *)
      let recurse_rhs =
        go ~lctx ~local_scopes ~block_level:false ~top_level
      in
      (* update the local scope with the variable being bound and the
       * variables that were (define)d; the latter take precedence *)
      let local_scopes =
        local_scopes |> Utils.map_hd (fun sc ->
            sc
            |> StringMap.add lhs var_id
            |> StringMap.union (fun _ defined _ -> Some defined) define_scope
          )
      in
      (* pass over the body using the new local scope *)
      let recurse_body =
        go ~lctx ~local_scopes ~block_level:true ~top_level
      in
      Let { lhs = var_id;
            rhs = recurse_rhs rhs;
            body = List.map recurse_body body }

    | List (Ident "lambda" :: List params :: body) ->
      if Utils.null body then
        raise (Invalid_argument "lambda body cannot be empty");

      let lctx = { vars = IntMap.empty } in

      (* get sequences of two kinds of local variables and their sources *)
      let param_vars =
        List.to_seq params
        |> Seq.map (function
            | Ident var -> (var, `Param)
            | _ -> raise (Invalid_argument "lambda parameters must be identifiers"))
      in
      let defined_vars =
        find_defined_vars body
        |> Seq.map (fun var -> (var, `Internal))
      in

      (* construct the initial scope containing those variables *)
      let local_scope =
        Seq.append param_vars defined_vars
        |> Utils.seq_mapi (fun i (var, source) ->
            let id = i in
            lctx |> add_local_var id source;
            (var, id)
          )
        |> StringMap.of_seq
      in
      let recurse =
        go ~lctx ~block_level:true ~top_level:false
          ~local_scopes:(local_scope :: local_scopes)
      in
      let local = { lctx; body = List.map recurse body } in
      gctx.procs <- { num_params = List.length params;
                      local } :: gctx.procs;
      let id = gctx.num_procs in
      gctx.num_procs <- id + 1;
      Lambda id

    | List [Ident "if"; cond; true_case; false_case] ->
      let recurse = go ~lctx ~local_scopes ~block_level:false ~top_level in
      If { condition = recurse cond;
           true_case = recurse true_case;
           false_case = recurse false_case }

    | List (f :: args) ->
      let op =
        match f with
        | Ident i -> Operator.lookup i
        | _ -> None
      in

      let recurse = go ~lctx ~local_scopes ~block_level:false ~top_level in

      (match op with
       | None -> Call (recurse f, List.map recurse args)
       | Some op -> Operator (op, List.map recurse args))

    | List [] ->
      raise (Invalid_argument "nil cannot be evaluated")

    | Int i ->
      Int i

    | String s ->
      let id = gctx.num_strings in
      gctx.num_strings <- id + 1;
      gctx.string_literals <- s :: gctx.string_literals;
      String id

  in
  fun forms ->
    let lctx = { vars = IntMap.empty } in
    find_defined_vars forms
    |> Utils.seq_iteri (fun i var ->
        let id = i in
        gctx.globals <- StringMap.add var id gctx.globals;
      );
    { lctx;
      body = forms |> List.map
               (go
                  ~lctx
                  ~local_scopes:[StringMap.empty]
                  ~block_level:true
                  ~top_level:true) }

let bprintf = Printf.bprintf

type local_data = {
  locals: var_metadata array;
  body: expr list;
}

let write_local { lctx; body } =
  { locals =
      IntMap.to_seq lctx.vars
      |> Seq.map snd
      |> Array.of_seq;
    body }

type proc_writers = {
  name: Writer.t;
  num_params: int;
  local: local_data;
}

type global_writers = {
  decls: Writer.t;
  procs: proc_writers list;
  main: local_data;
  after_main: Writer.t;
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
              bprintf buf "STR%d={%d,%t}"
                i
                (String.length lit) (* 'len' field *)
                (Writer.c_string lit) (* 'data' field *)
          )
        |> Writer.join ','
      in
      fun buf -> bprintf buf "static Str %t;\n" decls
  in

  let num_globals = StringMap.cardinal gctx.globals in

  (* declarations of global static objects to hold global variables *)
  let write_global_decls =
    let decls =
      Utils.seq_replicate num_globals
        (fun buf -> Buffer.add_string buf "NIL")
      |> Writer.join ','
    in
    fun buf -> bprintf buf "static Obj g[%d]={%t};\n" num_globals decls
  in

  (* declaration of a const representing the number of global variables *)
  let write_num_global_decl =
    fun buf ->
      bprintf buf "static const size_t NUM_GLOBALS=%d;\n" num_globals
  in

  { decls =
      (fun buf ->
         write_string_decls buf;
         write_global_decls buf;
         write_num_global_decl buf);

    procs =
      List.rev gctx.procs
      |> List.mapi (fun i (proc: proc) ->
          { name = (fun buf -> bprintf buf "PROC%d" i);
            num_params = proc.num_params;
            local = write_local proc.local });

    main = write_local main;

    after_main = Writer.empty;
  }

let write_access_string id =
  fun buf -> bprintf buf "&STR%d" id

let write_access_proc id =
  fun buf -> bprintf buf "&PROC%d" id
