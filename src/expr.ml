type form = Parse.form =
  | Int of int64
  | String of string
  | Ident of string
  | List of form list

type string_id = int
type proc_id = int
let int_of_proc_id i = i

type global_var_id = int
type local_var_id = int
type var_id =
  | Global of global_var_id
  | Local of local_var_id
  | Nonlocal of { distance: int; proc_id: proc_id; id: local_var_id }

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
  pos: int;
  boxed: bool;
}

type local_ctx = {
  mutable vars: var_metadata IntMap.t;
  (* does this procedure contain variables captured by inner lambdas? *)
  mutable is_closure: bool;
  (* are variables from enclosing procedures captured by inner lambdas? *)
  mutable fwd_env: bool;
}

let new_local_var source lctx =
  let id = IntMap.cardinal lctx.vars in
  lctx.vars <- IntMap.add id { source; pos = 0; boxed = false } lctx.vars;
  id

let make_local_boxed id lctx =
  let metadata = IntMap.find id lctx.vars in
  lctx.vars <- IntMap.add id { metadata with boxed = true } lctx.vars

type local_intermediate = {
  lctx: local_ctx;
  (* this needs to be mutable for reasons that will be elaborated on later *)
  mutable body: expr list;
}

type proc_intermediate = {
  local: local_intermediate;
  num_params: int;
}

type global_ctx = {
  mutable string_literals: string list;
  (* should be the same as 'List.length string_literals' *)
  mutable num_strings: int;

  mutable procs: proc_intermediate list;
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

type local_var_layer = {
  ctx: local_ctx;
  (* we use -1 to represent main unfortunately *)
  id: proc_id;
  scope: local_var_id StringMap.t;
}

let build_with ~(gctx: global_ctx): form list -> local_intermediate =
  let rec go
      ~(local: local_var_layer)
      ~(nonlocal: local_var_layer list)
      ~(block_level: bool)
      ~(top_level: bool) =
    function

    | Ident name ->
      (match StringMap.find_opt name local.scope with
       | Some id -> Var (Local id)
       | None ->
         (* if a variable name is found in an enclosing scope, mark it as boxed and mark
          * the current procedure as a closure *)
         let do_search layer =
           match StringMap.find_opt name layer.scope with
           | None -> None
           | Some id ->
             layer.ctx |> make_local_boxed id;
             Some (layer.id, id)
         in

         match Utils.search do_search nonlocal with
         | Some (idx, (proc_id, id)) ->
           local.ctx.is_closure <- true;
           nonlocal |> Utils.iter_max
             (fun local -> local.ctx.fwd_env <- true) idx;
           Var (Nonlocal { distance = idx; proc_id; id })

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

    | List [Ident "define"; Ident ident; expr] ->
      let id =
        if not block_level then
          raise (Invalid_argument "(define) cannot be used as an expression")
        else if top_level then
          Global (StringMap.find ident gctx.globals)
        else
          Local (StringMap.find ident local.scope)
      in
      Define (id, go ~local ~nonlocal ~block_level:false ~top_level expr)

    | List (Ident "let" ::
            List [List [Ident lhs; rhs]] ::
            body) ->
      (* add the variables (define)d by the body into the local state and
       * create a scope map based on their IDs *)
      let define_scope =
        find_defined_vars body
        |> Seq.map (fun var ->
            let id = local.ctx |> new_local_var `Internal in
            (var, id))
        |> StringMap.of_seq
      in
      (* pass over the rhs expression using the original scoping rules *)
      let recurse_rhs =
        go ~local ~nonlocal ~block_level:false ~top_level:false
      in
      (* add the variable being bound into the local state *)
      let lhs_id = local.ctx |> new_local_var `Internal in
      (* update the local scope with the variable being bound and the
       * variables that were (define)d; the latter take precedence *)
      let new_scope =
        StringMap.union (fun _ defined _ -> Some defined)
          define_scope
          (StringMap.add lhs lhs_id local.scope)
      in
      (* pass over the body using the new local scope *)
      let recurse_body =
        go ~nonlocal ~block_level:true ~top_level:false
          ~local:{local with scope = new_scope}
      in
      Let { lhs = lhs_id;
            rhs = recurse_rhs rhs;
            body = List.map recurse_body body }

    | List (Ident "lambda" :: List params :: body) ->
      if Utils.null body then
        raise (Invalid_argument "lambda body cannot be empty");

      let lctx = {
        vars = IntMap.empty;
        is_closure = false;
        fwd_env = false } in

      (* Add a dummy procedure to `gctx` with an empty body -
       * the reason it is necessary to do this beforehand is so
       * that we can give the procudure a stable ID, which other
       * enclosed procudures can use to refer to it. Once we are
       * done traversing the lambda body, we can mutate `body`
       * in `local_intermediate` with the result (and `lctx` is
       * also mutated in the process. *)
      let local_interm = { lctx; body = [] } in
      gctx.procs <- { local = local_interm;
                      num_params = List.length params } :: gctx.procs;
      let id = gctx.num_procs in
      gctx.num_procs <- id + 1;

      (* get sequences of two kinds of local variables and their sources:
       * those which are parameters, and those (define)d *)
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
      let new_layer = {
        scope =
          Seq.append param_vars defined_vars
          |> Seq.map (fun (var, source) ->
              let id = lctx |> new_local_var source in
              (var, id)
            )
          |> StringMap.of_seq;
        ctx = lctx;
        id }
      in

      (* traverse the lambda body *)
      let recurse =
        go ~block_level:true ~top_level:false
          ~local:new_layer ~nonlocal:(local :: nonlocal)
      in

      local_interm.body <- List.map recurse body;
      Lambda id

    | List [Ident "if"; cond; true_case; false_case] ->
      let recurse = go ~local ~nonlocal ~block_level:false ~top_level in
      If { condition = recurse cond;
           true_case = recurse true_case;
           false_case = recurse false_case }

    | List (f :: args) ->
      (* TODO: this check is incorrect; global variables override builtins *)
      let op =
        match f with
        | Ident i -> Operator.lookup i
        | _ -> None
      in

      let recurse = go ~local ~nonlocal ~block_level:false ~top_level in

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
    let lctx = {
      vars = IntMap.empty;
      is_closure = false;
      fwd_env = false } in
    (* treat all `(define)`d variables at the top level as globals *)
    find_defined_vars forms
    |> Utils.seq_iteri (fun i var ->
        let id = i in
        gctx.globals <- StringMap.add var id gctx.globals;
      );
    { lctx;
      body = forms |> List.map
               (go
                  ~local:{ ctx = lctx; scope = StringMap.empty; id = -1 }
                  ~nonlocal:[]
                  ~block_level:true
                  ~top_level:true) }

let bprintf = Printf.bprintf

type local_data = {
  locals: var_metadata array;
  num_boxed: int;
  num_unboxed: int;
  body: expr list;
  fwd_env: bool;
}

let write_local { lctx; body } =
  let num_boxed = ref 0 in
  let num_unboxed = ref 0 in
  let locals =
    IntMap.to_seq lctx.vars
    |> Seq.map (fun (_, metadata) ->
        let r = if metadata.boxed then num_boxed else num_unboxed in
        let pos = !r in
        r := pos + 1;
        { metadata with pos }
      )
    |> Array.of_seq
  in
  { locals;
    num_boxed = !num_boxed;
    num_unboxed = !num_unboxed;
    fwd_env = lctx.fwd_env;
    body }

type proc_writers = {
  name: Writer.t;
  num_params: int;
  local: local_data;
  is_closure: bool;
}

type global_writers = {
  decls: Writer.t;
  procs: proc_writers array;
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
      |> List.to_seq
      |> Utils.seq_mapi (fun i (proc: proc_intermediate) ->
          { name = (fun buf -> bprintf buf "PROC%d" i);
            num_params = proc.num_params;
            local = write_local proc.local;
            is_closure = proc.local.lctx.is_closure })
      |> Array.of_seq;

    main = write_local main;

    after_main = Writer.empty;
  }

let write_access_string id =
  fun buf -> bprintf buf "&STR%d" id
