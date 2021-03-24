module StringMap = Map.Make(String)
let bprintf = Printf.bprintf
let impossible () = raise (Invalid_argument "impossible")

type num_args =
  | Exactly of int
  | AtLeast of int

type t = {
  args: num_args;
  proc_ident: string;
  impl: args: (Writer.t list) -> out: Writer.t -> Writer.t;
}

let make_nullary ~proc_ident (impl: Writer.t -> Writer.t) =
  { args = Exactly 0;
    proc_ident;
    impl = fun ~args ~out buf ->
      match args with
      | [] -> impl out buf
      | _ -> impossible () }

let make_nullary_nil ~proc_ident (impl: string) =
  make_nullary ~proc_ident (fun out buf -> bprintf buf "MAKE_NIL(%t);%s" out impl)

let make_unary ~proc_ident (impl: Writer.t -> Writer.t -> Writer.t) =
  { args = Exactly 1;
    proc_ident;
    impl = fun ~args ~out buf ->
      match args with
      | [a] -> impl out a buf
      | _ -> impossible () }

let make_binary ~proc_ident (impl: Writer.t -> Writer.t -> Writer.t -> Writer.t) =
  { args = Exactly 2;
    proc_ident;
    impl = fun ~args ~out buf ->
      match args with
      | [a; b] -> impl out a b buf
      | _ -> impossible () }

let all_ops =
  StringMap.empty

  |> StringMap.add "+"
    { args = AtLeast 0;
      proc_ident = "add";
      impl = fun ~args ~out buf ->
        match args with
        | [] -> bprintf buf "MAKE_INT(%t,0);" out
        | _ :: rhss ->
          List.iter (bprintf buf "%t=add(%t,%t);" out out) rhss }

  |> StringMap.add "-"
    { args = AtLeast 1;
      proc_ident = "sub";
      impl = fun ~args ~out buf ->
        match args with
        | [] -> impossible ()
        | [x] -> bprintf buf "%t=neg(%t);" out x
        | _ :: rhss ->
          List.iter (bprintf buf "%t=sub(%t,%t);" out out) rhss }

  |> StringMap.add "*"
    { args = AtLeast 0;
      proc_ident = "mul";
      impl = fun ~args ~out buf ->
        match args with
        | [] -> bprintf buf "MAKE_INT(%t,1);" out
        | _ :: rhss ->
          List.iter (bprintf buf "%t=mul(%t,%t);" out out) rhss }

  (* TODO: support multiple arguments *)
  |> StringMap.add "<"
    (make_binary ~proc_ident:"less_than"
       (fun out a b buf -> bprintf buf "%t=less_than(%t,%t);" out a b))

  |> StringMap.add "len"
    (make_unary ~proc_ident:"len"
       (fun out a buf -> bprintf buf "%t=len(%t);" out a))

  |> StringMap.add "display"
    (make_unary ~proc_ident:"display"
       (fun _ a buf -> bprintf buf "display(%t);" a))

  |> StringMap.add "cons"
    (make_binary ~proc_ident:"cons"
       (fun out a b buf -> bprintf buf "%t=cons(%t,%t);" out a b))

  |> StringMap.add "call"
    { args = AtLeast 1;
      proc_ident = "call";
      impl = fun ~args ~out buf ->
        match args with
        | [] -> impossible ()
        | f :: args ->
          bprintf buf "arg_init(%d);" (List.length args);
          args |> List.iter (bprintf buf "arg_push(%t);");
          bprintf buf "%t=call(%t);" out f }

  |> StringMap.add "dbg"
    (make_nullary_nil ~proc_ident:"dbg" "gc_debug();")

  |> StringMap.add "gc-collect"
    (make_nullary_nil ~proc_ident:"gc_collect" "gc_collect();")

let lookup name = StringMap.find_opt name all_ops
