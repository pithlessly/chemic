module StringMap = Map.Make(String)
let bprintf = Printf.bprintf
let impossible () = invalid_arg "impossible"

type t = int

type num_args =
  | Exactly of int
  | AtLeast of int

type static_impl = { args: num_args; impl: args: (Writer.t list) -> out: Writer.t -> Writer.t }
type dynamic_impl = { args: num_args; name: Writer.t; body: Writer.t }

type dynamic_impl_cache = bool array

let (bindings, (all_ops: (static_impl * dynamic_impl) array), num_ops) =
  let bindings = ref StringMap.empty in
  let all_ops = ref [] in
  let n = ref 0 in

  let add_op ~name ?(ident = name) ~args ~static ~dynamic () =
    let body = fun buf -> dynamic |> List.iter (bprintf buf "  %s\n") in
    let static = { args; impl = static } in
    let dynamic = { args; name = (fun buf -> bprintf buf "dynop_%s" ident); body } in
    all_ops := (static, dynamic) :: !all_ops;
    bindings := StringMap.add name !n !bindings;
    n := !n + 1
  in

  let add_action ~name ?(ident = name) ~body () =
    add_op ~name ~ident ~args:(Exactly 0)
      ~static: (fun ~args:_ ~out ->
          fun buf -> bprintf buf "%sMAKE_NIL(%t);" body out)
      ~dynamic: [body; "return NIL;"]
      ()
  in

  let add_unary_op ~name ?(ident = name) ~fn () =
    add_op ~name ~ident ~args:(Exactly 1)
      ~static: (fun [@warning "-8"] ~args:[a] ~out ->
          fun buf -> bprintf buf "%t=%s(%t);" out fn a)
      ~dynamic: [Printf.sprintf "return %s(C_ARG(0));" fn]
      ()
  in

  let add_binary_op ~name ?(ident = name) ~fn () =
    add_op ~name ~ident ~args:(Exactly 2)
      ~static: (fun [@warning "-8"] ~args:[a; b] ~out ->
          fun buf -> bprintf buf "%t=%s(%t,%t);" out fn a b)
      ~dynamic: [Printf.sprintf "return %s(C_ARG(0),C_ARG(1));" fn]
      ()
  in

  add_binary_op ~name:"eqv?" ~ident:"eqv_q" ~fn:"eqv_q" ();

  add_binary_op ~name:"<" ~ident:"less_than" ~fn:"less_than" ();

  add_op ~name:"+" ~ident:"add" ~args:(AtLeast 0)
    ~static: (fun ~args ~out ->
        match args with
        | [] -> fun buf -> bprintf buf "MAKE_INT(%t,0);" out
        | [_] -> fun buf -> bprintf buf "EXPECT(%t,tag_int);" out
        | _ :: rhss ->
          fun buf ->
            List.iter (bprintf buf "%t=add(%t,%t);" out out) rhss)
    ~dynamic: [
      "Obj a;";
      "MAKE_INT(a, 0);";
      "for (size_t i = 0; i < var_args; i++)";
      "  a = add(a, C_ARG(i));";
      "return a;";
    ] ();

  add_op ~name:"*" ~ident:"mul" ~args:(AtLeast 0)
    ~static: (fun ~args ~out ->
        match args with
        | [] -> fun buf -> bprintf buf "MAKE_INT(%t,1);" out
        | [_] -> fun buf -> bprintf buf "EXPECT(%t,tag_int);" out
        | _ :: rhss ->
          fun buf ->
            List.iter (bprintf buf "%t=mul(%t,%t);" out out) rhss)
    ~dynamic: [
      "Obj a;";
      "MAKE_INT(a, 1);";
      "for (size_t i = 0; i < var_args; i++)";
      "  a = mul(a, C_ARG(i));";
      "return a;";
    ] ();

  add_op ~name:"-" ~ident:"sub" ~args:(AtLeast 1)
    ~static: (fun ~args ~out ->
        match args with
        | [] -> impossible ()
        | [a] -> fun buf -> bprintf buf "%t=neg(%t);" out a
        | _ :: rhss ->
          fun buf ->
            List.iter (bprintf buf "%t=sub(%t,%t);" out out) rhss)
    ~dynamic: [
      "Obj a = C_ARG(0);";
      "for (size_t i = 1; i < var_args; i++)";
      "  a = sub(a, C_ARG(i));";
      "return var_args == 1 ? neg(a) : a;";
    ] ();

  add_binary_op ~name:"cons" ~fn:"cons" ();

  add_unary_op ~name:"car" ~fn:"car" ();
  add_unary_op ~name:"cdr" ~fn:"cdr" ();

  add_binary_op ~name:"set-car!" ~ident:"set_car" ~fn:"set_car" ();
  add_binary_op ~name:"set-cdr!" ~ident:"set_cdr" ~fn:"set_cdr" ();

  (let add_unary_chain ~name fns =
     let begin_call = Buffer.create 0 in
     let end_call = Buffer.create 0 in
     fns |> List.iter (fun fn ->
         Buffer.add_string begin_call fn;
         Buffer.add_char begin_call '(';
         Buffer.add_char end_call ')');
     let begin_call = Buffer.contents begin_call in
     let end_call = Buffer.contents end_call in
     add_op ~name ~args:(Exactly 1)
       ~static: (fun [@warning "-8"] ~args:[a] ~out ->
           fun buf -> bprintf buf "%t=%s%t%s;" out begin_call a end_call)
       ~dynamic: [Printf.sprintf "return %sC_ARG(0)%s;" begin_call end_call]
       ();
   in

   add_unary_chain ~name:"caar" ["car"; "car"];
   add_unary_chain ~name:"cdar" ["cdr"; "car"];
   add_unary_chain ~name:"cadr" ["car"; "cdr"];
   add_unary_chain ~name:"cddr" ["cdr"; "cdr"];

   add_unary_chain ~name:"caaar" ["car"; "car"; "car"];
   add_unary_chain ~name:"cdaar" ["cdr"; "car"; "car"];
   add_unary_chain ~name:"cadar" ["car"; "cdr"; "car"];
   add_unary_chain ~name:"cddar" ["cdr"; "cdr"; "car"];
   add_unary_chain ~name:"caadr" ["car"; "car"; "cdr"];
   add_unary_chain ~name:"cdadr" ["cdr"; "car"; "cdr"];
   add_unary_chain ~name:"caddr" ["car"; "cdr"; "cdr"];
   add_unary_chain ~name:"cdddr" ["cdr"; "cdr"; "cdr"];

   add_unary_chain ~name:"caaaar" ["car"; "car"; "car"; "car"];
   add_unary_chain ~name:"cdaaar" ["cdr"; "car"; "car"; "car"];
   add_unary_chain ~name:"cadaar" ["car"; "cdr"; "car"; "car"];
   add_unary_chain ~name:"cddaar" ["cdr"; "cdr"; "car"; "car"];
   add_unary_chain ~name:"caadar" ["car"; "car"; "cdr"; "car"];
   add_unary_chain ~name:"cdadar" ["cdr"; "car"; "cdr"; "car"];
   add_unary_chain ~name:"caddar" ["car"; "cdr"; "cdr"; "car"];
   add_unary_chain ~name:"cdddar" ["cdr"; "cdr"; "cdr"; "car"];
   add_unary_chain ~name:"caaadr" ["car"; "car"; "car"; "cdr"];
   add_unary_chain ~name:"cdaadr" ["cdr"; "car"; "car"; "cdr"];
   add_unary_chain ~name:"cadadr" ["car"; "cdr"; "car"; "cdr"];
   add_unary_chain ~name:"cddadr" ["cdr"; "cdr"; "car"; "cdr"];
   add_unary_chain ~name:"caaddr" ["car"; "car"; "cdr"; "cdr"];
   add_unary_chain ~name:"cdaddr" ["cdr"; "car"; "cdr"; "cdr"];
   add_unary_chain ~name:"cadddr" ["car"; "cdr"; "cdr"; "cdr"];
   add_unary_chain ~name:"cddddr" ["cdr"; "cdr"; "cdr"; "cdr"];
  );

  add_unary_op ~name:"string?" ~ident:"string_q" ~fn:"string_q" ();
  add_unary_op ~name:"string-copy" ~ident:"string_copy" ~fn:"string_copy" ();
  add_unary_op ~name:"string-length" ~ident:"string_length" ~fn:"string_length" ();

  add_op ~name:"display" ~args:(Exactly 1)
    ~static: (fun ~args ~out buf -> bprintf buf "display(%t);MAKE_NIL(%t);" (List.hd args) out)
    ~dynamic: ["display(C_ARG(0));"; "return NIL;"] ();

  add_action ~name:"dbg" ~body:"gc_debug()" ();
  add_action ~name:"gc-collect" ~ident:"gc_collect" ~body:"gc_collect();" ();

  (!bindings, Array.of_list (List.rev !all_ops), !n)

let lookup ident = StringMap.find_opt ident bindings

let static_use id = all_ops.(id) |> fst
let init_cache () = Array.make num_ops false
let dynamic_use cache id =
  cache.(id) <- true;
  let (_, dyn) = all_ops.(id) in
  dyn.name

let get_dynamic_used cache =
  Array.to_seq all_ops
  |> Utils.seq_mapi (fun i (_, dyn_impl) -> (cache.(i), dyn_impl))
  |> Seq.filter_map (fun (used, impl) -> if used then Some impl else None)
  |> List.of_seq
