let input_line_opt ch =
  try Some (input_line ch)
  with End_of_file -> None

let input_all_lines ch =
  let rec loop lines =
    match input_line_opt ch with
    | Some line -> loop (line :: lines)
    | None -> List.rev lines
  in loop []

let input_all ch =
  input_all_lines ch |> String.concat "\n"

module String = struct
  include String

  let remove_prefix prefix s =
    let pl = length prefix in
    let sl = length s in
    if pl > sl then
      None
    else
      let rec loop i =
        if i >= pl then
          Some (sub s pl (sl - pl))
        else if prefix.[i] <> s.[i] then
          None
        else loop (i + 1)
      in loop 0
end

type state = {
  line_no: int;
  category: string;
  idx: int;
  code: string list;
  stdout: string list;
  expect: [`Ok | `CompileErr | `RuntimeErr of string];
}

exception Syntax_err of string
exception Test_err of string * string option

let test_compile c_code =
  let gen_file = open_out "out.c" in
  output_string gen_file c_code;
  close_out gen_file;
  (match Unix.system "cc -fsanitize=undefined -I../runtime/include out.c -c -o out.o" with
   | Unix.WEXITED 0 -> ()
   | _ -> raise (Test_err ("failed to compile C", None)));
  (match Unix.system "cc -fsanitize=undefined out.o chemic.o" with
   | Unix.WEXITED 0 -> ()
   | _ -> raise (Test_err ("failed to link C with runtime library", None)));
  let proc = Unix.open_process_full "./a.out" [||] in
  let (stdout, stdin, stderr) = proc in
  close_out stdin;
  let stdout_contents = input_all stdout in
  let stderr_contents = input_all stderr in
  match Unix.close_process_full proc with
  | Unix.WEXITED 0 -> (stdout_contents, `Ok)
  | _ -> (stdout_contents, `RuntimeErr stderr_contents)

let try_do_test state =
  let code = state.code |> List.rev in
  let expect_output = state.stdout |> List.rev |> String.concat "\n" in
  (* attempt to compile the test contents into C code *)
  let proc = Unix.open_process_full "../_build/default/main.bc" [||] in
  let (stdout, stdin, stderr) = proc in
  List.iter (Printf.fprintf stdin "%s\n") code;
  close_out stdin;
  let stdout_contents = input_all stdout in
  let stderr_contents = input_all stderr in
  let ok = match Unix.close_process_full proc with
      Unix.WEXITED 0 -> true | _ -> false in

  (match (ok, state.expect) with
   | false, `CompileErr -> ()
   | false, _ ->
     raise (Test_err ("failed to compile", Some stderr_contents))
   | true, `CompileErr ->
     raise (Test_err ("expected compile error but didn't get one", None))
   (* attempt to compile and run the generated C code *)
   | true, `Ok ->
     (match test_compile stdout_contents with
      | output, `Ok ->
        if not (String.equal expect_output output) then
          raise (Test_err ("unexpected output", Some output))
      | _, `RuntimeErr error ->
        raise (Test_err ("unexpected runtime error", Some error)))
   | true, `RuntimeErr expect_error ->
     (match test_compile stdout_contents with
      | _, `Ok ->
        raise (Test_err ("expected runtime error but didn't get one", None))
      | output, `RuntimeErr error ->
        if not (String.equal expect_error error) then
          raise (Test_err ("expected runtime error, got different runtime error", Some error));
        if not (String.equal expect_output output) then
          raise (Test_err ("unexpected output", Some output)))
  );
  { state with code = [];
               stdout = [];
               expect = `Ok;
               idx = state.idx + 1 }

let do_test state =
  Printf.printf "%s #%d... %!" state.category (state.idx + 1);
  try
    let state' = try_do_test state in
    print_endline "passed";
    state'
  with Test_err (msg, long_msg) ->
    Printf.printf "failed (%s)\n" msg;
    Option.iter print_endline long_msg;
    state

let handle_line state s =
  let state = { state with line_no = state.line_no + 1 } in
  (* indented lines are treated as directives *)
  match String.remove_prefix "    " s with
  | None ->
    (* lines starting with `##` are treated as category headers *)
    (match String.remove_prefix "## " s with
     | Some category -> { state with category; idx = 0 }
     | None ->
       match state.code with
       | [] -> state
       | _ -> do_test state)

  | Some s ->
    (* the `>` directive indicates code *)
    match String.remove_prefix "> " s with
    | Some content -> { state with code = content :: state.code }
    | None ->
      (* the `=` directive indicates a line of stdout *)
      match String.remove_prefix "= " s with
      | Some content -> { state with stdout = content :: state.stdout }
      | None ->
        (* the `!` directive indicates the result *)
        let expect =
          match String.remove_prefix "! " s with
          | Some "compile error" -> `CompileErr
          | Some msg -> `RuntimeErr msg
          | None -> raise (Syntax_err "unknown directive")
        in
        { state with expect }

let () =
  let in_file = open_in Sys.argv.(1) in
  try
    let rec loop state =
      match try Some (input_line in_file) with End_of_file -> None with
      | Some line -> loop (handle_line state line)
      | None ->
        match state.code with
        | [] -> state
        | _ -> do_test state
    in
    ignore (loop { line_no = 0;
                   category = "<start>";
                   idx = 0;
                   code = [];
                   stdout = [];
                   expect = `Ok })
  with e ->
    close_in_noerr in_file;
    raise e
