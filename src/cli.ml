let input_line_opt ch =
  try Some (input_line ch)
  with End_of_file -> None

let input_all ch =
  let rec loop lines =
    match input_line_opt ch with
    | Some line -> loop (line :: lines)
    | None -> String.concat "\n" (List.rev lines)
  in loop []

let main () =
  let code = input_all stdin in
  let parsed = Parse.parse code in
  let ectx, exprs = Expr.build parsed in
  let compiled = Codegen.gen_code ectx exprs in
  print_string compiled
