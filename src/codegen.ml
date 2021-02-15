type op = Parse.op =
  | Plus
  | Minus
  | Times

type form = Parse.form =
  | Int of int64
  | Op of op * form list

let write_nested buf name ~f init xs =
  let rec loop = function
    | [] -> f buf init
    | x :: xs ->
      Buffer.add_string buf name;
      Buffer.add_char buf '(';
      loop xs;
      Buffer.add_char buf ',';
      f buf x;
      Buffer.add_char buf ')';
  in loop (List.rev xs)

let rec write_form buf =
  function
  | Int i ->
    Buffer.add_string buf (Int64.to_string i)

  | Op (Plus, []) ->
    Buffer.add_char buf '0'
  | Op (Plus, x :: xs) ->
    write_nested ~f:write_form buf "add" x xs

  | Op (Minus, [x]) ->
    Buffer.add_string buf "neg(";
    write_form buf x;
    Buffer.add_string buf ")";
  | Op (Minus, x :: xs) ->
    write_nested ~f:write_form buf "sub" x xs

  | Op (Times, []) ->
    Buffer.add_char buf '1'
  | Op (Times, x :: xs) ->
    write_nested ~f:write_form buf "mul" x xs

  | _ -> raise (Invalid_argument "invalid expression")

(* Convert a form into a C program. *)
let gen_code forms =
  let buf = Buffer.create 2048 in
  let emit = Buffer.add_string buf in
  emit "#include \"chemic.h\"\n";
  emit "int main() {\n";
  forms |> List.iter (fun form ->
      emit "    print(";
      write_form buf form;
      emit ");\n";
    );
  emit "    return 0;\n";
  emit "}\n";
  Buffer.contents buf

(*
(* Write a form as a C expression. *)
let rec write_form ~prec form: writer =
  match form with
  | Op (Times, []) ->
    write_string "1"
  | Op (Times, x :: xs) ->
    let f = write_form ~prec:Prec.times in
    paren_if (prec > Prec.times)
      (concat [f x; write_all ~f " * " xs])

  | _ -> raise (Invalid_argument "invalid expression")

   *)
