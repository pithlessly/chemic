type op = Parse.op =
  | Plus
  | Minus
  | Times
  | Comma

type form = Parse.form =
  | Int of int64
  | String of string
  | Op of op * form list

let bprintf = Printf.bprintf

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

let write_string_literal buf s =
  let len = String.length s in
  let rec loop n =
    if n < len then (
      let c = String.get s n in
      if ' ' <= c && c <= '~' then (
        if c = '\\' || c = '"' then
          Buffer.add_char buf '\\';
        Buffer.add_char buf c
      ) else
        bprintf buf "\\%02x" (Char.code c);
      loop (n + 1)
    )
  in
  Buffer.add_char buf '"';
  loop 0;
  Buffer.add_char buf '"'

let rec write_form buf =
  function
  | Int i ->
    bprintf buf "make_int(%Ld)" i;

  | String s ->
    bprintf buf "make_string(%a,%d)"
      write_string_literal s
      (String.length s)

  | Op (Plus, []) ->
    Buffer.add_char buf '0'
  | Op (Plus, x :: xs) ->
    write_nested ~f:write_form buf "add" x xs

  | Op (Minus, [x]) ->
    bprintf buf "neg(%a)" write_form x
  | Op (Minus, x :: xs) ->
    write_nested ~f:write_form buf "sub" x xs

  | Op (Times, []) ->
    Buffer.add_char buf '1'
  | Op (Times, x :: xs) ->
    write_nested ~f:write_form buf "mul" x xs

  | Op (Comma, [x]) ->
    bprintf buf "len(%a)" write_form x

  | Op (Comma, _)
  | Op (Minus, []) ->
    raise (Invalid_argument "invalid expression")

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
