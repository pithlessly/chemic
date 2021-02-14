type op = Parse.op =
  | Plus
  | Minus
  | Times

type form = Parse.form =
  | Int of int64
  | Op of op * form list

(* Internally, serialized sub-expressions are represented
 * using functions that add text to a buffer. This enables
 * concatenation in O(1) time using a closure.
 *)
type writer = Buffer.t -> unit

(* Helper functions to generate various types of writers: *)

(* Write a constant string. *)
let write_string s = fun buf -> Buffer.add_string buf s

(* Use `f` to write every element of `xs`, with `sep` before each element. *)
let write_all ~f sep xs =
  fun buf ->
  List.iter (fun x -> write_string sep buf; f x buf) xs

(* Write each element of `ws`. *)
let concat ws buf = List.iter (fun w -> w buf) ws

(* If `cond` is true, surround the writer in parentheses. *)
let paren_if cond writer =
  if cond then
    fun buf ->
      Buffer.add_char buf '(';
      writer buf;
      Buffer.add_char buf ')';
  else writer

(* Precedence table of various expression forms. *)
module Prec = struct
  let n = ref 0
  let iota () =
    let old_n = !n in
    n := old_n + 1;
    old_n

  let add     = iota ()
  let sub_rhs = iota ()
  let times   = iota ()
  let unary   = iota ()
  (* let atom = iota () *)
end

(* Write a form as a C expression. *)
let rec write_form ~prec form: writer =
  match form with
  | Int i ->
    write_string (Int64.to_string i)

  | Op (Plus, []) ->
    write_string "0"
  | Op (Plus, x :: xs) ->
    let f = write_form ~prec:Prec.add in
    paren_if (prec > Prec.add)
      (concat [f x; write_all ~f " + " xs])

  | Op (Minus, [x]) ->
    let f = write_form ~prec:Prec.unary in
    paren_if (prec > Prec.unary)
      (concat [write_string "-"; f x])
  | Op (Minus, x :: xs) ->
    (* the precedence for the subexpressions of the LHS and RHS of
     * subtraction are different, because `a - b - c` is the same
     * as `(a - b) - c` but different from `a - (b - c)` *)
    let f_left = write_form ~prec:Prec.add in
    let f = write_form ~prec:Prec.sub_rhs in
    paren_if (prec > Prec.add)
      (concat [
          f_left x;
          write_all ~f " - " xs;
        ])

  | Op (Times, []) ->
    write_string "1"
  | Op (Times, x :: xs) ->
    let f = write_form ~prec:Prec.times in
    paren_if (prec > Prec.times)
      (concat [f x; write_all ~f " * " xs])

  | _ -> raise (Invalid_argument "invalid expression")

(* Convert a form into a C program. *)
let gen_code form =
  let buf = Buffer.create 2048 in
  let emit = Buffer.add_string buf in
  emit "#include <stdio.h>\n";
  emit "int main() {\n";
  emit "    printf(\"%d\", ";
  write_form ~prec:0 form buf;
  emit ");\n";
  emit "    return 0;\n";
  emit "}\n";
  Buffer.contents buf
