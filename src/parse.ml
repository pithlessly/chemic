(* utility function that takes a generator `f` (that transforms state and
 * returns an intermediate value) and returns the final state and a list
 * of all intermediate values *)

let unfold ~(f: 'a -> 'b option * 'a) (init: 'a): 'b list * 'a =
  let rec go acc xs =
    match f acc with
    | Some x, acc -> go acc (x :: xs)
    | None, acc -> (List.rev xs, acc)
  in go init []

(* classifications of characters *)

let is_space c =
  c = ' ' || c = '\n' || c = '\r' || c = '\t'

let is_digit c =
  '0' <= c && c <= '9'

(* the longest number of characters possible in a 64-bit integer literal *)

let max_literal_len = Int64.(max_int |> to_string) |> String.length

(* types internal to the parsing process *)

type state = { code: string; pos: int }

let current_char { code; pos } =
  if pos < String.length code then
    Some (String.get code pos)
  else
    None

let advance { code; pos } = { code; pos = pos + 1 }

exception Parse_error of int * string

let fail { pos; _ } msg = raise (Parse_error (pos, msg))

(* types for representing a parsed program *)

type op =
  | Plus
  | Minus
  | Times
  | Len
  | Register

type form =
  | Int of int64
  | String of string
  | Op of op * form list

(* incremental parsing functions which return the new state *)

let parse_num state: int64 * state =
  let digits = Buffer.create max_literal_len in
  let rec loop state =
    match current_char state with
    | Some c when is_digit c ->
      Buffer.add_char digits c;
      loop (advance state)
    | _ -> state
  in
  let state = loop state in
  match digits |> Buffer.contents |> Int64.of_string_opt with
  | Some n -> (n, state)
  | None -> fail state "integer literal too large"

let parse_string state: string * state =
  let chars = Buffer.create max_literal_len in
  let rec loop state =
    match current_char state with
    | Some '"' -> state
    | Some c ->
      Buffer.add_char chars c;
      loop (advance state)
    | None -> fail state "expected string literal, got EOF"
  in
  let state = loop state in
  (Buffer.contents chars, advance state)

let rec parse_op state: op * state =
  let make_op o = (o, advance state) in
  match current_char state with
  | Some '+' -> make_op Plus
  | Some '-' -> make_op Minus
  | Some '*' -> make_op Times
  | Some ',' -> make_op Len
  | Some '@' -> make_op Register
  | Some c when is_space c -> parse_op (advance state)
  | Some c -> fail state (Printf.sprintf "invalid operator: '%s'" (Char.escaped c))
  | None -> fail state "expected operator, got EOF"

let rec try_parse ~(top_level: bool) state: form option * state =
  match current_char state with
  | None ->
    if top_level
    then (None, state)
    else fail state "expected form, got EOF"
  | Some ')' ->
    if top_level
    then fail state "expected form, got )"
    else (None, advance state)
  | Some '(' ->
    let op, state = parse_op (advance state) in
    let args, state = parse_many ~top_level:false state in
    (Some (Op (op, args)), state)
  | Some '"' ->
    let s, state = parse_string (advance state) in
    (Some (String s), state)
  | Some c when is_digit c ->
    let n, state = parse_num state in
    (Some (Int n), state)
  | Some c when is_space c ->
    try_parse ~top_level (advance state)
  | Some c -> fail state (Printf.sprintf "unrecognized character '%s'"
                            (Char.escaped c))

and parse_many ~top_level state: form list * state =
  unfold ~f:(try_parse ~top_level) state

(* the global parsing function *)

let parse code =
  parse_many ~top_level:true { code; pos = 0 } |> fst
