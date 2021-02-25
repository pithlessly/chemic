(* classifications of characters *)

let is_space c =
  c = ' ' || c = '\n' || c = '\r' || c = '\t'

let is_digit c =
  '0' <= c && c <= '9'

let is_ident c =
  is_digit c ||
  ('a' <= c && c <= 'z') ||
  ('A' <= c && c <= 'Z') ||
  List.mem c ['!'; '$'; '%'; '&'; '*'; '/'; ':'; '<'; '=';
              '>'; '?'; '^'; '_'; '~'; '+'; '-'; '.'; '@']

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

type form =
  | Int of int64
  | String of string
  | Ident of string
  | List of form list

(* incremental parsing functions which return the new state *)

let rec skip_whitespace state: state =
  match current_char state with
  | Some c when is_space c ->
    skip_whitespace (advance state)
  | _ -> state

let parse_ident_like state: form * state =
  let chars = Buffer.create max_literal_len in
  let numeric = ref true in
  let rec loop state =
    match current_char state with
    | Some c when is_ident c ->
      if not (is_digit c) then
        numeric := false;
      Buffer.add_char chars c;
      loop (advance state)
    | _ -> state
  in
  let state = loop state in
  let chars = Buffer.contents chars in
  if String.length chars = 0 then
    fail state "expected identifier, got EOF"
  else if not !numeric then
    (Ident chars, state)
  else
    match Int64.of_string_opt chars with
    | Some n -> (Int n, state)
    | None -> fail state "integer literal too large"

let parse_string state: string * state =
  let chars = Buffer.create max_literal_len in
  let add = Buffer.add_char chars in
  let rec loop escaped state =
    match escaped, current_char state with
    | _, None          -> fail state "unexpected end of string literal"
    | false, Some '"'  -> advance state
    | false, Some '\\' -> loop true (advance state)
    | false, Some c    -> add c;      loop false (advance state)
    | true, Some '"'   -> add '"';    loop false (advance state)
    | true, Some '\\'  -> add '\\';   loop false (advance state)
    | true, Some 'n'   -> add '\n';   loop false (advance state)
    | true, Some 't'   -> add '\t';   loop false (advance state)
    | true, Some 'r'   -> add '\r';   loop false (advance state)
    | true, Some '0'   -> add '\000'; loop false (advance state)
    | true, Some c     -> fail state (Printf.sprintf "invalid escape sequence: \\%c" c)
  in
  let state = loop false state in
  (Buffer.contents chars, state)

let rec try_parse ~(top_level: bool) state: form option * state =
  let state = skip_whitespace state in
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
    let children, state = parse_many ~top_level:false (advance state) in
    (Some (List children), state)
  | Some '"' ->
    let s, state = parse_string (advance state) in
    (Some (String s), state)
  | Some c when is_ident c ->
    let form, state = parse_ident_like state in
    (Some form, state)
  | Some c -> fail state (Printf.sprintf "unrecognized character '%s'"
                            (Char.escaped c))

and parse_many ~top_level state: form list * state =
  Utils.unfold (try_parse ~top_level) state

(* the global parsing function *)

let parse code =
  parse_many ~top_level:true { code; pos = 0 } |> fst
