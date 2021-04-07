type t = Buffer.t -> unit

let empty _ = ()

let join sep items buf =
  let do_sep = ref false in
  Seq.iter (fun item ->
      if !do_sep then
        Buffer.add_char buf sep;
      do_sep := true;
      item buf
    ) items

let c_string s buf =
  let len = String.length s in
  let rec loop n =
    if n < len then (
      let c = String.get s n in
      if ' ' <= c && c <= '~' then (
        if c = '\\' || c = '"' then
          Buffer.add_char buf '\\';
        Buffer.add_char buf c
      ) else
        (* Previously we used hex escapes, but this would cause us to generate
         * strings like "\x0ab", which is invalid because C allows hex escapes
         * to be more than 2 characters for some reason. Instead, we use octal
         * escapes, which are guaranteed to be a maximum of 3 characters. *)
        Printf.bprintf buf "\\%03o" (Char.code c);
      loop (n + 1)
    )
  in
  Buffer.add_char buf '"';
  loop 0;
  Buffer.add_char buf '"'
