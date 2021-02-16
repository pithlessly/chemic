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
