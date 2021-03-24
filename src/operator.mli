type num_args =
  | Exactly of int
  | AtLeast of int

type t = {
  args: num_args;
  (* Takes a list of input registers and an output register; should generate
   * code that uses the inputs to compute the output and stores it there. It is
   * guaranteed that 'List.hd args' will be the same as 'out', if it exists. *)
  impl: args: (Writer.t list) -> out: Writer.t -> Writer.t;
}

val lookup: string -> t option
