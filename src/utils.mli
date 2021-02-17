(* create a sequence of a given length by applying a function to the index *)
val seq_init: int -> (int -> 'a) -> 'a Seq.t

(* map a function over a sequence with access to the index *)
val seq_mapi: (int -> 'a -> 'b) -> 'a Seq.t -> 'b Seq.t
