(* takes a generator `f` (that transforms state and returns an intermediate value) and
 * returns the final state and a list of all intermediate values *)
val unfold: ('a -> 'b option * 'a) -> 'a -> 'b list * 'a

(* create a sequence of a given length by applying a function to the index *)
val seq_init: int -> (int -> 'a) -> 'a Seq.t

(* map a function over a sequence with access to the index *)
val seq_mapi: (int -> 'a -> 'b) -> 'a Seq.t -> 'b Seq.t

(* iterate over a sequence with access to the index *)
val seq_iteri: (int -> 'a -> unit) -> 'a Seq.t -> unit

(* create a sequence that returns repeated copies of a value *)
val seq_replicate: int -> 'a -> 'a Seq.t

(* return the first 'Some _' value returned by the function for an element in the list *)
val search: ('a -> 'b option) -> 'a list -> 'b option

(* determine if a list is empty *)
val null: 'a list -> bool
