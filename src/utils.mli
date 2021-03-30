(* takes a generator `f` (that transforms state and returns an intermediate value) and
 * returns the final state and a list of all intermediate values *)
val unfold: ('a -> 'b option * 'a) -> 'a -> 'b list * 'a

(* Like `List.iter`, but only visit a limited number of elements *)
val iter_max: ('a -> unit) -> int -> 'a list -> unit

(* Creates two lists out of the elements returned by the function *)
val unzip_with: ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list

(* map a function over a sequence with access to the index *)
val seq_mapi: (int -> 'a -> 'b) -> 'a Seq.t -> 'b Seq.t

(* iterate over a sequence with access to the index *)
val seq_iteri: (int -> 'a -> unit) -> 'a Seq.t -> unit

(* create a sequence that returns repeated copies of a value *)
val seq_replicate: int -> 'a -> 'a Seq.t

(* return the first 'Some _' value returned by the function for an element in
 * the list, along with its index *)
val search: ('a -> 'b option) -> 'a list -> (int * 'b) option

(* determine if a list is empty *)
val null: 'a list -> bool

(* modify only the first element of a list *)
val map_hd: ('a -> 'a) -> 'a list -> 'a list
