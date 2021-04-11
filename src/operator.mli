(* An abstract type representing an operator. *)
type t
val lookup: string -> t option

(* Represents the constraints on the number of arguments passed: *)
type num_args =
  (* It must be passed a fixed number of arguments *)
  | Exactly of int
  (* It must be passed at least a particular number of arguments *)
  | AtLeast of int

(* Information needed for static dispatch *)
type static_impl =
  { args: num_args;
    (* Takes a list of input registers and an output register; should generate
     * code that uses the inputs to compute the output and stores it there. It is
     * guaranteed that 'List.hd args' will be the same as 'out', if it exists. *)
    impl: args: (Writer.t list) -> out: Writer.t -> Writer.t }

val static_use: t -> static_impl

(* Information needed to generate the implementation used for dynamic dispatch *)
type dynamic_impl =
  { args: num_args;
    name: Writer.t;
    body: Writer.t }

(* Caches information about whether each dynamic definition needs to be included *)
type dynamic_impl_cache

val init_cache: unit -> dynamic_impl_cache
val dynamic_use: dynamic_impl_cache -> t -> Writer.t (* returns an identifier *)
val get_dynamic_used: dynamic_impl_cache -> dynamic_impl list
