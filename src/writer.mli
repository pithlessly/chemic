type t = Buffer.t -> unit

val empty: t
val join: char -> t Seq.t -> t
