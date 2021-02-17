type t = Buffer.t -> unit

val empty: t
val join: char -> t Seq.t -> t
val c_string: string -> t (* encode a string to the buffer as a C string literal *)
