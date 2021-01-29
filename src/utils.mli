val ostr : string option -> string
val esc : string -> string
val int_of_hexchar : char -> int
val hex_of_big_int : int -> int -> string
val big_int_of_hex : string -> int
val split_string : char -> string -> string list
val ivy : bool ref
val report : bool ref
val sexpr : bool ref
val latex : bool ref
val fancy : bool ref
val html : bool ref
val base58 : int -> string
val frombase58 : string -> int
val prover9style : bool ref
val tptpstyle : bool ref
