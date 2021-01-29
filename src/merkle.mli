type merkl =
  | MerkN of merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option
  | MerkL of string
  | MerkH of string
    
exception MerkleH of string

val merkle_root : merkl option -> string option
val merkle_insert : merkl option -> string -> string -> merkl option
val merkle_lookup : merkl option -> string -> string

val supporting_merkle_tree : string list -> merkl option -> merkl option

val sha256reg : string -> string

val save_merkl : out_channel -> merkl option -> unit
val load_merkl : in_channel -> merkl option
