open Utils
open Sha256
  
(***
   Merkle tree with 16 branches for each node,
   using the hex char from the hash string, with relevant information stored as a string at the leaf
   ***)
type merkl =
  | MerkN of merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option * merkl option
  | MerkL of string
  | MerkH of string

let rec merkle_root m =
  match m with
  | None -> None
  | Some(MerkH(x)) -> Some(x)
  | Some(MerkL(x)) -> Some(sha256 x)
  | Some(MerkN(None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None)) -> None
  | Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) ->
      Some(sha256
	     (Printf.sprintf "MerkNode:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s:%s"
		(ostr (merkle_root m0)) (ostr (merkle_root m1)) (ostr (merkle_root m2)) (ostr (merkle_root m3))
		(ostr (merkle_root m4)) (ostr (merkle_root m5)) (ostr (merkle_root m6)) (ostr (merkle_root m7))
		(ostr (merkle_root m8)) (ostr (merkle_root m9)) (ostr (merkle_root m10)) (ostr (merkle_root m11))
		(ostr (merkle_root m12)) (ostr (merkle_root m13)) (ostr (merkle_root m14)) (ostr (merkle_root m15))))

let merkle_remember_lookedup : bool ref = ref false
let merkle_lookedup : (string,unit) Hashtbl.t = Hashtbl.create 100

exception MerkleH of string

let merkle_lookup m h =
  if !merkle_remember_lookedup then Hashtbl.add merkle_lookedup h ();
  let rec merkle_lookup_1 i m h =
    match m with
    | None -> raise Not_found
    | Some(MerkH(x)) -> raise (MerkleH(x)) (*** not enough info in the Merkle tree to do the lookup ***)
    | Some(MerkL(x)) -> x
    | Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) ->
	match h.[i] with
	| '0' -> merkle_lookup_1 (i+1) m0 h
	| '1' -> merkle_lookup_1 (i+1) m1 h
	| '2' -> merkle_lookup_1 (i+1) m2 h
	| '3' -> merkle_lookup_1 (i+1) m3 h
	| '4' -> merkle_lookup_1 (i+1) m4 h
	| '5' -> merkle_lookup_1 (i+1) m5 h
	| '6' -> merkle_lookup_1 (i+1) m6 h
	| '7' -> merkle_lookup_1 (i+1) m7 h
	| '8' -> merkle_lookup_1 (i+1) m8 h
	| '9' -> merkle_lookup_1 (i+1) m9 h
	| 'a' -> merkle_lookup_1 (i+1) m10 h
	| 'b' -> merkle_lookup_1 (i+1) m11 h
	| 'c' -> merkle_lookup_1 (i+1) m12 h
	| 'd' -> merkle_lookup_1 (i+1) m13 h
	| 'e' -> merkle_lookup_1 (i+1) m14 h
	| 'f' -> merkle_lookup_1 (i+1) m15 h
	| _ -> raise (Failure "non hex char")
  in
  merkle_lookup_1 0 m h

let merkle_insert m k v =
  let rec merkle_insert_0 i k v =
    if i < 64 then
      let mi = Some(merkle_insert_0 (i+1) k v) in
      match k.[i] with
      | '0' -> MerkN(mi,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None)
      | '1' -> MerkN(None,mi,None,None,None,None,None,None,None,None,None,None,None,None,None,None)
      | '2' -> MerkN(None,None,mi,None,None,None,None,None,None,None,None,None,None,None,None,None)
      | '3' -> MerkN(None,None,None,mi,None,None,None,None,None,None,None,None,None,None,None,None)
      | '4' -> MerkN(None,None,None,None,mi,None,None,None,None,None,None,None,None,None,None,None)
      | '5' -> MerkN(None,None,None,None,None,mi,None,None,None,None,None,None,None,None,None,None)
      | '6' -> MerkN(None,None,None,None,None,None,mi,None,None,None,None,None,None,None,None,None)
      | '7' -> MerkN(None,None,None,None,None,None,None,mi,None,None,None,None,None,None,None,None)
      | '8' -> MerkN(None,None,None,None,None,None,None,None,mi,None,None,None,None,None,None,None)
      | '9' -> MerkN(None,None,None,None,None,None,None,None,None,mi,None,None,None,None,None,None)
      | 'a' -> MerkN(None,None,None,None,None,None,None,None,None,None,mi,None,None,None,None,None)
      | 'b' -> MerkN(None,None,None,None,None,None,None,None,None,None,None,mi,None,None,None,None)
      | 'c' -> MerkN(None,None,None,None,None,None,None,None,None,None,None,None,mi,None,None,None)
      | 'd' -> MerkN(None,None,None,None,None,None,None,None,None,None,None,None,None,mi,None,None)
      | 'e' -> MerkN(None,None,None,None,None,None,None,None,None,None,None,None,None,None,mi,None)
      | 'f' -> MerkN(None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,mi)
      | _ -> raise (Failure "non hex char")
    else
      MerkL(v)
  in
  let rec merkle_insert_1 i m k v =
    match m with
    | None -> Some(merkle_insert_0 i k v)
    | Some(MerkH(x)) -> raise (Failure "cannot insert into this incomplete Merkle tree") (*** not enough info in the Merkle tree to do the insert ***)
    | Some(MerkL(x)) -> if x = v then Some(MerkL(x)) else raise (Failure("already have " ^ x ^ " stored at " ^ k))
    | Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) ->
	match k.[i] with
	| '0' -> Some(MerkN(merkle_insert_1 (i+1) m0 k v,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '1' -> Some(MerkN(m0,merkle_insert_1 (i+1) m1 k v,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '2' -> Some(MerkN(m0,m1,merkle_insert_1 (i+1) m2 k v,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '3' -> Some(MerkN(m0,m1,m2,merkle_insert_1 (i+1) m3 k v,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '4' -> Some(MerkN(m0,m1,m2,m3,merkle_insert_1 (i+1) m4 k v,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '5' -> Some(MerkN(m0,m1,m2,m3,m4,merkle_insert_1 (i+1) m5 k v,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '6' -> Some(MerkN(m0,m1,m2,m3,m4,m5,merkle_insert_1 (i+1) m6 k v,m7,m8,m9,m10,m11,m12,m13,m14,m15))
	| '7' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,merkle_insert_1 (i+1) m7 k v,m8,m9,m10,m11,m12,m13,m14,m15))
	| '8' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,merkle_insert_1 (i+1) m8 k v,m9,m10,m11,m12,m13,m14,m15))
	| '9' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,merkle_insert_1 (i+1) m9 k v,m10,m11,m12,m13,m14,m15))
	| 'a' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,merkle_insert_1 (i+1) m10 k v,m11,m12,m13,m14,m15))
	| 'b' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,merkle_insert_1 (i+1) m11 k v,m12,m13,m14,m15))
	| 'c' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,merkle_insert_1 (i+1) m12 k v,m13,m14,m15))
	| 'd' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,merkle_insert_1 (i+1) m13 k v,m14,m15))
	| 'e' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,merkle_insert_1 (i+1) m14 k v,m15))
	| 'f' -> Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,merkle_insert_1 (i+1) m15 k v))
	| _ -> raise (Failure "non hex char")
  in
  merkle_insert_1 0 m k v

let int_of_hexchar c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | _ -> raise (Failure "non hex char")

let supporting_merkle_tree (hl:string list) (m:merkl option) =
  let rec supporting_merkle_tree_1 i hl m =
    if hl = [] then
      begin
	match merkle_root m with
	| None -> None
	| Some(x) -> Some(MerkH(x))
      end
    else
      match m with
      | Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) ->
	  let ha = Array.make 16 [] in
	  List.iter (fun h -> ha.(int_of_hexchar h.[i]) <- h::ha.(int_of_hexchar h.[i])) hl;
	  Some(MerkN(supporting_merkle_tree_1 (i+1) (ha.(0)) m0,
		     supporting_merkle_tree_1 (i+1) (ha.(1)) m1,
		     supporting_merkle_tree_1 (i+1) (ha.(2)) m2,
		     supporting_merkle_tree_1 (i+1) (ha.(3)) m3,
		     supporting_merkle_tree_1 (i+1) (ha.(4)) m4,
		     supporting_merkle_tree_1 (i+1) (ha.(5)) m5,
		     supporting_merkle_tree_1 (i+1) (ha.(6)) m6,
		     supporting_merkle_tree_1 (i+1) (ha.(7)) m7,
		     supporting_merkle_tree_1 (i+1) (ha.(8)) m8,
		     supporting_merkle_tree_1 (i+1) (ha.(9)) m9,
		     supporting_merkle_tree_1 (i+1) (ha.(10)) m10,
		     supporting_merkle_tree_1 (i+1) (ha.(11)) m11,
		     supporting_merkle_tree_1 (i+1) (ha.(12)) m12,
		     supporting_merkle_tree_1 (i+1) (ha.(13)) m13,
		     supporting_merkle_tree_1 (i+1) (ha.(14)) m14,
		     supporting_merkle_tree_1 (i+1) (ha.(15)) m15))
      | _ -> m
  in
  supporting_merkle_tree_1 0 hl m

let global_merkle_tree : merkl option ref = ref None

let sha256reg s =
  let h = sha256 s in
  global_merkle_tree := merkle_insert !global_merkle_tree h s;
  if not (Hashtbl.mem sha256revh h) then
    begin
      Hashtbl.add sha256revh h s;
      if !sexpr then Printf.printf "(SHA256 \"%s\" \"%s\")\n" h (esc s)
    end;
  h
    
let rec save_merkl c m =
  match m with
  | None -> Printf.fprintf c "\n";
  | Some(MerkH(x)) -> Printf.fprintf c "H%s\n" x
  | Some(MerkL(x)) -> Printf.fprintf c "L%s\n" x
  | Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)) ->
      Printf.fprintf c "N\n";
      save_merkl c m0;
      save_merkl c m1;
      save_merkl c m2;
      save_merkl c m3;
      save_merkl c m4;
      save_merkl c m5;
      save_merkl c m6;
      save_merkl c m7;
      save_merkl c m8;
      save_merkl c m9;
      save_merkl c m10;
      save_merkl c m11;
      save_merkl c m12;
      save_merkl c m13;
      save_merkl c m14;
      save_merkl c m15

let rec load_merkl c =
  let l = input_line c in
  let ln = String.length l in
  if ln = 0 then
    None
  else
    match l.[0] with
    | 'H' -> Some(MerkH(String.sub l 1 (ln-1)))
    | 'L' -> Some(MerkL(String.sub l 1 (ln-1)))
    | 'N' ->
	let m0 = load_merkl c in
	let m1 = load_merkl c in
	let m2 = load_merkl c in
	let m3 = load_merkl c in
	let m4 = load_merkl c in
	let m5 = load_merkl c in
	let m6 = load_merkl c in
	let m7 = load_merkl c in
	let m8 = load_merkl c in
	let m9 = load_merkl c in
	let m10 = load_merkl c in
	let m11 = load_merkl c in
	let m12 = load_merkl c in
	let m13 = load_merkl c in
	let m14 = load_merkl c in
	let m15 = load_merkl c in
	Some(MerkN(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15))
    | _ -> raise (Failure "Not a Merkle tree")
