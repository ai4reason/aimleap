open Utils
open Sha256
open Merkle
  
let abbr_name : (string,string) Hashtbl.t = Hashtbl.create 10;;

Hashtbl.add abbr_name "b92287e489629dcd8586ce1dca89d36a30da0259351a2b738c915ed1287f1b0a" "a";;
Hashtbl.add abbr_name "26b3335f83b6745a2cf93050b5e2c2e5e6df5ffcb49c042b709fcd9dae79bfd4" "K";;
Hashtbl.add abbr_name "1f24505a8d5d29443b75542c864edb271c315991743813440806fa3c813a1124" "T";;
Hashtbl.add abbr_name "ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c" "L";;
Hashtbl.add abbr_name "077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b" "R";;

type tm =
| Var of int
| Skol of string
| Prim of string * tm list
| Abbrev of string * tm list

let rec send_tm c m =
  match m with
  | Var(i) when i < 256 ->
      output_byte c 1;
      output_byte c i
  | Skol(h) ->
      output_byte c 2;
      for i = 0 to 63 do
	output_byte c (Char.code h.[i])
      done
  | Prim("e",[]) ->
      output_byte c 3
  | Prim("*",[m1;m2]) ->
      output_byte c 4;
      send_tm c m1;
      send_tm c m2
  | Prim("\\",[m1;m2]) ->
      output_byte c 5;
      send_tm c m1;
      send_tm c m2
  | Prim("/",[m1;m2]) ->
      output_byte c 6;
      send_tm c m1;
      send_tm c m2
  | Abbrev("b92287e489629dcd8586ce1dca89d36a30da0259351a2b738c915ed1287f1b0a",[m1;m2;m3]) -> (** a **)
      output_byte c 7;
      send_tm c m1;
      send_tm c m2;
      send_tm c m3
  | Abbrev("26b3335f83b6745a2cf93050b5e2c2e5e6df5ffcb49c042b709fcd9dae79bfd4",[m1;m2]) -> (** K **)
      output_byte c 8;
      send_tm c m1;
      send_tm c m2
  | Abbrev("1f24505a8d5d29443b75542c864edb271c315991743813440806fa3c813a1124",[m1;m2]) -> (** T **)
      output_byte c 9;
      send_tm c m1;
      send_tm c m2
  | Abbrev("ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c",[m1;m2;m3]) -> (*** L ***)
      output_byte c 10;
      send_tm c m1;
      send_tm c m2;
      send_tm c m3
  | Abbrev("077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b",[m1;m2;m3]) -> (*** R ***)
      output_byte c 11;
      send_tm c m1;
      send_tm c m2;
      send_tm c m3
  | _ -> raise (Failure "send_tm got an unexpected case")

let rec rec_tm c =
  let by = input_byte c in
  if by = 1 then
    let i = input_byte c in
    Var(i)
  else if by = 2 then
    begin
      let b = Buffer.create 64 in
      for i = 0 to 63 do
	Buffer.add_char b (Char.chr (input_byte c))
      done;
      Skol(Buffer.contents b)
    end
  else if by = 3 then
    Prim("e",[])
  else if by = 4 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      Prim("*",[m1;m2])
    end
  else if by = 5 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      Prim("\\",[m1;m2])
    end
  else if by = 6 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      Prim("/",[m1;m2])
    end
  else if by = 7 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      let m3 = rec_tm c in
      Abbrev("b92287e489629dcd8586ce1dca89d36a30da0259351a2b738c915ed1287f1b0a",[m1;m2;m3])
    end
  else if by = 8 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      Abbrev("26b3335f83b6745a2cf93050b5e2c2e5e6df5ffcb49c042b709fcd9dae79bfd4",[m1;m2])
    end
  else if by = 9 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      Abbrev("1f24505a8d5d29443b75542c864edb271c315991743813440806fa3c813a1124",[m1;m2])
    end
  else if by = 10 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      let m3 = rec_tm c in
      Abbrev("ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c",[m1;m2;m3])
    end
  else if by = 11 then
    begin
      let m1 = rec_tm c in
      let m2 = rec_tm c in
      let m3 = rec_tm c in
      Abbrev("077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b",[m1;m2;m3])
    end
  else
    raise (Failure "rec_tm got an unexpected case")

let rec dual_tm m pd ad =
  match m with
  | Prim(x,s) -> Hashtbl.find pd x (List.map (fun n -> dual_tm n pd ad) s)
  | Abbrev(x,s) -> Hashtbl.find ad x (List.map (fun n -> dual_tm n pd ad) s)
  | _ -> m

let skol_name : (string,string) Hashtbl.t = Hashtbl.create 10

let rec tm_str m =
  match m with
  | Var(i) ->
      if !tptpstyle then
	Printf.sprintf "X%d" i
      else if !prover9style then
	Printf.sprintf "v%d" i
      else
	Printf.sprintf "?%d" i
  | Skol(x) ->
      begin
	try
	  Hashtbl.find skol_name x
	with Not_found ->
	  Printf.sprintf "sk%s" x
      end
  | Prim(x,s) ->
      Printf.sprintf "%s%s" x (spine_str s)
  | Abbrev(x,s) ->
      begin
	try
	  Printf.sprintf "%s%s" (Hashtbl.find abbr_name x) (spine_str s)
	with Not_found ->
	  Printf.sprintf "h%s%s" x (spine_str s)
      end
and spine_str s =
  match s with
  | [] -> ""
  | m::r ->
      Printf.sprintf "(%s%s)" (tm_str m) (spine2_str r)
and spine2_str s =
  match s with
  | [] -> ""
  | m::r ->
      Printf.sprintf ",%s%s" (tm_str m) (spine2_str r)

let tm_str_1 m =
  let rec tm_str_1 m p =
    match m with
    | Var(0) -> "X"
    | Var(1) -> "Y"
    | Var(2) -> "Z"
    | Var(3) -> "W"
    | Var(4) -> "U"
    | Var(i) -> Printf.sprintf "V%d" i
    | Skol(x) ->
       begin
	 try
	   Hashtbl.find skol_name x
	 with Not_found ->
	   Printf.sprintf "sk%s" x
       end
    | Prim("e",[]) -> "e"
    | Prim("*",[m1;m2]) when p -> Printf.sprintf "(%s * %s)" (tm_str_1 m1 true) (tm_str_1 m2 true)
    | Prim("*",[m1;m2]) -> Printf.sprintf "%s * %s" (tm_str_1 m1 true) (tm_str_1 m2 true)
    | Prim("\\",[m1;m2]) when p -> Printf.sprintf "(%s \\ %s)" (tm_str_1 m1 true) (tm_str_1 m2 true)
    | Prim("\\",[m1;m2]) -> Printf.sprintf "%s \\ %s" (tm_str_1 m1 true) (tm_str_1 m2 true)
    | Prim("/",[m1;m2]) when p -> Printf.sprintf "(%s / %s)" (tm_str_1 m1 true) (tm_str_1 m2 true)
    | Prim("/",[m1;m2]) -> Printf.sprintf "%s / %s" (tm_str_1 m1 true) (tm_str_1 m2 true)
    | Prim(x,s) ->
	Printf.sprintf "%s%s" x (spine_str_1 s)
    | Abbrev(x,s) ->
	begin
	  try
	    Printf.sprintf "%s%s" (Hashtbl.find abbr_name x) (spine_str_1 s)
	  with Not_found ->
	    Printf.sprintf "h%s%s" x (spine_str_1 s)
	end
  and spine_str_1 s =
    match s with
    | [] -> ""
    | m::r ->
	Printf.sprintf "(%s%s)" (tm_str_1 m false) (spine2_str_1 r)
  and spine2_str_1 s =
    match s with
    | [] -> ""
    | m::r ->
	Printf.sprintf ",%s%s" (tm_str_1 m false) (spine2_str_1 r)
  in
  tm_str_1 m false

let e = Prim("e",[])
let o m n = Prim("*",[m;n])
let b m n = Prim("\\",[m;n])
let s m n = Prim("/",[m;n])
let aexp x y z = b (o x (o y z)) (o (o x y) z)
let kexp x y = b (o x y) (o y x)
let lexp u x y = b (o y x) (o y (o x u))
let rexp u x y = s (o (o u x) y) (o x y)
let texp u x = b x (o u x)
let aa x y z = Abbrev("b92287e489629dcd8586ce1dca89d36a30da0259351a2b738c915ed1287f1b0a",[x;y;z])
let ka x y = Abbrev("e37d0ba946755e12407fbe545bfb766663eb7e91cc597ae750933c4ea048036b",[x;y])
let la x y z = Abbrev("ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c",[x;y;z])
let ra x y z = Abbrev("077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b",[x;y;z])
let ta x y = Abbrev("1f24505a8d5d29443b75542c864edb271c315991743813440806fa3c813a1124",[x;y])

let eta_spine_p s =
  let rec eta_spine_p_a s i =
    match s with
    | Var(j)::r when j = i -> eta_spine_p_a r (i+1)
    | _::_ -> false
    | [] -> true
  in
  eta_spine_p_a s 0

exception Not_nice
    
let rec nice_tm_a m i =
  match m with
  | Var(j) -> if j <= i then j+1 else raise Not_nice
  | Prim(_,s) -> nice_spine_a s i
  | Abbrev(_,s) -> nice_spine_a s i
  | _ -> raise Not_nice
and nice_spine_a s i =
  match s with
  | [] -> i
  | m::r -> nice_spine_a r (nice_tm_a m i)

let nice_tm m = try ignore (nice_tm_a m 0); true with Not_nice -> false

let rec tm_frees m =
  match m with
  | Var(j) -> [j]
  | Skol(_) -> []
  | Prim(_,s) -> spine_frees s
  | Abbrev(_,s) -> spine_frees s
and spine_frees s =
  match s with
  | [] -> []
  | m::r -> tm_frees m @ spine_frees r

let rec tm_abbrevs m =
  match m with
  | Var(j) -> []
  | Skol(_) -> []
  | Prim(_,s) -> spine_abbrevs s
  | Abbrev(x,s) -> x::spine_abbrevs s
and spine_abbrevs s =
  match s with
  | [] -> []
  | m::r -> tm_abbrevs m @ spine_abbrevs r

let rec tm_prims m =
  match m with
  | Var(j) -> []
  | Skol(_) -> []
  | Prim(x,s) -> x::spine_prims s
  | Abbrev(_,s) -> spine_prims s
and spine_prims s =
  match s with
  | [] -> []
  | m::r -> tm_prims m @ spine_prims r

let rec tm_skols m =
  match m with
  | Var(j) -> []
  | Skol(x) -> [x]
  | Prim(x,s) -> spine_skols s
  | Abbrev(_,s) -> spine_skols s
and spine_skols s =
  match s with
  | [] -> []
  | m::r -> tm_skols m @ spine_skols r

let rec tm_subst theta m =
  match m with
  | Var(j) -> (try theta.(j) with _ -> Var(j)) (** should not happen, but might **)
  | Skol(x) -> Skol(x)
  | Prim(x,s) -> Prim(x,List.map (tm_subst theta) s)
  | Abbrev(x,s) -> Abbrev(x,List.map (tm_subst theta) s)

let rec tm_root m =
  match m with
  | Var(i) -> sha256reg (Printf.sprintf "AIMtmVar:%d" i)
  | Skol(x) -> sha256reg (Printf.sprintf "AIMtmSkolem:%s" x)
  | Prim(x,s) -> sha256reg (Printf.sprintf "AIMtmPrim:%s:%s" x (spine_root s))
  | Abbrev(x,s) -> sha256reg (Printf.sprintf "AIMtmAbbrev:%s:%s" x (spine_root s))
and spine_root s =
  match s with
  | [] -> "" (** treat like 'none' **)
  | m::r -> sha256reg (Printf.sprintf "AIMcons:%s:%s" (tm_root m) (spine_root r))

type lit =
  | Eq of tm * tm
  | NEq of tm * tm

let lit_str l =
  match l with
  | Eq(m,n) -> Printf.sprintf "(%s = %s)" (tm_str m) (tm_str n)
  | NEq(m,n) -> Printf.sprintf "(%s != %s)" (tm_str m) (tm_str n)

let lit_negate l =
  match l with
  | Eq(m,n) -> NEq(m,n)
  | NEq(m,n) -> Eq(m,n)

let lit_subst theta l =
  match l with
  | Eq(m,n) -> Eq(tm_subst theta m,tm_subst theta n)
  | NEq(m,n) -> NEq(tm_subst theta m,tm_subst theta n)

let lit_root l =
  match l with
  | Eq(m,n) -> sha256reg (Printf.sprintf "AIMEq:%s:%s" (tm_root m) (tm_root n))
  | NEq(m,n) -> sha256reg (Printf.sprintf "AIMNEq:%s:%s" (tm_root m) (tm_root n))

let lit_frees l =
  match l with
  | Eq(m,n) -> tm_frees m @ tm_frees n
  | NEq(m,n) -> tm_frees m @ tm_frees n

let lit_abbrevs l =
  match l with
  | Eq(m,n) -> tm_abbrevs m @ tm_abbrevs n
  | NEq(m,n) -> tm_abbrevs m @ tm_abbrevs n

let lit_prims l =
  match l with
  | Eq(m,n) -> tm_prims m @ tm_prims n
  | NEq(m,n) -> tm_prims m @ tm_prims n

let lit_skols l =
  match l with
  | Eq(m,n) -> tm_skols m @ tm_skols n
  | NEq(m,n) -> tm_skols m @ tm_skols n

let rec remdups hl =
  match hl with
  | [] -> []
  | h::hr -> if List.mem h hr then remdups hr else h::remdups hr

let hashset_root pre hl =
  let rec hashset_root_a pre hl =
    match hl with
    | [] -> ""
    | h::hr -> sha256reg (Printf.sprintf "%s:%s:%s" pre h (hashset_root_a pre hr))
  in
  hashset_root_a pre (List.sort String.compare (remdups hl))

type clause = lit list

let rec clause2_str cl =
  match cl with
  | [] -> ""
  | l::cr -> Printf.sprintf "| %s%s" (lit_str l) (clause2_str cr)
  
let clause_str cl =
  match cl with
  | [] -> "$false"
  | l::cr -> Printf.sprintf "%s%s" (lit_str l) (clause2_str cr)

let clause_subst theta cl = List.map (lit_subst theta) cl

let clause_root cl = hashset_root "AIMcl" (List.map lit_root cl)

let rec tm_with_root x =
  let y = Hashtbl.find sha256revh x in
  let yl = split_string ':' y in
  match yl with
  | ["AIMtmVar";i] -> Var(int_of_string i)
  | ["AIMtmSkol";x] -> Skol(x)
  | ["AIMtmPrim";x;y] -> Prim(x,spine_with_root y)
  | ["AIMtmAbbrev";x;y] -> Abbrev(x,spine_with_root y)
  | _ -> raise (Failure "bad root for tm")
and spine_with_root x =
  if x = "" then
    []
  else
    let y = Hashtbl.find sha256revh x in
    let yl = split_string ':' y in
    match yl with
    | ["AIMcons";z;r] -> tm_with_root z::spine_with_root r
    | _ -> raise (Failure "bad root for spine")

let lit_with_root x =
  let y = Hashtbl.find sha256revh x in
  let yl = split_string ':' y in
  match yl with
  | ["AIMEq";l;r] -> Eq(tm_with_root l,tm_with_root r)
  | ["AIMNEq";l;r] -> NEq(tm_with_root l,tm_with_root r)
  | _ -> raise (Failure "bad root for lit")

let rec clauserst_with_root x =
  if x = "" then
    []
  else
    let y = Hashtbl.find sha256revh x in
    let yl = split_string ':' y in
    match yl with
    | ["AIMcl";z] -> [lit_with_root z]
    | ["AIMcl";z;r] -> (lit_with_root z::clauserst_with_root r)
    | _ -> raise (Failure "bad root for clause")
  
let clause_with_root x =
  if x = "" then
    []
  else
    let y = Hashtbl.find sha256revh x in
    let yl = split_string ':' y in
    match yl with
    | ["AIMcl";z] -> [lit_with_root z]
    | ["AIMcl";z;r] -> (lit_with_root z::clauserst_with_root r)
    | _ -> raise (Failure "bad root for clause")

let rec clause_frees cl =
  match cl with
  | [] -> []
  | l::r -> lit_frees l @ clause_frees r

let rec clause_abbrevs cl =
  match cl with
  | [] -> []
  | l::r -> lit_abbrevs l @ clause_abbrevs r

let rec clause_prims cl =
  match cl with
  | [] -> []
  | l::r -> lit_prims l @ clause_prims r

let rec clause_skols cl =
  match cl with
  | [] -> []
  | l::r -> lit_skols l @ clause_skols r

			    (*** a clause is OK if it mentions all the first n variables without skipping any and has no skolems ***)
let clause_ok cl =
  let rec upfrom l j =
    match l with
    | [] -> true
    | i::r when i = j || i = j-1 -> upfrom r (i+1)
    | _ -> false
  in
  clause_skols cl == [] && upfrom (List.sort compare (clause_frees cl)) 0

let rec okify_tm m theta i =
  match m with
  | Var(j) ->
      begin
	try
	  match theta.(j) with
	  | Skol(_) ->
	      let z = Var(i) in
	      theta.(j) <- z;
	      (z,i+1)
	  | n -> (n,i)
	with _ -> (m,i)
      end
  | Prim(x,s) ->
      let (s2,i) = okify_spine s theta i in
      (Prim(x,s2),i)
  | Abbrev(x,s) ->
      let (s2,i) = okify_spine s theta i in
      (Abbrev(x,s2),i)
  | m -> (m,i)
and okify_spine s theta i =
  match s with
  | (m::r) ->
      let (m2,i) = okify_tm m theta i in
      let (r2,i) = okify_spine r theta i in
      (m2::r2,i)
  | [] -> ([],i)

let okify_lit l theta i =
  match l with
  | Eq(m1,m2) ->
      let (m1b,i) = okify_tm m1 theta i in
      let (m2b,i) = okify_tm m2 theta i in
      (Eq(m1b,m2b),i)
  | NEq(m1,m2) -> 
      let (m1b,i) = okify_tm m1 theta i in
      let (m2b,i) = okify_tm m2 theta i in
      (NEq(m1b,m2b),i)

let rec okify_clause cl theta i =
  match cl with
  | l::cr ->
      let (l2,i) = okify_lit l theta i in
      let (cr2,i) = okify_clause cr theta i in
      (l2::cr2,i)
  | [] -> ([],i)
      
let tm_var_upperbd m =
  List.fold_left (fun x y -> if x > y then x else y+1) 0 (tm_frees m)

let clause_var_upperbd cl =
  List.fold_left (fun x y -> if x > y then x else y+1) 0 (clause_frees cl)

let rec push_tm_frees n m =
  match m with
  | Var(i) -> Var(i+n)
  | Prim(h,ml) -> Prim(h,List.map (push_tm_frees n) ml)
  | Abbrev(h,ml) -> Abbrev(h,List.map (push_tm_frees n) ml)
  | _ -> m

let push_lit_frees n l =
  match l with
  | Eq(m1,m2) -> Eq(push_tm_frees n m1,push_tm_frees n m2)
  | NEq(m1,m2) -> NEq(push_tm_frees n m1,push_tm_frees n m2)

let push_clause_frees n cl =
  List.map (push_lit_frees n) cl

let norm_clause_frees cl =
  let vl = clause_frees cl in
  let nold = List.fold_left (fun x y -> if x > y then x else y+1) 0 vl in
  let nnew = ref 0 in
  let theta = Array.make nold (Skol("fake")) in
  List.iter (fun j -> theta.(j) <- Var(!nnew); incr nnew) vl;
  (clause_subst theta cl,!nnew,theta)

type clauseset = clause list

let clauseset_root cll = hashset_root "AIMcls" (List.map clause_root cll)

    (*** return a list of skolemized unit clauses ***)
let negate_clause cl =
  let r = clause_root cl in
  let vl = clause_frees cl in
  let bd = List.fold_left (fun x y -> if x > y then x else y+1) 0 vl in
  let theta = Array.make bd (Skol("fake")) in
  List.iter
    (fun j -> theta.(j) <- Skol(sha256reg (Printf.sprintf "SkolemFor:%s:%d" r j)))
    vl;
  List.map
    (fun l -> lit_subst theta (lit_negate l))
    cl

let rec clauseset_frees cll =
  match cll with
  | [] -> []
  | cl::clr -> clause_frees cl @ clauseset_frees clr

let rec clauseset_abbrevs cll =
  match cll with
  | [] -> []
  | cl::clr -> clause_abbrevs cl @ clauseset_abbrevs clr

let rec clauseset_prims cll =
  match cll with
  | [] -> []
  | cl::clr -> clause_prims cl @ clauseset_prims clr

let process_clause name cl =
  if clause_ok cl then
    begin
      let h = clause_root cl in
      Printf.printf "(CLAUSENYM \"%s\" \"%s\")\n" name h
    end
  else
    raise (Failure(name ^ " not ok"))

let process_axiom_clause name cl =
  if clause_ok cl then
    begin
      let h = clause_root cl in
      Printf.printf "(CLAUSENYM \"%s\" \"%s\")\n" name h;
      Printf.printf "(AXIOM \"%s\")\n" h
    end
  else
    raise (Failure(name ^ " not ok"))
      
let process_abbrev name name9 m =
  if not (nice_tm m) then Printf.printf "(WARNING \"%s\" NOTNICE)\n" name;
  let h = tm_root m in
  Printf.printf "(ABBREVNYM \"%s\" \"%s\")\n" name h;
  Printf.printf "(ABBREVNYM9 \"%s\" \"%s\")\n" name9 h

let process_basic () =
  let x = Var(0) in
  let y = Var(1) in
  let z = Var(2) in
  let u = Var(3) in
  let w = Var(4) in
  process_abbrev "a" "a" (aexp x y z);
  process_abbrev "k" "K" (kexp x y);
  process_abbrev "l" "L" (lexp x y z);
  process_abbrev "r" "R" (rexp x y z);
  process_abbrev "t" "T" (texp x y);
  process_axiom_clause "lid" [Eq(o e x,x)];
  process_axiom_clause "rid" [Eq(o x e,x)];
  process_axiom_clause "b1" [Eq(b x (o x y),y)];
  process_axiom_clause "b2" [Eq(o x (b x y),y)];
  process_axiom_clause "s1" [Eq(s (o x y) y,x)];
  process_axiom_clause "s2" [Eq(o (s x y) y,x)];
  process_axiom_clause "aimtt" [Eq(ta (ta x y) z,ta (ta x z) y)];
  process_axiom_clause "aimtl" [Eq(ta (la x y z) u,la (ta x u) y z)];
  process_axiom_clause "aimtr" [Eq(ta (ra x y z) u,ra (ta x u) y z)];
  process_axiom_clause "aimlr" [Eq(la (ra x y z) u w,ra (la x u w) y z)];
  process_axiom_clause "aimll" [Eq(la (la x y z) u w,la (la x u w) y z)];
  process_axiom_clause "aimrr" [Eq(ra (ra x y z) u w,ra (ra x u w) y z)];
  process_clause "posb1" [NEq(o x y,z);Eq(s z y,x)];
  process_clause "posb2" [NEq(o x y,z);Eq(b x z,y)];
  process_clause "psob1" [NEq(s z y,x);Eq(o x y,z)];
  process_clause "psob2" [NEq(s z y,x);Eq(b x z,y)];
  process_clause "pbos1" [NEq(b x z,y);Eq(o x y,z)];
  process_clause "pbos2" [NEq(b x z,y);Eq(s z y,x)];
  process_clause "co1" [NEq(o x y,u);NEq(o x z,u);Eq(y,z)];
  process_clause "co2" [NEq(o y x,u);NEq(o z x,u);Eq(y,z)];
  process_clause "co3" [NEq(o x y,o x z);Eq(y,z)];
  process_clause "co4" [NEq(o y x,o z x);Eq(y,z)];
  process_clause "cs1" [NEq(s x y,u);NEq(s x z,u);Eq(y,z)];
  process_clause "cs2" [NEq(s y x,u);NEq(s z x,u);Eq(y,z)];
  process_clause "cs3" [NEq(s x y,s x z);Eq(y,z)];
  process_clause "cs4" [NEq(s y x,s z x);Eq(y,z)];
  process_clause "cb1" [NEq(b x y,u);NEq(b x z,u);Eq(y,z)];
  process_clause "cb2" [NEq(b y x,u);NEq(b z x,u);Eq(y,z)];
  process_clause "cb3" [NEq(b x y,b x z);Eq(y,z)];
  process_clause "cb4" [NEq(b y x,b z x);Eq(y,z)];
  process_clause "comp1" [NEq(aa x y z,e);Eq(la z y x,z)];
  process_clause "comp2" [NEq(la x y z,x);Eq(aa z y x,e)];
  process_clause "comp3" [NEq(ta x y,x);Eq(ta y x,y)];
  process_clause "comp4" [NEq(ta x y,x);Eq(ka x y,e)];
  process_clause "comp5" [NEq(ka x y,e);Eq(ta x y,x)]

type deriv =
  | ImportRule of clause
  | SubstRule of tm list * deriv
  | RearrangeRule of clause * deriv
  | ResRule of deriv * deriv
  | ParaRule of clause * deriv * deriv
  | RefRule of tm
  | AbbrevRule of clause * string * deriv

let tmlist_to_subst thetal =
  let ll = List.length thetal in
  let theta = Array.make ll (Skol("fake")) in
  let i = ref 0 in
  List.iter (fun m -> theta.(!i) <- m; incr i) thetal;
  theta

let rec clause_of_deriv d =
  match d with
  | ImportRule(cl) -> cl
  | SubstRule(thetal,d) ->
      let cl = clause_of_deriv d in
      let theta = tmlist_to_subst thetal in
      clause_subst theta cl
  | RearrangeRule(cl,_) -> cl
  | ResRule(d,e) ->
      let cl1 = clause_of_deriv d in
      let cl2 = clause_of_deriv e in
      begin
	match cl1,cl2 with
	| (_::cr1),(_::cr2) -> cr1 @ cr2
	| _,_ -> raise (Failure "bad resolution step with one clause empty")
      end
  | ParaRule(cl,_,_) -> cl
  | RefRule(m) -> [Eq(m,m)]
  | AbbrevRule(cl,_,_) -> cl

exception DerivError of string

let rec tms_eq_mod_eqn m n m1 m2 =
  if m1 = m2 then
    true
  else if m1 = m && m2 = n then
    true
  else if m1 = n && m2 = m then
    true
  else
    match m1,m2 with
    | Prim(x1,s1),Prim(x2,s2) when x1 = x2 -> spines_eq_mod_eqn m n s1 s2
    | Abbrev(x1,s1),Abbrev(x2,s2) when x1 = x2 -> spines_eq_mod_eqn m n s1 s2
    | _,_ -> false
and spines_eq_mod_eqn m n s1 s2 =
  match s1,s2 with
  | [],[] -> true
  | (m1::r1),(m2::r2) -> tms_eq_mod_eqn m n m1 m2 && spines_eq_mod_eqn m n r1 r2
  | _,_ -> false

let lits_eq_mod_eqn m n l1 l2 =
  match l1,l2 with
  | Eq(m1,n1),Eq(m2,n2) -> tms_eq_mod_eqn m n m1 m2 && tms_eq_mod_eqn m n n1 n2
  | NEq(m1,n1),NEq(m2,n2) -> tms_eq_mod_eqn m n m1 m2 && tms_eq_mod_eqn m n n1 n2
  | _,_ -> false

let rec tms_eq_mod_abbrev h m m1 m2 =
  if m1 = m2 then
    true
  else
    match m1 with
    | Abbrev(x1,s1) when x1 = h ->
	let theta = tmlist_to_subst s1 in
	begin
	  try
	    let m1a = tm_subst theta m1 in
	    if m1a = m2 then
	      true
	    else
	      tms_eq_mod_abbrev_2 h m m1a m2
	  with (Invalid_argument _) -> false
	end
    | _ ->
	tms_eq_mod_abbrev_2 h m m1 m2
and tms_eq_mod_abbrev_2 h m m1 m2 =
  match m2 with
  | Abbrev(x2,s2) when x2 = h ->
      let theta = tmlist_to_subst s2 in
      begin
	try
	  let m2a = tm_subst theta m2 in
	  if m1 = m2a then
	    true
	  else
	    tms_eq_mod_abbrev_3 h m m1 m2a
	with (Invalid_argument _) -> false
      end
  | _ ->
      tms_eq_mod_abbrev_3 h m m1 m2
and tms_eq_mod_abbrev_3 h m m1 m2 =
  match m1,m2 with
  | Prim(x1,s1),Prim(x2,s2) when x1 = x2 -> spines_eq_mod_abbrev h m s1 s2
  | Abbrev(x1,s1),Abbrev(x2,s2) when x1 = x2 -> spines_eq_mod_abbrev h m s1 s2
  | _,_ -> false
and spines_eq_mod_abbrev h m s1 s2 =
  match s1,s2 with
  | [],[] -> true
  | (m1::r1),(m2::r2) -> tms_eq_mod_abbrev h m m1 m2 && spines_eq_mod_abbrev h m r1 r2
  | _,_ -> false

let lits_eq_mod_abbrev m n l1 l2 =
  match l1,l2 with
  | Eq(m1,n1),Eq(m2,n2) -> tms_eq_mod_abbrev m n m1 m2 && tms_eq_mod_abbrev m n n1 n2
  | NEq(m1,n1),NEq(m2,n2) -> tms_eq_mod_abbrev m n m1 m2 && tms_eq_mod_abbrev m n n1 n2
  | _,_ -> false
	
let rec check_para_step_2 m n cl cl2 =
  match cl,cl2 with
  | (l::cr),(l2::cr2) ->
      if lits_eq_mod_eqn m n l l2 then
	check_para_step_2 m n cr cr2
      else
	raise (DerivError("Para lit mismatch " ^ (lit_str l) ^ " not equal to " ^ (lit_str l2) ^ " up to eqn " ^ (tm_str m) ^ " = " ^ (tm_str n)))
  | (l::_),[] -> raise (DerivError("Para rule problem, extra lit " ^ (lit_str l)))
  | [],(l2::_) -> raise (DerivError("Para rule problem, missing lit " ^ (lit_str l2)))
  | [],[] -> ()

let rec check_para_step m n cl cl1 cl2 =
  match cl,cl1 with
  | (l::cr),(l1::cr1) ->
      if l = l1 then
	check_para_step m n cr cr1 cl2
      else
	raise (DerivError("Para lit mismatch " ^ (lit_str l) ^ " not equal to " ^ (lit_str l1)))
  | _,[] -> check_para_step_2 m n cl cl2
  | [],(l1::_) -> raise (DerivError("Para rule problem, missing lit " ^ (lit_str l1)))

let rec check_abbrev_step h m cl cl1 =
  match cl,cl1 with
  | (l::cr),(l1::cr1) ->
      if lits_eq_mod_abbrev h m l l1 then
	check_abbrev_step h m cr cr1
      else
	raise (DerivError("Abbrev lit mismatch " ^ (lit_str l) ^ " not equal to " ^ (lit_str l1) ^ " up to abbrev " ^ h))
  | (l::_),[] -> raise (DerivError("Abbrev rule problem, extra lit " ^ (lit_str l)))
  | [],(l1::_) -> raise (DerivError("Abbrev rule problem, missing lit " ^ (lit_str l1)))
  | [],[] -> ()

let rec check_deriv importp abbrevlookup d =
  match d with
  | ImportRule(cl) -> let r = clause_root cl in if importp r then cl else raise (DerivError("bad import " ^ r))
  | RearrangeRule(cl,d) ->
      let cl2 = check_deriv importp abbrevlookup d in
      begin
	try
	  let bl = List.find (fun l -> not (List.mem l cl)) cl2 in
	  raise (DerivError("Rearrange dropped literal " ^ (lit_str bl)))
	with Not_found ->
	  try
	    let bl = List.find (fun l -> not (List.mem l cl2)) cl in
	    raise (DerivError("Rearrange added literal " ^ (lit_str bl)))
	  with Not_found ->
	    cl
      end
  | SubstRule(thetal,d) ->
      let cl = check_deriv importp abbrevlookup d in
      let ll = List.length thetal in
      let theta = Array.make ll (Skol("fake")) in
      let i = ref 0 in
      List.iter (fun m -> theta.(!i) <- m; incr i) thetal;
      begin
	try
	  clause_subst theta cl
	with (Invalid_argument _) -> raise (DerivError("SubstRule does not give term for every free variable"))
      end
  | ResRule(d,e) ->
      let cl1 = check_deriv importp abbrevlookup d in
      let cl2 = check_deriv importp abbrevlookup e in
      begin
	match cl1,cl2 with
	| (l1::cr1),(l2::cr2) when lit_negate l1 = l2 -> cr1 @ cr2
	| (l1::_),(l2::_) ->
	    raise (DerivError("Bad resolution step: neg of " ^ (lit_str l1) ^ " is not " ^ (lit_str l2)))
	| _,_ ->
	    raise (DerivError("Bad resolution step: one parent clause is empty"))
      end
  | ParaRule(cl,d,e) ->
      let cl1 = check_deriv importp abbrevlookup d in
      let cl2 = check_deriv importp abbrevlookup e in
      begin
	match cl1 with
	| Eq(m,n)::cr1 -> check_para_step m n cl cr1 cl2; cl
	| (l1::_) -> raise (DerivError("Bad paramodulation step: non eqn lit " ^ (lit_str l1)))
	| _ -> raise (DerivError("Bad paramodulation step: first parent is empty"))
      end
  | RefRule(m) -> [Eq(m,m)]
  | AbbrevRule(cl,h,d) ->
      try
	let cl1 = check_deriv importp abbrevlookup d in
	let m = abbrevlookup h in
	check_abbrev_step h m cl cl1;
	cl
      with Not_found -> raise (DerivError("Bad abbrev step, unknown abbrev " ^ h))
      
type conseq_deriv =
  | ForwardDeriv of deriv
  | RefutDeriv of deriv

let check_conseq_deriv importp abbrevlookup d cl =
  match d with
  | ForwardDeriv(d) -> cl = check_deriv importp abbrevlookup d
  | RefutDeriv(d) ->
      let cll = List.map (fun l -> [l]) (negate_clause cl) in
      let rl = List.map clause_root cll in
      let import2p r = List.mem r rl || importp r
      in
      [] = check_deriv import2p abbrevlookup d

let merkle_thmp m r =
  let x = sha256reg (Printf.sprintf "AIMThm:%s" r) in
  try
    ignore (merkle_lookup m x);
    true
  with Not_found ->
    false

let rec merkle_lookup_tm m r =
  let yl = split_string ':' (merkle_lookup m r) in
  match yl with
  | ["AIMtmVar";i] -> Var(int_of_string i)
  | ["AIMtmSkolem";x] -> Skol(x)
  | ["AIMtmPrim";x;sr] -> Prim(x,merkle_lookup_spine m sr)
  | ["AIMtmAbbrev";x;sr] -> Abbrev(x,merkle_lookup_spine m sr)
  | _ -> raise Not_found
and merkle_lookup_spine m r =
  if r = "" then
    []
  else
    let yl = split_string ':' (merkle_lookup m r) in
    match yl with
    | ["AIMcons";x;r] -> merkle_lookup_tm m x::merkle_lookup_spine m r
    | _ -> raise Not_found

let merkle_lookup_abbrev m r = merkle_lookup_tm m r
      
let merkle_check_conseq_deriv m d cl =
  check_conseq_deriv (merkle_thmp m) (merkle_lookup_abbrev m) d cl

type loopimpl = int * int array array * int array array * int array array

let rec eval_tm l phi m =
  let (card,mtable,stable,btable) = l in
  match m with
  | Var(i) -> List.nth phi i
  | Prim(x,[]) when x = "e" -> 0
  | Prim(x,[m;n]) when x = "*" -> mtable.(eval_tm l phi m).(eval_tm l phi n)
  | Prim(x,[m;n]) when x = "\\" -> btable.(eval_tm l phi m).(eval_tm l phi n)
  | Prim(x,[m;n]) when x = "/" -> stable.(eval_tm l phi m).(eval_tm l phi n)
  | Prim(x,_) -> raise (Failure("cannot eval prim " ^ x))
  | Abbrev(x,_) -> raise (Failure("cannot eval abbrev " ^ x))
  | Skol(x) -> raise (Failure("cannot eval skol " ^ x))
	
let eval_lit l phi li =
  match li with
  | Eq(m,n) -> eval_tm l phi m = eval_tm l phi n
  | NEq(m,n) -> not (eval_tm l phi m = eval_tm l phi n)
	
let rec eval_clause l phi cl =
  match cl with
  | [] -> false
  | li::cr -> eval_lit l phi li || eval_clause l phi cr

let clause_valid_p l cl =
  let (card,mtable,stable,btable) = l in
  let v = clause_var_upperbd cl in
  let tocheck = ref 1 in
  for i = 1 to v do tocheck := !tocheck * card done;
  if !tocheck > 1000000 then raise (Failure(Printf.sprintf "refusing to evaluate clause with %d variables in a model with %d elements." v card));
  try
    let rec pow x n = if n > 0 then x * pow x (n-1) else 1 in
    for i = 0 to (pow card v)-1 do
      let phi = ref [] in
      let z = ref i in
      for j = 0 to v-1 do
	phi := (!z mod card)::!phi;
	z := !z / card;
      done;
      if not (eval_clause l !phi cl) then
	begin
	  if !report && not (!phi = []) then
	    begin
	      Printf.printf "Index of witnesses for nontheorem (in hex; careful as these are not necessarily names of elements):";
	      List.iter (fun x -> Printf.printf " %x" x) !phi;
	      Printf.printf "\n";
	    end;
	  raise Exit
	end
    done;
    true
  with Exit -> false

let loopspec specstr =
  match split_string ':' specstr with
  | ["AIMloop";c;m] ->
      begin
	let card = int_of_string c in
	try
	  if card > 16 then (Printf.printf "Cardinality must be <= 16.\n"; raise Exit);
	  let mtable = Array.make_matrix card card 0 in
	  let stable = Array.make_matrix card card 0 in
	  let btable = Array.make_matrix card card 0 in
	  for i = 1 to card-1 do
	    mtable.(i).(0) <- i;
	    stable.(i).(0) <- i;
	    mtable.(0).(i) <- i;
	    btable.(0).(i) <- i;
	  done;
	  for i = 0 to card-2 do
	    let x = i+1 in
	    for j = 0 to card-2 do
	      let y = j+1 in
	      let z = int_of_hexchar m.[i*(card-1)+j] in
	      if z >= card then raise Exit;
	      mtable.(x).(y) <- z;
	      stable.(z).(y) <- x;
	      btable.(x).(z) <- y
	    done
	  done;
	  let l = (card,mtable,stable,btable) in
	  let ensure_clause_valid_p name cl =
	    if not (clause_valid_p l cl) then
	      begin
		Printf.printf "%s does not hold\n" name;
		raise Exit
	      end
	  in
	  let x = Var(0) in
	  let y = Var(1) in
	  let z = Var(2) in
	  let u = Var(3) in
	  let w = Var(4) in
	  ensure_clause_valid_p "lid" [Eq(o e x,x)];
	  ensure_clause_valid_p "rid" [Eq(o x e,x)];
	  ensure_clause_valid_p "b1" [Eq(b x (o x y),y)];
	  ensure_clause_valid_p "b2" [Eq(o x (b x y),y)];
	  ensure_clause_valid_p "s1" [Eq(s (o x y) y,x)];
	  ensure_clause_valid_p "s2" [Eq(o (s x y) y,x)];
	  ensure_clause_valid_p "aimtt" [Eq(texp (texp x y) z,texp (texp x z) y)];
	  ensure_clause_valid_p "aimtl" [Eq(texp (lexp x y z) u,lexp (texp x u) y z)];
	  ensure_clause_valid_p "aimtr" [Eq(texp (rexp x y z) u,rexp (texp x u) y z)];
	  ensure_clause_valid_p "aimlr" [Eq(lexp (rexp x y z) u w,rexp (lexp x u w) y z)];
	  ensure_clause_valid_p "aimll" [Eq(lexp (lexp x y z) u w,lexp (lexp x u w) y z)];
	  ensure_clause_valid_p "aimrr" [Eq(rexp (rexp x y z) u w,rexp (rexp x u w) y z)];
	  true
	with Exit -> false
      end
    | _ -> false

let pretty_loop_to_loopspec lll =
  match lll with
  | (el::llr) ->
      let card = List.length el in
      if card <= 0 then raise (Failure "number of elements must be positive.");
      if card > 16 then raise (Failure "number of elements must be <= 16.");
      if not (List.length lll = card) then raise (Failure "must be a square matrix");
      if List.exists (fun r -> not (List.length r = card)) llr then raise (Failure "must be a square matrix");
      let eltnum = Hashtbl.create 16 in
      for i = 0 to card-1 do
	let x = List.nth el i in
	if Hashtbl.mem eltnum x then raise (Failure(x ^ " repeated in first row"));
	Hashtbl.add eltnum x i;
	if i > 0 then
	  begin
	    match List.nth llr (i-1) with
	    | (x2::_) when x = x2 -> ()
	    | _ -> raise (Failure("first column does not satisfy identity law"))
	  end
      done;
      let loopspec = Buffer.create 50 in
      Buffer.add_string loopspec (Printf.sprintf "AIMloop:%d:" card);
      let mtable = Array.make_matrix card card 0 in
      let stable = Array.make_matrix card card 0 in
      let btable = Array.make_matrix card card 0 in
      for i = 1 to card-1 do
	mtable.(i).(0) <- i;
	stable.(i).(0) <- i;
	mtable.(0).(i) <- i;
	btable.(0).(i) <- i;
      done;
      for x = 1 to card-1 do
	for y = 1 to card-1 do
	  let zn = List.nth (List.nth lll y) x in
	  try
	    let z = Hashtbl.find eltnum zn in
	    Buffer.add_string loopspec (Printf.sprintf "%x" z);
	    mtable.(x).(y) <- z;
	    stable.(z).(y) <- x;
	    btable.(x).(z) <- y
	  with Not_found -> raise (Failure("unknown element " ^ zn))
	done
      done;
      let l = (card,mtable,stable,btable) in
      let ensure_clause_valid_p name cl =
	if not (clause_valid_p l cl) then raise (Failure("clause " ^ name ^ " does not hold"))
      in
      let x = Var(0) in
      let y = Var(1) in
      let z = Var(2) in
      let u = Var(3) in
      let w = Var(4) in
      ensure_clause_valid_p "lid" [Eq(o e x,x)];
      ensure_clause_valid_p "rid" [Eq(o x e,x)];
      ensure_clause_valid_p "b1" [Eq(b x (o x y),y)];
      ensure_clause_valid_p "b2" [Eq(o x (b x y),y)];
      ensure_clause_valid_p "s1" [Eq(s (o x y) y,x)];
      ensure_clause_valid_p "s2" [Eq(o (s x y) y,x)];
      ensure_clause_valid_p "aimtt" [Eq(texp (texp x y) z,texp (texp x z) y)];
      ensure_clause_valid_p "aimtl" [Eq(texp (lexp x y z) u,lexp (texp x u) y z)];
      ensure_clause_valid_p "aimtr" [Eq(texp (rexp x y z) u,rexp (texp x u) y z)];
      ensure_clause_valid_p "aimlr" [Eq(lexp (rexp x y z) u w,rexp (lexp x u w) y z)];
      ensure_clause_valid_p "aimll" [Eq(lexp (lexp x y z) u w,lexp (lexp x u w) y z)];
      ensure_clause_valid_p "aimrr" [Eq(rexp (rexp x y z) u w,rexp (rexp x u w) y z)];
      (l,Buffer.contents loopspec)
  | [] -> raise (Failure "Number of rows must be between 1 and 16.")

let parse_pretty_loop c =
  let el = ref [] in
  while !el = [] do
    let l = input_line c in
    if not (l = "") && not (l.[0] = '#') && not (l.[0] = '%') then
      el := split_string ' ' l
  done;
  let card = List.length !el in
  if card <= 0 then (Printf.printf "number of elements must be positive.\n"; raise Exit);
  if card > 16 then (Printf.printf "number of elements must be <= 16.\n"; raise Exit);
  let b = Buffer.create 100 in
  Buffer.add_string b (Printf.sprintf "AIMloop:%d:" card);
  let rec hexelt i x el =
    match el with
    | y::er when x = y -> Printf.sprintf "%x" i
    | _::er -> hexelt (i+1) x er
    | [] -> raise Exit
  in
  for i = 1 to (card-1) do
    let l = input_line c in
    let rw = split_string ' ' l in
    if not (List.length rw = card) then (Printf.printf "wrong number of elements in row %s\n" l; raise Exit);
    match rw with
    | [] -> (Printf.printf "empty row %s\n" l; raise Exit);
    | f::r ->
	if not (f = List.nth !el i) then (Printf.printf "first elt contradicts %s identity for row %s\n" (List.nth !el 0) l; raise Exit);
	List.iter (fun x -> Buffer.add_string b (hexelt 0 x !el)) r
  done;
  Printf.printf "%s\n" (Buffer.contents b);
  loopspec (Buffer.contents b)
  
let parse_pretty_loops_file f =
  let c = open_in f in
  try
    while true do
      if not (parse_pretty_loop c) then raise Exit
    done
  with
  | End_of_file -> ()
  | Exit -> Printf.printf "something wrong"

type infixop = InfNam of string

type atree =
  | Na of string
  | Info of infixop * atree * atree
  | Implop of atree * atree
  | Tuple of atree * atree * atree list

type ltree =
  | NaL of string
  | Linebreak of ltree
  | InfoL of infixop * ltree * ltree
  | ImplopL of ltree * ltree
  | ParenL of ltree * ltree list
      
let rec output_ltree ch a =
  match a with
  | NaL(x) -> output_string ch x
  | Linebreak(a) ->
      output_ltree ch a;
      output_char ch '\n';
  | InfoL(InfNam x,a,b) ->
      output_ltree ch a;
      output_char ch ' ';
      output_string ch x;
      output_char ch ' ';
      output_ltree ch b
  | ImplopL(a,b) ->
      output_ltree ch a;
      output_char ch ' ';
      output_ltree ch b
  | ParenL(a,bl) ->
      output_char ch '(';
      output_ltree ch a;
      List.iter (fun b -> output_char ch ','; output_ltree ch b) bl;
      output_char ch ')'

let rec output_atree ch a =
  match a with
  | Na(x) -> output_string ch x
  | Info(InfNam x,a,b) ->
      output_string ch "(INFO ";
      output_string ch x;
      output_char ch ' ';
      output_atree ch a;
      output_char ch ' ';
      output_atree ch b;
      output_char ch ')';
  | Implop(a,b) ->
      output_string ch "(IMPLOP ";
      output_atree ch a;
      output_char ch ' ';
      output_atree ch b;
      output_char ch ')';
  | Tuple(a,b,bl) ->
      output_string ch "(TUPLE ";
      output_atree ch a;
      output_char ch ' ';
      output_atree ch b;
      List.iter (fun b -> output_char ch ' '; output_atree ch b) bl;
      output_char ch ')'
      
let rec ltree_to_atree a =
  match a with
  | NaL(x) -> Na(x)
  | Linebreak(a) -> ltree_to_atree a
  | InfoL(x,a,b) -> Info(x,ltree_to_atree a,ltree_to_atree b)
  | ImplopL(a,b) -> Implop(ltree_to_atree a,ltree_to_atree b)
  | ParenL(a,[]) -> ltree_to_atree a
  | ParenL(a,b::cl) -> Tuple(ltree_to_atree a,ltree_to_atree b,List.map ltree_to_atree cl)

type picase = Postfix | InfixNone | InfixLeft | InfixRight

type nym = string * string * string

type docitem =
  | ClaimItem of (bool * nym list) option * string option * ltree * ((ltree list) option)
  | Editors of int * nym list
  | AddEditors of int * nym list
  | RemEditors of nym list
  | EditorQuorum of int
  | LastIssueOfVolume
  | PreviousIssue of string
  | Article of string
  | Author of nym list
  | Title of string
  | Section of string
  | End of string
  | HypDecl of string * ltree
  | ParamDecl of string * int * string
  | DefDecl of (bool * nym list) option * string * string list * ltree
  | ParamHash of string * string
  | AxDecl of string * ltree
  | ThmDecl of string * (bool * nym list) option * string * ltree
  | KnownDecl of string * ltree
  | ConjDecl of (bool * nym list) option * string option * ltree
  | LoopDecl of (bool * nym list) option * string option * string list list
  | NonThmDecl of (bool * nym list) option * string option * ltree * string list

type pftacitem =
  | PfStruct of int
  | Exact of ltree
  | LetTac of string list * ltree option
  | AssumeTac of string option * ltree
  | HaveTac of string option * ltree * bool * string list option
  | ThusTac of string option * ltree * bool * string list option
  | EqChainTac of string option * ltree * bool * string list option
  | SetTac of string * ltree option * ltree
  | ApplyTac of ltree
  | ClaimTac of string * ltree
  | ProveTac of ltree
  | CasesTac of ltree * bool * string list option
  | WitnessTac of ltree
  | RewriteTac of bool * string * int list
  | Qed
  | Admitted
  | Admit

type eqpftacitem =
  | EqLink of ltree * string list option
  | EqDone
      
type docorpftacitem =
  | DocItem : docitem -> docorpftacitem
  | PfTacItem : pftacitem -> docorpftacitem

let latexify_mathname x =
  let y = Buffer.create 10 in
  let l = String.length x in
  let rec latexify_mathname_1 i b =
    if i < l then
      let c = x.[i] in
      if c = '_' then
	begin
	  if b then Buffer.add_char y '}';
	  Buffer.add_char y c;
	  Buffer.add_char y '{';
	  latexify_mathname_1 (i+1) true
	end
      else if c = '^' then
	begin
	  if b then Buffer.add_char y '}';
	  Buffer.add_char y c;
	  Buffer.add_char y '{';
	  latexify_mathname_1 (i+1) true
	end
      else
	begin
	  Buffer.add_char y c;
	  latexify_mathname_1 (i+1) b
	end
    else
      if b then Buffer.add_char y '}';
  in
  latexify_mathname_1 0 false;
  Buffer.contents y

let rec ltree_has_a_linebreak_p l =
  match l with
  | Linebreak(_) -> true
  | InfoL(_,l1,l2) -> ltree_has_a_linebreak_p l1 || ltree_has_a_linebreak_p l2
  | ImplopL(l1,l2) -> ltree_has_a_linebreak_p l1 || ltree_has_a_linebreak_p l2
  | ParenL(l,r) -> ltree_has_a_linebreak_p l || ltrees_have_a_linebreak_p r
  | _ -> false
and ltrees_have_a_linebreak_p r =
  match r with
  | [] -> false
  | l::r -> ltree_has_a_linebreak_p l || ltrees_have_a_linebreak_p r

let rec ltree_to_latex_math c l =
  match l with
  | NaL(x) -> Printf.fprintf c "%s" (latexify_mathname x);
  | Linebreak(l) -> raise (Failure "linebreaks must be in array")
  | InfoL(InfNam(op),l1,l2) ->
      ltree_to_latex_math c l1;
      begin
	if op = "\\" then
	  Printf.fprintf c " \\backslash "
	else if op = "!=" then
	  Printf.fprintf c " \\not= "
	else if op = "|" then
	  Printf.fprintf c " \\lor "
	else if op = "&" then
	  Printf.fprintf c " \\land "
	else if op = "->" then
	  Printf.fprintf c " \\limplies "
	else if op = "*" then
	  Printf.fprintf c " \\cdot "
	else
	  Printf.fprintf c " %s " op
      end;
      ltree_to_latex_math c l2;
  | ImplopL(l1,l2) ->
      ltree_to_latex_math c l1;
      ltree_to_latex_math c l2;
  | ParenL(l,r) ->
      Printf.fprintf c "(";
      ltree_to_latex_math c l;
      ltree_to_latex_math_tuplerst c r;
      Printf.fprintf c ")"
and ltree_to_latex_math_tuplerst c r =
  match r with
  | [] -> ()
  | l::r ->
      Printf.fprintf c ",";
      ltree_to_latex_math c l;
      ltree_to_latex_math_tuplerst c r

let rec ltree_to_latex_matharray i c l =
  match l with
  | NaL(x) -> Printf.fprintf c "%s" (latexify_mathname x);
  | Linebreak(l) ->
      ltree_to_latex_matharray i c l;
      Printf.fprintf c "\\\\\n";
      for j = 1 to i do Printf.fprintf c "& " done
  | InfoL(InfNam(op),l1,l2) ->
      ltree_to_latex_matharray i c l1;
      begin
	if op = "\\" then
	  Printf.fprintf c " \\backslash "
	else if op = "!=" then
	  Printf.fprintf c " \\not= "
	else if op = "|" then
	  Printf.fprintf c " \\lor "
	else if op = "&" then
	  Printf.fprintf c " \\land "
	else if op = "->" then
	  Printf.fprintf c " \\limplies "
	else if op = "*" then
	  Printf.fprintf c " \\cdot "
	else
	  Printf.fprintf c " %s " op
      end;
      ltree_to_latex_matharray i c l2;
  | ImplopL(l1,l2) ->
      ltree_to_latex_matharray i c l1;
      ltree_to_latex_matharray i c l2;
  | ParenL(l,r) ->
      Printf.fprintf c "(";
      ltree_to_latex_matharray i c l;
      ltree_to_latex_matharray_tuplerst i c r;
      Printf.fprintf c ")"
and ltree_to_latex_matharray_tuplerst i c r =
  match r with
  | [] -> ()
  | l::r ->
      Printf.fprintf c ",";
      ltree_to_latex_matharray i c l;
      ltree_to_latex_matharray_tuplerst i c r

let ltree_to_latex_matharray_top dot i c l =
  ltree_to_latex_matharray i c l;
  if dot then Printf.fprintf c "."

let ltree_to_latex_math_or_matharray_top dot c l =
  if ltree_has_a_linebreak_p l then
    begin
      Printf.fprintf c "\n$$\\begin{array}{c}\n";
      ltree_to_latex_matharray 0 c l;
      Printf.fprintf c "%s\n\\end{array}$$\n" (if dot then "." else "")
    end
  else
    begin
      Printf.fprintf c "$";
      ltree_to_latex_math c l;
      Printf.fprintf c "$%s" (if dot then "." else "")
    end
      
let ltree_to_latex_matharray_septopeqn c l =
  match l with
  | InfoL(InfNam(op),l1,l2) when op = "=" ->
      Printf.fprintf c " & ";
      ltree_to_latex_matharray 1 c l1;
      Printf.fprintf c " \\\\ = & ";
      ltree_to_latex_matharray 1 c l2;
  | _ ->
      ltree_to_latex_math c l

let rec ltree_to_html_math c h l =
  match l with
  | NaL(x) ->
      begin
	try
	  let r = h x in
	  Printf.fprintf c "%s" r
	with Not_found ->
	  Printf.fprintf c "%s" x
      end
  | Linebreak(l) ->
      ltree_to_html_math c h l;
      Printf.fprintf c "<br/>\n"
  | InfoL(InfNam(op),l1,l2) ->
      ltree_to_html_math c h l1;
      begin
	if op = "!=" then
	  Printf.fprintf c " &ne; "
	else if op = "|" then
	  Printf.fprintf c " &or; "
	else if op = "&" then
	  Printf.fprintf c " &and; "
	else if op = "->" then
	  Printf.fprintf c " &rarr; "
	else
	  Printf.fprintf c " %s " op
      end;
      ltree_to_html_math c h l2;
  | ImplopL(l1,l2) ->
      ltree_to_html_math c h l1;
      ltree_to_html_math c h l2;
  | ParenL(l,r) ->
      Printf.fprintf c "(";
      ltree_to_html_math c h l;
      ltree_to_html_math_tuplerst c h r;
      Printf.fprintf c ")"
and ltree_to_html_math_tuplerst c h r =
  match r with
  | [] -> ()
  | l::r ->
      Printf.fprintf c ",";
      ltree_to_html_math c h l;
      ltree_to_html_math_tuplerst c h r

let ltree_to_html_math_septopeqn c h l =
  match l with
  | InfoL(InfNam(op),l1,l2) when op = "=" ->
      Printf.fprintf c "<td></td><td>";
      ltree_to_html_math c h l1;
      Printf.fprintf c "</td></tr><tr><td>=</td><td>";
      ltree_to_html_math c h l2;
      Printf.fprintf c "</td>";
  | _ ->
      ltree_to_html_math c h l

let rec delta_normalize_tm m d =
  match m with
  | Prim(x,s) -> Prim(x,List.map (fun n -> delta_normalize_tm n d) s)
  | Abbrev(x,s) ->
      begin
	try
	  let (da,dd) = d in
	  let a = Hashtbl.find da x in
	  let n = Hashtbl.find dd x in
	  if not (List.length s = a) then raise (Failure(x ^ " used with wrong arity"));
	  let theta = Array.make a (Skol("fake")) in
	  for i = 0 to a-1 do
	    theta.(i) <- delta_normalize_tm (List.nth s i) d
	  done;
	  delta_normalize_tm (tm_subst theta n) d
	with Not_found ->
	  Abbrev(x,List.map (fun n -> delta_normalize_tm n d) s)
      end
  | _ -> m

let delta_normalize_lit l d =
  match l with
  | Eq(m,n) -> Eq(delta_normalize_tm m d,delta_normalize_tm n d)
  | NEq(m,n) -> NEq(delta_normalize_tm m d,delta_normalize_tm n d)
	
let rec delta_normalize_clause cl d =
  match cl with
  | [] -> []
  | l::cr -> delta_normalize_lit l d::delta_normalize_clause cr d

let rec tm_ivy m =
  match m with
  | Var(i) -> Printf.sprintf "v%d" i
  | Skol(c) -> Printf.sprintf "(c%s)" c
  | Prim("e",[]) -> "(one_for_ivy)"
  | Prim("\\",s) -> Printf.sprintf "(backslash_for_ivy%s)" (spine_ivy s)
  | Prim(f,s) -> Printf.sprintf "(%s%s)" f (spine_ivy s)
  | Abbrev("1f24505a8d5d29443b75542c864edb271c315991743813440806fa3c813a1124",s) -> Printf.sprintf "(T%s)" (spine_ivy s)
  | Abbrev("ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c",s) -> Printf.sprintf "(L%s)" (spine_ivy s)
  | Abbrev("077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b",s) -> Printf.sprintf "(R%s)" (spine_ivy s)
  | Abbrev("b92287e489629dcd8586ce1dca89d36a30da0259351a2b738c915ed1287f1b0a",s) -> Printf.sprintf "(a%s)" (spine_ivy s)
  | Abbrev("26b3335f83b6745a2cf93050b5e2c2e5e6df5ffcb49c042b709fcd9dae79bfd4",s) -> Printf.sprintf "(K%s)" (spine_ivy s)
  | Abbrev(f,s) -> Printf.sprintf "(\"%s\"%s)" f (spine_ivy s)
and spine_ivy s =
  match s with
  | [] -> ""
  | n::r -> Printf.sprintf " %s%s" (tm_ivy n) (spine_ivy r)

let lit_ivy l =
  match l with
  | Eq(m,n) -> Printf.sprintf "(= %s %s)" (tm_ivy m) (tm_ivy n)
  | NEq(m,n) -> Printf.sprintf "(not (= %s %s))" (tm_ivy m) (tm_ivy n)

let rec clause_ivy cl =
  match cl with
  | [] -> "false"
  | [l] -> lit_ivy l
  | l::cr -> Printf.sprintf "(or %s %s)" (lit_ivy l) (clause_ivy cr)
