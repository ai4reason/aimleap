open Utils
open Sha256
open Merkle
open Syntax
open Parser

let dyndist = ref false;;
let usebatchdist = ref true;;
let maxdepth = ref 10.0;;
let valid_aimnym_p _ = raise (Failure "unwritten");;
let sgn_from_aimsgn _ = raise (Failure "unwritten");;
let pubkey_from_aimnym _ = raise (Failure "unwritten");;
let check_sgn _ _ _ = raise (Failure "unwritten");;
let eq_big_int _ _ = raise (Failure "unwritten");;
    
let sign = ref false;;
let publish = ref false;;
let admits = ref [];;
let verbose = ref 1;;
let evaluateconj = ref false;;
let currentfileh = ref "";;
let currentfileh58 = ref "";;
let currentfiletitle = ref "";;
let check_conjectures_for_subsumption = ref false;;
let conjl = ref [];;
let articlerefs = ref [];;
let articletitle = ref None;;
let articleauthors = ref None;;
let articleauthorpubkeys : ((string * string) * string) list ref = ref [];;
let authpubkeyslefttosign = ref [];;
let authpubkeyssigned = ref [];;
let authornymh : (nym,unit) Hashtbl.t = Hashtbl.create 10;;
let othercreditnymh : (nym,int) Hashtbl.t = Hashtbl.create 10;;
let itemcount = ref 0;;
let editorslefttosign : (int * (unit * unit) * Syntax.nym) list ref = ref [];;
let editorssigned = ref [];;
let volumeno = ref 0;;
let issueno = ref 0;;
let signwithprivkey : unit option ref = ref None;;
let ensurecompletelysigned = ref false;;
let sgns : (unit * string) list ref = ref [];;
let latexc = ref stdout;;
let previousissue = ref None;;
let summary = ref false;;

let eqnindh : (string,int) Hashtbl.t = Hashtbl.create 87

let set_eqnindh () =
  Hashtbl.add eqnindh "lid" 0;
  Hashtbl.add eqnindh "rid" 1;
  Hashtbl.add eqnindh "b1" 2;
  Hashtbl.add eqnindh "b2" 3;
  Hashtbl.add eqnindh "s1" 4;
  Hashtbl.add eqnindh "s2" 5;
  Hashtbl.add eqnindh "a" 6;
  Hashtbl.add eqnindh "K" 7;
  Hashtbl.add eqnindh "T" 8;
  Hashtbl.add eqnindh "L" 9;
  Hashtbl.add eqnindh "R" 10;
  Hashtbl.add eqnindh "TT" 11;
  Hashtbl.add eqnindh "TL" 12;
  Hashtbl.add eqnindh "TR" 13;
  Hashtbl.add eqnindh "LR" 14;
  Hashtbl.add eqnindh "LL" 15;
  Hashtbl.add eqnindh "RR" 16;
  Hashtbl.add eqnindh "id1" 17;
  Hashtbl.add eqnindh "id2" 18;
  Hashtbl.add eqnindh "id3" 19;
  Hashtbl.add eqnindh "id4" 20;
  Hashtbl.add eqnindh "id5" 21;
  Hashtbl.add eqnindh "id6" 22;
  Hashtbl.add eqnindh "prop_034cf5c5" 23;
  Hashtbl.add eqnindh "prop_66f8dd43" 24;
  Hashtbl.add eqnindh "prop_81894ca4" 25;
  Hashtbl.add eqnindh "prop_da4738e2" 26;
  Hashtbl.add eqnindh "prop_4a90a23f" 27;
  Hashtbl.add eqnindh "prop_73fa4877" 28;
  Hashtbl.add eqnindh "prop_d10a3b1a" 29;
  Hashtbl.add eqnindh "prop_293cbc16" 30;
  Hashtbl.add eqnindh "prop_55464ec9" 31;
  Hashtbl.add eqnindh "prop_61fb8127" 32;
  Hashtbl.add eqnindh "prop_ddd1c86f" 33;
  Hashtbl.add eqnindh "prop_0d7e7151" 34;
  Hashtbl.add eqnindh "prop_1aae4a83" 35;
  Hashtbl.add eqnindh "prop_1db0183a" 36;
  Hashtbl.add eqnindh "prop_a4abb1e0" 37;
  Hashtbl.add eqnindh "prop_55842885" 38;
  Hashtbl.add eqnindh "prop_1a725917" 39;
  Hashtbl.add eqnindh "prop_c626af2d" 40;
  Hashtbl.add eqnindh "prop_526a359c" 41;
  Hashtbl.add eqnindh "prop_deeac89a" 42;
  Hashtbl.add eqnindh "prop_575d1ed9" 43;
  Hashtbl.add eqnindh "prop_f338e359" 44;
  Hashtbl.add eqnindh "prop_5a914e30" 45;
  Hashtbl.add eqnindh "prop_cc8a9ae6" 46;
  Hashtbl.add eqnindh "prop_c2aa8580" 47;
  Hashtbl.add eqnindh "prop_b3890d2c" 48;
  Hashtbl.add eqnindh "prop_f14899ed" 49;
  Hashtbl.add eqnindh "prop_f2b7d0ab" 50;
  Hashtbl.add eqnindh "prop_be6cad0a" 51;
  Hashtbl.add eqnindh "prop_1e558562" 52;
  Hashtbl.add eqnindh "prop_e9eef609" 53;
  Hashtbl.add eqnindh "prop_9ee87fb5" 54;
  Hashtbl.add eqnindh "prop_da958b3f" 55;
  Hashtbl.add eqnindh "prop_c3fa51e8" 56;
  Hashtbl.add eqnindh "prop_ba6418d1" 57;
  Hashtbl.add eqnindh "prop_d7dd57dd" 58;
  Hashtbl.add eqnindh "prop_e1aa92db" 59;
  Hashtbl.add eqnindh "prop_19fcac9b2" 60;
  Hashtbl.add eqnindh "prop_3d75df700" 61;
  Hashtbl.add eqnindh "prop_acafcc6f0" 62;
  Hashtbl.add eqnindh "prop_203fc9151" 63;
  Hashtbl.add eqnindh "prop_2e844a2a9" 64;
  Hashtbl.add eqnindh "prop_d9f457e09" 65;
  Hashtbl.add eqnindh "prop_ce2987245" 66;
  Hashtbl.add eqnindh "prop_b7fe5fbfb" 67;
  Hashtbl.add eqnindh "prov9_7c96e347d4" 68;
  Hashtbl.add eqnindh "prov9_3e047dc57d" 69;
  Hashtbl.add eqnindh "prov9_ee78192c46" 70;
  Hashtbl.add eqnindh "prov9_062c221162" 71;
  Hashtbl.add eqnindh "prov9_d18167fcf7" 72;
  Hashtbl.add eqnindh "prov9_6385279d78" 73;
  Hashtbl.add eqnindh "prov9_b192646899" 74;
  Hashtbl.add eqnindh "prov9_2e3bc568bd_alt1" 75;
  Hashtbl.add eqnindh "prov9_1ffb5e2572" 76;
  Hashtbl.add eqnindh "prov9_7fed2c3e64" 77;
  Hashtbl.add eqnindh "prov9_47e1e09ded" 78;
  Hashtbl.add eqnindh "prov9_a06014c62d_com" 79;
  Hashtbl.add eqnindh "prov9_a06014c62d" 80;
  Hashtbl.add eqnindh "prov9_49726cdcf0" 81;
  Hashtbl.add eqnindh "prov9_a69214de59" 82;
  Hashtbl.add eqnindh "prov9_3a3c9a39ee" 83;
  Hashtbl.add eqnindh "prov9_1cecad55d3" 84;
  Hashtbl.add eqnindh "prov9_13e5c8ed0a" 85;
  Hashtbl.add eqnindh "prov9_183b179b43" 86

let fakedepth = ref 0
let fakedepthh : (string,int) Hashtbl.t = Hashtbl.create 100
let fakelearn = ref false
let fakedist : (string * string,float) Hashtbl.t = Hashtbl.create 100

let constdist : float option ref = ref None

type refut =
  | Hyp of clause
  | Kn of int * clause * string 
  | Def of int * clause * string list * refut
  | Subst of int * clause * int * tm array * refut
  | Refl of int * clause * refut
  | Res of int * clause * lit * refut * refut
  | Para of int * clause * tm * tm * lit * refut * refut;;

let refutcomment : (refut,string) Hashtbl.t = Hashtbl.create 100;;

let print_refut_info r =
  let lineno = ref 1 in
  let h : (refut,int) Hashtbl.t = Hashtbl.create 100 in
  let rec print_refut_info_r r =
    if not (Hashtbl.mem h r) then
      begin
	let ln = !lineno in
	Hashtbl.add h r ln;
	incr lineno;
	begin
	  try
	    let c = Hashtbl.find refutcomment r in
	    Printf.printf "[%d] %s\n" ln c
	  with Not_found -> ()
	end;
	match r with
	| Hyp(cl) -> Printf.printf "%d. Hyp: %s\n" ln (clause_str cl)
	| Kn(_,cl,x) -> Printf.printf "%d. Known %s: %s\n" ln x (clause_str cl)
	| Def(_,cl,xl,r1) ->
	    print_refut_info_r r1;
	    Printf.printf "%d. Deltas" ln; List.iter (fun x -> Printf.printf " %s" x) xl;
	    Printf.printf ": %s by %d\n" (clause_str cl) (Hashtbl.find h r1)
	| Subst(_,cl,k,theta,r1) ->
	    print_refut_info_r r1;
	    Printf.printf "%d. Subst: %s by %d\n" ln (clause_str cl) (Hashtbl.find h r1)
	| Refl(_,cl,r1) ->
	    print_refut_info_r r1;
	    Printf.printf "%d. Refl: %s by %d\n" ln (clause_str cl) (Hashtbl.find h r1)
	| Res(_,cl,_,r1,r2) ->
	    print_refut_info_r r1;
	    print_refut_info_r r2;
	    Printf.printf "%d. Res: %s by %d,%d\n" ln (clause_str cl) (Hashtbl.find h r1) (Hashtbl.find h r2)
	| Para(_,cl,m1,m2,_,r1,r2) ->
	    print_refut_info_r r1;
	    print_refut_info_r r2;
	    Printf.printf "%d. Para (%s = %s): %s by %d,%d\n" ln (tm_str m1) (tm_str m2) (clause_str cl) (Hashtbl.find h r1) (Hashtbl.find h r2)
      end
  in
  print_refut_info_r r

let rem_dups l =
  let rec rem_dups_r l rl =
    match l with
    | a::r when List.mem a rl -> rem_dups_r r rl
    | a::r -> rem_dups_r r (a::rl)
    | [] -> rl
  in
  rem_dups_r l []

let depth : int ref = ref 0;;
let reverseusable = ref false;;
let usableeqns : (string * tm * tm) list ref = ref [];;
let sos2 : refut list ref = ref [];;
let sos : refut list ref = ref [];;
let pending : (string * tm * tm * int) option ref = ref None;;
let rewritefailed = ref false;;
let rewritecount = ref 0;;
let countoptions = ref false;;
let abstracttimelimit = ref None;;
let abstracttimecount = ref 0;;
let starttime = ref 0.0;;

let advisorsocket = ref None;;
let advisorsocketfd = ref None;;
let advisorsocketin = ref None;;
let advisorsocketout = ref None;;

let open_advisor_socket () =
  match !advisorsocket with
  | None ->
      if !verbose > 1 then Printf.printf "No advisor\n"
  | Some(sock) ->
      let t1 = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let ia = Unix.inet_addr_of_string "127.0.0.1" in
      Unix.connect t1 (Unix.ADDR_INET(ia, sock));
      let t1in = Unix.in_channel_of_descr t1 in
      let t1out = Unix.out_channel_of_descr t1 in
      set_binary_mode_in t1in true;
      set_binary_mode_out t1out true;
      advisorsocketfd := Some(t1);
      advisorsocketin := Some(t1in);
      advisorsocketout := Some(t1out)

let close_advisor_socket () =
  begin
    match !advisorsocketfd with
    | Some(t1) ->
	Unix.shutdown t1 Unix.SHUTDOWN_ALL;
	Unix.close t1
    | None -> ()
  end

exception AbstractTimeLimit;;
exception EmptyClause of refut;;

let hyprefs : (string option,bool) Hashtbl.t = Hashtbl.create 100;;

let axioms = [
  "397bbf43bf477cea90860dda24d94120dce41723f5d88e832c32d0b0a3b24e81";
  "2184e15b29d6fd2e313e4ea5ae0ad79a60ff01e38e803a60fc0b88db4b0bacc3";
  "46c19e3c61174e562bff1429b879c1a4b9666419aee687487a1ef8d505970841";
  "59fd3a14e001799be827ed797525af57526a585e7e0568a4caa0ff63d1e213a5";
  "de078b9508c9d429ff38315671454c16513bd2dc77c0b48f24958843d956ad26";
  "3771530a7b6b64867684ebec9159a93f496e67885cbc0aea4873f6c1398f6411";
  "49470c011b2801cfe3194ee7df6a6d51ea75b6b3600c0a246cf1456a19f999a8";
  "184e78e899b6e0fe0869a4926ea3d78cafbf38b30122c40bae4209c74b2d3524";
  "bc5d34b79ef11cf26e6bc0798ebe7a3c8b13f277c1a410ecf6c20db18516a8be";
  "fa6c343e72fba29154085f54946d09855c56518271243e197e0df37a97fb4da4";
  "b47852e8a285899bac1fd54ac22a00a796674f63095f2667d5279bf025155801";
  "8cfec45a332cd57fb0e238bac5cc0dbdbd17088d7c7b1ebc3b54bffa8f4f5b9c"
];;

let main7conjectures = [
  "678fb70a73634e5cd2c9d6843d31deb6feffc85fece61f0b054a39678e72d6dd";
  "f886df6cf6f9de5f303a4606f113dc319cdb80a44baf65a0837d96055b538766";
  "82b15b1015630406f549d3e1132be42a810b9018a02f5e1f367c1fbcc378e60c";
  "5b6d5525ce96b886df4b5b73fd092086bf65aa4796332da06c1ade16c89c9fbb";
  "d54500d91e3d64263ef639fdbcfa8a61d35b5468fc4e76326e26dcdb065f3ef1";
  "cbfb4363a0805a6d9b860d13f512d7c210257a87b8ba75451fba7d091ceebe7b";
  "6bb7c4d9f5ce4f337ea973fcd0e03beb452497e4ca5cba965c9d72bb083afcbd"
];;

let main5abbrevs = [
  (3,"b92287e489629dcd8586ce1dca89d36a30da0259351a2b738c915ed1287f1b0a");
  (2,"26b3335f83b6745a2cf93050b5e2c2e5e6df5ffcb49c042b709fcd9dae79bfd4");
  (2,"1f24505a8d5d29443b75542c864edb271c315991743813440806fa3c813a1124");
  (3,"ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c");
  (3,"077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b")
];;

let dualprim : (string,tm list -> tm) Hashtbl.t = Hashtbl.create 10;;
let dualabbrev : (string,tm list -> tm) Hashtbl.t = Hashtbl.create 10;;

Hashtbl.add dualprim "*" (fun s -> match s with [m;n] -> Prim("*",[n;m]) | _ -> raise Not_found);;
Hashtbl.add dualprim "\\" (fun s -> match s with [m;n] -> Prim("/",[n;m]) | _ -> raise Not_found);;
Hashtbl.add dualprim "/" (fun s -> match s with [m;n] -> Prim("\\",[n;m]) | _ -> raise Not_found);;
Hashtbl.add dualabbrev "ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c" (fun s -> match s with [m;n;k] -> Abbrev("077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b",[m;n;k]) | _ -> raise Not_found);; (*** L to R ***)
Hashtbl.add dualabbrev "077ee3019aec47d0a7a3e9323ddec0abc01e7bb1752ebe2d36b3d541888cba1b" (fun s -> match s with [m;n;k] -> Abbrev("ce732969701b096ac12147afcf266c35588c46a811d3d2ae9a0a94aa204e544c",[m;n;k]) | _ -> raise Not_found);; (*** R to L ***)

let confirm msg y n =
  let cnt = ref true in
  while !cnt do
    Printf.printf "%s " msg;
    flush stdout;
    let l = read_line () in
    if l = "" || l = "y" || l = "Y" then
      (y(); cnt := false)
    else if l = "n" || l = "N" then
      (n(); cnt := false)
    else
      Printf.printf "Please answer with y or n.\n";
  done

let ensure_dir_exists d f =
  try
    if not (Sys.is_directory d) then
      raise (Failure(d ^ " exists and is not a directory"))
  with Sys_error(_) ->
    f d;
    Unix.mkdir d 0o775;;

let addothercreditnyml creditnyml =
  match creditnyml with
  | None -> ()
  | Some(_,l) ->
      List.iter
	(fun a ->
	  if not (Hashtbl.mem authornymh a) then
	    begin
	      try
		let n = Hashtbl.find othercreditnymh a in
		Hashtbl.replace othercreditnymh a (n+1)
	      with Not_found ->
		Hashtbl.add othercreditnymh a 1
	    end)
	l;;

let read_signatures_then_line c collectsigs =
  let ll = ref "" in
  try
    while true do
      let l = input_line c in
      if String.length l > 4 && String.sub l 0 4 = "SIG:" then
	begin
	  if collectsigs then
	    let sgs = String.sub l 4 (String.length l - 4) in
	    let sg = sgn_from_aimsgn sgs in
	    sgns := (sg,sgs)::!sgns
	end
      else
	(ll := l; raise Exit)
    done;
    !ll
  with Exit ->
    !ll;;
      
let editors : (int * nym) list ref = ref [];;
let unlistededitors : (int * nym) list ref = ref [];;
let quorum : int ref = ref 4;;
let allknowns_merkle_root = ref "";;
let allknowns_merkl : merkl option ref = ref None;;
let allknowns_merkl_lookups : (string,unit) Hashtbl.t = Hashtbl.create 10;;
let allconjs_merkle_root = ref "";;
let allconjs_merkl : merkl option ref = ref None;;
let allconjs_merkl_lookups : (string,unit) Hashtbl.t = Hashtbl.create 10;;
let allnonthms_merkle_root = ref "";;
let allnonthms_merkl : merkl option ref = ref None;;
let allnonthms_merkl_lookups : (string,unit) Hashtbl.t = Hashtbl.create 10;;
let allloops_merkle_root = ref "";;
let allloops_merkl : merkl option ref = ref None;;
let allloops_merkl_lookups : (string,unit) Hashtbl.t = Hashtbl.create 10;;
let allabbrevs_merkle_root = ref "";;
let allabbrevs_merkl : merkl option ref = ref None;;
let allabbrevs_merkl_lookups : (string,unit) Hashtbl.t = Hashtbl.create 10;;
let knownclauses : (string,string) Hashtbl.t = Hashtbl.create 10;;
let abbrevroot : (string,string) Hashtbl.t = Hashtbl.create 10;;
let abbrevarity : (string,int) Hashtbl.t = Hashtbl.create 10;;
let abbrevdef : (string,tm) Hashtbl.t = Hashtbl.create 10;;

let title_split x =
  let xr = ref x in
  let r = ref "" in
  while not (!xr = "") do
    if String.length !xr < 10 then
      if !r = "" then
	(r := !xr; xr := "")
      else
	(r := !r ^ " " ^ !xr; xr := "")
    else if !r = "" then
      (r := String.sub !xr 0 7; xr := String.sub !xr 7 (String.length !xr - 7))
    else
      (r := !r ^ " " ^ String.sub !xr 0 7; xr := String.sub !xr 7 (String.length !xr - 7))
  done;
  !r

let known_lookup k f g =
  try
    if not (Hashtbl.mem allknowns_merkl_lookups k) then Hashtbl.add allknowns_merkl_lookups k ();
    f (merkle_lookup !allknowns_merkl k)
  with
  | Not_found -> g()
  | MerkleH(x) ->
      Printf.printf "Hash %s in merkle tree for knowns must be expanded.\n" x;
      exit 1
      
let conj_lookup k f g =
  try
    if not (Hashtbl.mem allconjs_merkl_lookups k) then Hashtbl.add allconjs_merkl_lookups k ();
    f (merkle_lookup !allconjs_merkl k)
  with
  | Not_found -> g()
  | MerkleH(x) ->
      Printf.printf "Hash %s in merkle tree for conjectures must be expanded.\n" x;
      exit 1

let nonthm_lookup k f g =
  try
    if not (Hashtbl.mem allnonthms_merkl_lookups k) then Hashtbl.add allnonthms_merkl_lookups k ();
    f (merkle_lookup !allnonthms_merkl k)
  with
  | Not_found -> g()
  | MerkleH(x) ->
      Printf.printf "Hash %s in merkle tree for nonthms must be expanded.\n" x;
      exit 1

let loop_lookup k f g =
  try
    if not (Hashtbl.mem allloops_merkl_lookups k) then Hashtbl.add allloops_merkl_lookups k ();
    f (merkle_lookup !allloops_merkl k)
  with
  | Not_found -> g()
  | MerkleH(x) ->
      Printf.printf "Hash %s in merkle tree for loops must be expanded.\n" x;
      exit 1

let abbrev_lookup k f g =
  try
    if not (Hashtbl.mem allabbrevs_merkl_lookups k) then Hashtbl.add allabbrevs_merkl_lookups k ();
    f (merkle_lookup !allabbrevs_merkl k)
  with
  | Not_found -> g()
  | MerkleH(x) ->
      Printf.printf "Hash %s in merkle tree for abbrevs must be expanded.\n" x;
      exit 1

let clear_declarations () =
  Hashtbl.clear knownclauses;
  Hashtbl.clear abbrevroot;
  Hashtbl.clear abbrevarity;
  Hashtbl.clear abbrevdef;;

let state_root () =
  let h = ref (sha256 (Printf.sprintf "volume:%d:issueno:%d:quorum:%d" !volumeno !issueno !quorum)) in
  List.iter
    (fun (r,(e1,e2,e3)) -> h := sha256 (Printf.sprintf "%s:editor:%d:%s:%s:%s" !h r e1 e2 e3))
    !editors;
  h := sha256 (Printf.sprintf "%s:knownroot:%s" !h (ostr (merkle_root !allknowns_merkl)));
  h := sha256 (Printf.sprintf "%s:abbrevroot:%s" !h (ostr (merkle_root !allabbrevs_merkl)));
  h := sha256 (Printf.sprintf "%s:conjroot:%s" !h (ostr (merkle_root !allconjs_merkl)));
  h := sha256 (Printf.sprintf "%s:nonthmroot:%s" !h (ostr (merkle_root !allnonthms_merkl)));
  h := sha256 (Printf.sprintf "%s:looproot:%s" !h (ostr (merkle_root !allloops_merkl)));
  !h;;

(*** hardcoded genesis state ***)
let genesis_state () =
  editors := [(4,("CXSbZP8nCbob49P892dvsj5A1Dsmrg7AvKn6zYTkABum","CcEFyW5EuzeERJkTVVTSdUY31tLDzApiHuD9kkrK7XYv","TaULp"))];
  volumeno := 1;
  issueno := 1;
  quorum := 4;;
    
type pfctx = (string * tm) list * (string option * clause) list;;

type pfstatetype =
  | PfStateGoal of pfctx * lit list
  | PfStateSep of int * bool

let proving : (string * string * int * pfstatetype list) option ref = ref None;;
    
let eqproving : (string * string * int * pfctx * string option * tm * tm * lit list * pfstatetype list) option ref = ref None;;

let fakefvh : (string,int) Hashtbl.t = Hashtbl.create 1;;
let fakefv : ((string,int) Hashtbl.t) * int ref = (fakefvh,ref (-1));;

let global_loops : (string,loopimpl) Hashtbl.t = Hashtbl.create 5;;
let global_loop_hash : (string,string) Hashtbl.t = Hashtbl.create 5;;
let global_loop_htmlnum : (string,int) Hashtbl.t = Hashtbl.create 5;;
    
let global_latex_ref : (string,string) Hashtbl.t = Hashtbl.create 100;;
let local_latex_ref : (string,string) Hashtbl.t = Hashtbl.create 100;;
let local_latex_ref_counter = ref 0;;
let global_html_ref : (string,string) Hashtbl.t = Hashtbl.create 100;;
let local_html_ref : (string,string) Hashtbl.t = Hashtbl.create 100;;
let local_html_ref_counter = ref 0;;
let htmlthmcount = ref 1;;

let latex_ref x =
  try
    Hashtbl.find local_latex_ref x
  with Not_found ->
    try
      Hashtbl.find global_latex_ref x
    with Not_found ->
      Printf.sprintf "(\\ref{%s})" x;;

let html_ref x =
  try
    Hashtbl.find local_html_ref x
  with Not_found ->
    try
      Hashtbl.find global_html_ref x
    with Not_found ->
      Printf.sprintf "(%s)" x;;

let html_outer_ref fvh x =
  if Hashtbl.mem fvh x then
    x
  else
    let r = Hashtbl.find abbrevroot x in
    Printf.sprintf "<a href='ref.php?h=%s'>%s</a>" r x
    
let rec interp_aim_tm_r m fv nctx =
  match m with
  | Na("e") -> Prim("e",[])
  | Na(x) ->
      begin
	try
	  List.assoc x nctx
	with Not_found ->
	  let (fvh,fvc) = fv in
	  try
	    let v = Hashtbl.find fvh x in
	    Var(v)
	  with Not_found ->
	    if !fvc >= 0 then
	      let v = !fvc in
	      incr fvc;
	      Hashtbl.add fvh x v;
	      Var(v)
	    else
	      raise (Failure("Unknown name " ^ x))
      end
  | Info(InfNam("*"),m,n) -> Prim("*",[interp_aim_tm_r m fv nctx;interp_aim_tm_r n fv nctx])
  | Info(InfNam("\\"),m,n) -> Prim("\\",[interp_aim_tm_r m fv nctx;interp_aim_tm_r n fv nctx])
  | Info(InfNam("/"),m,n) -> Prim("/",[interp_aim_tm_r m fv nctx;interp_aim_tm_r n fv nctx])
  | Implop(Na(h),Tuple(m1,m2,mr)) ->
      begin
	try
	  let hr = Hashtbl.find abbrevroot h in
	  try
	    let ha = Hashtbl.find abbrevarity hr in
	    let ml = m1::m2::mr in
	    if not (List.length ml = ha) then
	      raise (Failure(h ^ " expected to have arity " ^ (string_of_int ha) ^ " but used with arity " ^ (string_of_int (List.length ml))))
	    else
	      Abbrev(hr,interp_aim_spine_r ml fv nctx)
	  with Not_found ->
	    raise (Failure("Unknown arity for " ^ h))
	with Not_found ->
	  raise (Failure("Unknown abbrev " ^ h))
      end
  | Implop(Na(h),m1) ->
      begin
	try
	  let hr = Hashtbl.find abbrevroot h in
	  try
	    let ha = Hashtbl.find abbrevarity hr in
	    let ml = [m1] in
	    if not (List.length ml = ha) then
	      raise (Failure(h ^ " expected to have arity " ^ (string_of_int ha) ^ " but used with arity " ^ (string_of_int (List.length ml))))
	    else
	      Abbrev(hr,interp_aim_spine_r ml fv nctx)
	  with Not_found ->
	    raise (Failure("Unknown arity for " ^ h))
	with Not_found ->
	  raise (Failure("Unknown abbrev " ^ h))
      end
  | _ ->
      output_atree stdout m;
      raise (Failure("cannot interpret as aim term"))
and interp_aim_spine_r s fv nctx =
  match s with
  | m::r -> interp_aim_tm_r m fv nctx::interp_aim_spine_r r fv nctx
  | [] -> []
  
let interp_aim_lit_r l fv nctx =
  match l with
  | Info(InfNam("="),m,n) -> Eq(interp_aim_tm_r m fv nctx,interp_aim_tm_r n fv nctx)
  | Info(InfNam("!="),m,n) -> NEq(interp_aim_tm_r m fv nctx,interp_aim_tm_r n fv nctx)
  | _ ->
      output_atree stdout l;
      raise (Failure "cannot interp as aim lit")

let rec interp_aim_clause_r c fv nctx =
  match c with
  | Info(InfNam("->"),nc1,c2) ->
      interp_aim_clause_neg_r nc1 fv nctx @ interp_aim_clause_r c2 fv nctx
  | Info(InfNam("|"),c1,c2) ->
      interp_aim_clause_r c1 fv nctx @ interp_aim_clause_r c2 fv nctx
  | Info(InfNam("&"),_,_) -> raise (Failure "not a single clause; needs to be split")
  | _ -> [interp_aim_lit_r c fv nctx]
and interp_aim_clause_neg_r c fv nctx =
  match c with
  | Info(InfNam("->"),c1,nc2) ->
      interp_aim_clause_r c1 fv nctx @ interp_aim_clause_neg_r nc2 fv nctx
  | Info(InfNam("&"),nc1,nc2) ->
      interp_aim_clause_neg_r nc1 fv nctx @ interp_aim_clause_neg_r nc2 fv nctx
  | Info(InfNam("|"),_,_) -> raise (Failure "not a single clause; needs to be split")
  | _ -> [lit_negate (interp_aim_lit_r c fv nctx)]

let interp_aim_clause c nctx =
  let fvh = Hashtbl.create 10 in
  let fvc = ref 0 in
  interp_aim_clause_r c (fvh,fvc) nctx

let ostring_sexpr s =
  match s with
  | Some(x) -> Printf.sprintf "\"%s\"" x
  | None -> "NIL"

let root_aname r =
  Printf.sprintf "<a name='r:%s'/e>" r
    
let o_aname s =
  match s with
  | Some(x) -> Printf.sprintf "<a name='%s'/>" x
  | None -> ""

let o_href s y =
  match s with
  | Some(x) -> Printf.sprintf "<a href='#%s'>%s</a>" x y
  | None -> ""

exception UnifFail

let rec occurs_check_p i theta m =
  match m with
  | Var(j) ->
      begin
	match theta.(j) with
	| Some(n) -> occurs_check_p i theta n
	| None -> i = j
      end
  | Prim(_,sp) -> occurs_check_sp_p i theta sp
  | Abbrev(_,sp) -> occurs_check_sp_p i theta sp
  | _ -> false
and occurs_check_sp_p i theta ml =
  match ml with
  | m::mr -> occurs_check_p i theta m || occurs_check_sp_p i theta mr
  | [] -> false

let ss n theta =
  let theta2 : tm array = Array.make n (Skol("fake")) in
  for i = 0 to n-1 do
    match theta.(i) with
    | None -> theta2.(i) <- Var(i)
    | Some(m) -> theta2.(i) <- m
  done;
  theta2

let rec unif_dpairs theta dl : unit =
  if !verbose > 5 then
    begin
      Printf.printf "unif_dpairs dl %d\n" (List.length dl);
      for j = 0 to (Array.length theta - 1) do
              match theta.(j) with
              | None -> Printf.printf "theta %d = None\n" j
              | Some(n) -> Printf.printf "theta %d = Some %s\n" j (tm_str n)
      done;
      List.iter (fun (m,n) -> Printf.printf ". %s =? %s\n" (tm_str m) (tm_str n)) dl;
      flush stdout;
    end;
  match dl with
  | [] -> ()
  | (m,n)::dr when m = n -> unif_dpairs theta dr
  | (Var(i),m)::dr ->
      begin
	match theta.(i) with
	| Some(n) ->
	    unif_dpairs theta ((n,m)::dr)
	| None ->
	    if !verbose > 5 then (Printf.printf "Occurs check %d in %s test\n" i (tm_str m); flush stdout); 
	    if occurs_check_p i theta m then raise UnifFail;
	    if !verbose > 5 then (Printf.printf "Setting %d to %s\n" i (tm_str m); flush stdout);
            let theta1 = ss (Array.length theta) theta in
	    theta.(i) <- Some(tm_subst theta1 m);
            let theta2 = ss (Array.length theta) theta in
            for j = 0 to (Array.length theta - 1) do
              match theta.(j) with
              | None -> ()
              | Some(n) -> theta.(j) <- Some(tm_subst theta2 n)
            done;
	    unif_dpairs theta dr
      end
  | (m,Var(i))::dr ->
      begin
	match theta.(i) with
	| Some(n) ->
	    unif_dpairs theta ((m,n)::dr)
	| None ->
	    if !verbose > 5 then (Printf.printf "Occurs check %d in %s test\n" i (tm_str m); flush stdout); 
	    if occurs_check_p i theta m then raise UnifFail;
	    if !verbose > 5 then (Printf.printf "Setting %d to %s\n" i (tm_str m); flush stdout);
            let theta1 = ss (Array.length theta) theta in
	    theta.(i) <- Some(tm_subst theta1 m);
            let theta2 = ss (Array.length theta) theta in
            for j = 0 to (Array.length theta - 1) do
              match theta.(j) with
              | None -> ()
              | Some(n) -> theta.(j) <- Some(tm_subst theta2 n)
            done;
	    unif_dpairs theta dr
      end
  | (Prim(h1,args1),Prim(h2,args2))::dr when h1 = h2 && List.length args1 = List.length args2 ->
      unif_dpairs theta ((List.map2 (fun m n -> (m,n)) args1 args2) @ dr)
  | (Abbrev(h1,args1),Abbrev(h2,args2))::dr when h1 = h2 && List.length args1 = List.length args2 ->
      unif_dpairs theta ((List.map2 (fun m n -> (m,n)) args1 args2) @ dr)
  | (_,_)::dr -> raise UnifFail

let rec match_dpairs theta dl deltas =
  match dl with
  | [] -> true
  | (Var(i),m)::dr ->
      begin
	match theta.(i) with
	| Some(n) ->
	    if m = n then
	      match_dpairs theta dr deltas
	    else
	      false
	| None ->
	    theta.(i) <- Some(m);
	    match_dpairs theta dr deltas
      end
  | (Skol(x),Skol(y))::dr ->
      if x = y then
	match_dpairs theta dr deltas
      else
	false
  | (Prim(x,sl),Prim(y,tl))::dr ->
      if x = y then
	match_dpairs theta ((List.map2 (fun m n -> (m,n)) sl tl) @ dr) deltas
      else
	false
  | (Abbrev(x,sl),n)::dr when List.mem_assoc x deltas ->
      begin
	try
	  let (arity,m) = List.assoc x deltas in
	  let theta2 = Array.make arity (Skol("fake")) in
	  if arity = List.length sl then
	    begin
	      for i = 0 to arity-1 do
		theta2.(i) <- List.nth sl i
	      done;
	      match_dpairs theta ((tm_subst theta2 m,n)::dr) deltas
	    end
	  else
	    false
	with Not_found ->
	  false
      end
  | (m,Abbrev(x,sl))::dr when List.mem_assoc x deltas ->
      begin
	try
	  let (arity,n) = List.assoc x deltas in
	  let theta2 = Array.make arity (Skol("fake")) in
	  if arity = List.length sl then
	    begin
	      for i = 0 to arity-1 do
		theta2.(i) <- List.nth sl i
	      done;
	      match_dpairs theta ((m,tm_subst theta2 n)::dr) deltas
	    end
	  else
	    false
	with Not_found ->
	  false
      end
  | (Abbrev(x,sl),Abbrev(y,tl))::dr ->
      if x = y then
	match_dpairs theta ((List.map2 (fun m n -> (m,n)) sl tl) @ dr) deltas
      else
	false
  | (_,_)::dr -> false

let match_lit n theta l1 l2 deltas =
  match l1,l2 with
  | Eq(m1,n1),Eq(m2,n2) -> match_dpairs theta [(m1,m2);(n1,n2)] deltas
  | NEq(m1,n1),NEq(m2,n2) -> match_dpairs theta [(m1,m2);(n1,n2)] deltas
  | _,_ -> false

let delta_eq l r deltas =
  let theta : tm option array = Array.make 0 None in (*** fake value to call match_dpairs with ***)
  match_dpairs theta [(l,r)] deltas

(*** dl: dpairs, ground; all vars are in lhs and rhs ***)
let rec ematch1_dpairs n theta lhs rhs dl deltas =
  match dl with
  | [] -> true
  | (m1,m2)::dr ->
      let savetheta : tm option array = Array.make n None in
      for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
      begin
	try
	  if not (match_dpairs theta [(lhs,m1);(rhs,m2)] deltas) then raise Exit;
	  if not (ematch1_dpairs n theta lhs rhs dr deltas) then raise Exit;
	  true
	with Exit ->
	  try
	    for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;	  
	    if not (match_dpairs theta [(lhs,m2);(rhs,m1)] deltas) then raise Exit;
	    if not (ematch1_dpairs n theta lhs rhs dr deltas) then raise Exit;
	    true
	  with Exit ->
	    for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;	  
	    ematch1_dpairs_2 n theta lhs rhs dl deltas
      end
and ematch1_dpairs_2 n theta lhs rhs dl deltas =
  match dl with
  | [] -> true
  | (Skol(x),Skol(y))::dr ->
      if x = y then
	ematch1_dpairs n theta lhs rhs dr deltas
      else
	false
  | (Prim(x,sl),Prim(y,tl))::dr ->
      if x = y then
	ematch1_dpairs n theta lhs rhs ((List.map2 (fun m n -> (m,n)) sl tl) @ dr) deltas
      else
	false
  | (Abbrev(x,sl),Abbrev(y,tl))::dr ->
      if x = y then
	ematch1_dpairs n theta lhs rhs ((List.map2 (fun m n -> (m,n)) sl tl) @ dr) deltas
      else
	false
  | (_,_)::dr -> false

let ematch1_lit n theta lhs rhs l1 l2 deltas =
  match l1,l2 with
  | Eq(m1,n1),Eq(m2,n2) -> ematch1_dpairs n theta lhs rhs [(m1,m2);(n1,n2)] deltas
  | NEq(m1,n1),NEq(m2,n2) -> ematch1_dpairs n theta lhs rhs [(m1,m2);(n1,n2)] deltas
  | _,_ -> false

let sym_lit l =
  match l with
  | Eq(m,n) -> Eq(n,m)
  | NEq(m,n) -> NEq(n,m)

let obvious_by_res n maincl gunits deltas =
  let theta : tm option array = Array.make n None in
  let rec obvious_by_res_r maincl gunits =
    match maincl with
    | [] -> true
    | l::maincr -> obvious_by_res_r_2 (lit_negate l) maincr gunits gunits
  and obvious_by_res_r_2 nl maincr gunits allgunits =
    match gunits with
    | gl::gunitr ->
	begin
	  let savetheta : tm option array = Array.make n None in
	  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	  try
	    if not (match_lit n theta nl gl deltas) then raise Exit;
	    if not (obvious_by_res_r maincr allgunits) then raise Exit;
	    true
	  with Exit ->
	    for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
	    try
	      let gl2 = sym_lit gl in
	      if not (match_lit n theta nl gl2 deltas) then raise Exit;
	      if not (obvious_by_res_r maincr allgunits) then raise Exit;
	      true
	    with
	    | Exit ->
		for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
		obvious_by_res_r_2 nl maincr gunitr allgunits
	end
    | [] -> false
  in
  obvious_by_res_r maincl gunits

let obvious_by_para_1 n maincl gunits deltas =
  let theta : tm option array = Array.make n None in
  let rec obvious_by_para_1_r maincl gunits =
    match maincl with
    | [] -> true
    | (Eq(l,r) as li)::maincr ->
	(obvious_by_para_1_r_3 l r maincr gunits || obvious_by_para_1_r_2 (lit_negate li) maincr gunits gunits)
    | l::maincr -> obvious_by_para_1_r_2 (lit_negate l) maincr gunits gunits
  and obvious_by_para_1_r_2 nl maincr gunits allgunits =
    match gunits with
    | gl::gunitr ->
	begin
	  let savetheta : tm option array = Array.make n None in
	  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	  try
	    if not (match_lit n theta nl gl deltas) then raise Exit;
	    if not (obvious_by_para_1_r maincr allgunits) then raise Exit;
	    true
	  with Exit ->
	    for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
	    obvious_by_para_1_r_2 nl maincr gunitr allgunits
	end
    | [] -> false
  and obvious_by_para_1_r_3 lhs rhs maincl gunits =
    match maincl with
    | [] -> (*** all lits except the lhs = rhs eqn have been resolved; now use eqn to paramod either a unit diseqn to be negrefl or two units to be compl ***)
	(obvious_by_para_1_r_5 lhs rhs gunits || obvious_by_para_1_r_6 lhs rhs gunits gunits)
    | l::maincr -> obvious_by_para_1_r_4 lhs rhs (lit_negate l) maincr gunits gunits
  and obvious_by_para_1_r_4 lhs rhs nl maincr gunits allgunits =
    match gunits with
    | gl::gunitr ->
	begin
	  let savetheta : tm option array = Array.make n None in
	  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	  try
	    if not (match_lit n theta nl gl deltas) then raise Exit;
	    if not (obvious_by_para_1_r_3 lhs rhs maincr allgunits) then raise Exit;
	    true
	  with Exit ->
	    for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
	    obvious_by_para_1_r_4 lhs rhs nl maincr gunitr allgunits
	end
    | [] -> false
  and obvious_by_para_1_r_5 lhs rhs gunits =
    match gunits with
    | NEq(lhs2,rhs2)::gunitr ->
	begin
	  let savetheta : tm option array = Array.make n None in
	  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	  try
	    if not (ematch1_dpairs n theta lhs rhs [(lhs2,rhs2)] deltas) then raise Exit;
	    true
	  with Exit ->
	    try
	      if not (ematch1_dpairs n theta rhs lhs [(lhs2,rhs2)] deltas) then raise Exit;
	      true
	    with Exit ->
	      for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
	      obvious_by_para_1_r_5 lhs rhs gunitr
	end
    | _::gunitr -> obvious_by_para_1_r_5 lhs rhs gunitr
    | [] -> false
  and obvious_by_para_1_r_6 lhs rhs gunits allgunits =
    match gunits with
    | gl::gunitr ->
	begin
	  let savetheta : tm option array = Array.make n None in
	  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	  try
	    if obvious_by_para_1_r_7 lhs rhs (lit_negate gl) allgunits then
	      true
	    else
	      raise Exit
	  with Exit ->
	    for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
	    obvious_by_para_1_r_6 lhs rhs gunitr allgunits
	end
    | [] -> false
  and obvious_by_para_1_r_7 lhs rhs ngl gunits =
    match gunits with
    | gl::gunitr ->
	begin
	  let savetheta : tm option array = Array.make n None in
	  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	  try
	    if not (ematch1_lit n theta lhs rhs ngl gl deltas) then raise Exit;
	    true
	  with Exit ->
	    try
	      for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
	      if not (ematch1_lit n theta rhs lhs ngl gl deltas) then raise Exit;
	      true
	    with Exit ->
	      let gl2 = sym_lit gl in
	      try
		for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
		if not (ematch1_lit n theta lhs rhs ngl gl2 deltas) then raise Exit;
		true
	      with Exit ->
		try
		  for i = 0 to (n-1) do savetheta.(i) <- theta.(i) done;
		  if not (ematch1_lit n theta rhs lhs ngl gl2 deltas) then raise Exit;
		  true
		with Exit ->
		  for i = 0 to (n-1) do theta.(i) <- savetheta.(i) done;
		  obvious_by_para_1_r_7 lhs rhs ngl gunitr
	end
    | [] -> false
  in
  obvious_by_para_1_r maincl gunits

let obvious_by_para_2 n maincl guniteqns deltas =
  false

let obvious_by_para_3 lhs rhs gunits deltas =
  let theta : tm option array = Array.make 0 None in (*** fake value to call ematch1 with ***)
  let rec obvious_by_para_3_r_5 gunits =
    match gunits with
    | NEq(lhs2,rhs2)::gunitr ->
	if ematch1_dpairs 0 theta lhs rhs [(lhs2,rhs2)] deltas then
	  true
	else if ematch1_dpairs 0 theta rhs lhs [(lhs2,rhs2)] deltas then
	  true
	else
	  obvious_by_para_3_r_5 gunitr
    | _::gunitr -> obvious_by_para_3_r_5 gunitr
    | [] -> false
  and obvious_by_para_3_r_6 gunits allgunits =
    match gunits with
    | gl::gunitr ->
	if obvious_by_para_3_r_7 (lit_negate gl) allgunits then
	  true
	else
	  obvious_by_para_3_r_6 gunitr allgunits
    | [] -> false
  and obvious_by_para_3_r_7 ngl gunits =
    match gunits with
    | gl::gunitr ->
	if ematch1_lit 0 theta lhs rhs ngl gl deltas then
	  true
	else if ematch1_lit 0 theta rhs lhs ngl gl deltas then
	  true
	else
	  obvious_by_para_3_r_7 ngl gunitr
    | [] -> false
  in
  obvious_by_para_3_r_5 gunits || obvious_by_para_3_r_6 gunits gunits

let ensureobvious pctx nll thn obyl =
  let (nctx,hyps) = pctx in
  let refclauses = ref [] in
  let refdeltas = ref [] in
  if thn then
    begin
      match hyps with
      | (_,cl)::hypr -> refclauses := cl::!refclauses
      | [] ->
	  Printf.printf "justification failed at line %d char %d\n" !lineno !charno;
	  raise (Failure "nothing to link")
    end;
  begin
    match obyl with
    | None -> if !fakelearn then incr fakedepth;
    | Some(byl) ->
	if !fakelearn then
	  begin
	    List.iter
	      (fun b ->
		try
		  fakedepth := !fakedepth + Hashtbl.find fakedepthh b
		with Not_found ->
		  incr fakedepth)
	      byl
	  end;
	List.iter
	  (fun x ->
	    try
	      let cl = List.assoc (Some(x)) hyps in
	      if !sexpr then Hashtbl.replace hyprefs (Some(x)) true;
	      refclauses := cl::!refclauses
	    with Not_found ->
	      try
		let cl = clause_with_root (Hashtbl.find knownclauses x) in
		if !sexpr then Printf.printf "(KNOWNDEP \"%s\")\n" x;
		refclauses := cl::!refclauses
	      with Not_found ->
		try
		  let r = Hashtbl.find abbrevroot x in
		  if !sexpr then Printf.printf "(DELTADEP \"%s\")\n" x;
		  try
		    let arity = Hashtbl.find abbrevarity r in
		    let m = Hashtbl.find abbrevdef r in
		    refdeltas := (r,(arity,m))::!refdeltas
		  with Not_found ->
		    Printf.printf "the definition of %s is opaque here and cannot be used in a justification at line %d char %d\n" x !lineno !charno;
		    raise (Failure "attempt to expand opaque defn")
		with Not_found ->
		  Printf.printf "cannot find reference %s at line %d char %d\n" x !lineno !charno;
		  raise (Failure "unknown ref"))
	  byl
  end;
  List.iter (fun nl -> refclauses := [nl]::!refclauses) nll;
  if !verbose > 9 then
    begin
      Printf.printf "here are all ref clauses:\n";
      List.iter
	(fun cl ->
	  Printf.printf ". %s\n" (clause_str cl))
	!refclauses;
    end;
  if List.mem [] !refclauses then (*** empty clause known ***)
    ()
  else if (List.exists (fun cl1 -> match cl1 with [NEq(l,r)] when delta_eq l r !refdeltas -> true | _ -> false) !refclauses) then (*** negated reflexivity unit ***)
    ()
  else if (List.exists (fun cl1 -> match cl1 with [l1] -> List.mem [lit_negate l1] !refclauses | _ -> false) !refclauses) then (*** unit conflict ***)
    ()
  else
    let grounduniteqns = ref [] in
    let allgroundunits = ref [] in
    let mainclauses = ref [] in
    List.iter
      (fun cl ->
	let n = clause_var_upperbd cl in
	if n = 0 then (*** ground ***)
	  begin
	    match cl with
	    | [Eq(l,r) as li] ->
		grounduniteqns := (l,r)::!grounduniteqns;
		grounduniteqns := (r,l)::!grounduniteqns;
		allgroundunits := li::!allgroundunits;
		allgroundunits := Eq(r,l)::!allgroundunits;
	    | [li] -> allgroundunits := li::!allgroundunits
	    | _ -> mainclauses := (0,cl)::!mainclauses
	  end
	else
	  mainclauses := (n,cl)::!mainclauses)
      !refclauses;
    match !mainclauses with
    | (_::_::_) ->
	Printf.printf "justification failed at line %d char %d, too many open/nonunit clause refs (should be 0 or 1)\n" !lineno !charno;
	raise (Failure "failed justification")	
    | [(n,maincl)] ->
	begin
	  if obvious_by_res n maincl !allgroundunits !refdeltas then
	    ()
	  else if obvious_by_para_1 n maincl !allgroundunits !refdeltas then
	    ()
	  else if obvious_by_para_2 n maincl !grounduniteqns !refdeltas then
	    ()
	  else
	    begin
	      Printf.printf "justification failed at line %d char %d, could not use main clause to complete the inference\n" !lineno !charno;
	      raise (Failure "failed justification")	
	    end
	end
    | [] -> (*** no main clause and no unit conflict; last hope is delta or unit eqn para into another ground ***)
	if List.exists
	    (fun (l,r) -> obvious_by_para_3 l r !allgroundunits !refdeltas)
	    !grounduniteqns
	then
	  ()
	else
	  begin
	    Printf.printf "justification failed at line %d char %d, all references are unit and ground, but no conflict or rewriting found to complete the inference\n" !lineno !charno;
	    if !verbose > 5 then
	      begin
		Printf.printf "here are all ref clauses:\n";
		List.iter
		  (fun cl ->
		    Printf.printf ". %s\n" (clause_str cl))
		  !refclauses;
	      end;
	    raise (Failure "failed justification")
	  end

let clause_subsumes_p cl1 cl2 =
  let n1 = clause_var_upperbd cl1 in
  let nll2 = negate_clause cl2 in
  obvious_by_res n1 cl1 nll2 []
	    
let printf_latex_by byl =
  match byl with
  | [] -> ()
  | [by1] ->
      Printf.fprintf !latexc " by ";
      Printf.fprintf !latexc "%s" (latex_ref by1)
  | (by1::by2::byr) ->
      Printf.fprintf !latexc " by ";
      Printf.fprintf !latexc "%s" (latex_ref by1);
      let rec printf_latex_byr by2 byr =
	match byr with
	| [] -> Printf.fprintf !latexc " and %s" (latex_ref by2)
	| by3::byrr ->
	    Printf.fprintf !latexc ", %s" (latex_ref by2);
	    printf_latex_byr by3 byrr
      in
      printf_latex_byr by2 byr

let printf_html_by c byl =
  match byl with
  | [] -> ()
  | [by1] ->
      Printf.fprintf c " by ";
      Printf.fprintf c "%s" (html_ref by1)
  | (by1::by2::byr) ->
      Printf.fprintf c " by ";
      Printf.fprintf c "%s" (html_ref by1);
      let rec printf_html_byr by2 byr =
	match byr with
	| [] -> Printf.fprintf c " and %s" (html_ref by2)
	| by3::byrr ->
	    Printf.fprintf c ", %s" (html_ref by2);
	    printf_html_byr by3 byrr
      in
      printf_html_byr by2 byr

let rec reduce_goalstack gs =
  match gs with
  | PfStateSep(_,false)::gr -> reduce_goalstack gr
  | _ -> gs

let rec easygens m =
  let rl = ref [(Var(0),Some(m));(m,None)] in
  begin
    match m with
    | Prim(x,sp) -> List.iter (fun (sp,th) -> rl := (Prim(x,sp),th)::!rl) (easygens_sp sp)
    | Abbrev(x,sp) -> List.iter (fun (sp,th) -> rl := (Abbrev(x,sp),th)::!rl) (easygens_sp sp)
    | _ -> ()
  end;
  !rl
and easygens_sp ml =
  match ml with
  | [] -> [([],None)]
  | m::mr ->
      let y = easygens m in
      let z = easygens_sp mr in
      let rl = ref [] in
      List.iter
	(fun (n,o1) ->
	  List.iter
	    (fun (sp,o2) ->
	      match o2 with
	      | None -> rl := (n::sp,o1)::!rl
	      | Some(n2) ->
		  match o1 with
		  | None -> rl := (n::sp,o2)::!rl
		  | Some(n1) -> if n1 = n2 then rl := (n::sp,o1)::!rl)
	    z)
	y;
      !rl

let output_fakedists l r =
  let lr = tm_root l in
  let rr = tm_root r in
  Printf.printf "FAKEDIST\n%s\n%s\n1.0\n" lr rr;
  List.iter
    (fun (m,o) ->
      if not (o = None) then
	begin
	  if !verbose > 8 then Printf.printf "%s %s\n" (tm_root m) (tm_str m);
	  Printf.printf "FAKEDIST\n%s\n%s\n1.0\n" lr (tm_root m);
	  Printf.printf "FAKEDIST\n%s\n%s\n0.0\n" rr (tm_root m)
	end)
    (easygens r);
  List.iter
    (fun (m,o) ->
      if not (o = None) then
	begin
	  if !verbose > 8 then Printf.printf "%s %s\n" (tm_root m) (tm_str m);
	  Printf.printf "FAKEDIST\n%s\n%s\n1.0\n" rr (tm_root m);
	  Printf.printf "FAKEDIST\n%s\n%s\n0.0\n" lr (tm_root m)
	end)
    (easygens l)

let evaluate_eqpftacitem thmname thmhash n pctx label l r nll goalstack epfitem =
  match epfitem with
  | EqLink(a,obyl) ->
      let (nctx,hyps) = pctx in
      let newr = interp_aim_tm_r (ltree_to_atree a) fakefv nctx in
      ensureobvious pctx [NEq(r,newr)] false obyl;
      if !fakelearn then output_fakedists l newr;
      eqproving := Some(thmname,thmhash,n,pctx,label,l,newr,nll,goalstack);
      if !latex then
	begin
	  Printf.fprintf !latexc "\\\\\n";
	  Printf.fprintf !latexc " = & ";
	  ltree_to_latex_matharray 1 !latexc a;
	  match obyl with
	  | None -> ()
	  | Some(byl) ->
	      Printf.fprintf !latexc " & {\\mbox{ by }} ";
	      let fst = ref true in
	      List.iter (fun b -> if !fst then fst := false else Printf.fprintf !latexc ", "; Printf.fprintf !latexc "%s" (latex_ref b)) byl;
	end;
      if !html then
	begin
	  Printf.printf "</tr>\n<tr>";
	  Printf.printf "<td>=</td><td>";
	  ltree_to_html_math stdout (html_outer_ref fakefvh) a;
	  Printf.printf "</td>";
	  match obyl with
	  | None -> ()
	  | Some(byl) ->
	      Printf.printf "<td> by ";
	      let fst = ref true in
	      List.iter (fun b -> if !fst then fst := false else Printf.printf ", "; Printf.printf "%s" (html_ref b)) byl;
	end
  | EqDone ->
      let (nctx,hyps) = pctx in
(*      Printf.printf "After EqDone pusing eqn %s = %s onto hyps\n" (tm_str l) (tm_str r); *)
      if !latex then Printf.fprintf !latexc ".\n\\end{eqnarray*}\n";
      if !html then Printf.printf "</tr></table><br/>\n";
      eqproving := None;
      if !sexpr then Hashtbl.replace hyprefs label false;
      proving := Some(thmname,thmhash,n,(PfStateGoal((nctx,(label,[Eq(l,r)])::hyps),nll)::goalstack))

let rec structure_pfstate i g1 gs =
  match gs with
  | (PfStateGoal(g2a,g2b)::gr) ->
      (PfStateSep(i,true)::g1::structure_pfstate i (PfStateGoal(g2a,g2b)) gr)
  | gs ->
      (PfStateSep(i,true)::g1::PfStateSep(i,false)::gs)

let clause_of_refut r =
  match r with
  | Hyp(cl) -> (0,cl)
  | Kn(n,cl,_) -> (n,cl)
  | Def(n,cl,_,_) -> (n,cl)
  | Subst(n,cl,_,_,_) -> (n,cl)
  | Refl(n,cl,_) -> (n,cl)
  | Res(n,cl,_,_,_) -> (n,cl)
  | Para(n,cl,_,_,_,_,_) -> (n,cl)

let addnewsos r =
  let (n,cl) = clause_of_refut r in
  if cl = [] then
    raise (EmptyClause(r))
  else
    sos2 := r::!sos2

let rec delta_tms m =
  match m with
  | Prim(x,ml) ->
      let spl = delta_spines ml in
      let mll = ref [] in
      List.iter
	(fun (ml1,xl) ->
	  mll := (Prim(x,ml1),xl)::!mll)
	spl;
      !mll
  | Abbrev(x,ml) ->
      let spl = delta_spines ml in
      let mll = ref [] in
      List.iter
	(fun (ml1,xl) ->
	  mll := (Prim(x,ml1),xl)::!mll)
	spl;
      begin
	let arity = Hashtbl.find abbrevarity x in
	let mdef = Hashtbl.find abbrevdef x in
	let theta2 = Array.make arity (Skol("fake")) in
	if arity = List.length ml then
	  begin
	    for i = 0 to arity-1 do
	      theta2.(i) <- List.nth ml i
	    done;
	    let ndef = tm_subst theta2 mdef in
	    let nl = delta_tms ndef in
	    mll := (ndef,[x])::!mll;
	    List.iter
	      (fun (n,xl) -> mll := (n,rem_dups (x::xl))::!mll)
	      nl
	  end;
      end;
      !mll
  | _ -> []
and delta_spines ml =
  match ml with
  | [] -> []
  | m::mr ->
      let ml = delta_tms m in
      let mrl = delta_spines mr in
      let mll = ref [] in
      List.iter
	(fun (m1,xl) ->
	  mll := (m1::mr,xl)::!mll;
	  List.iter
	    (fun (mr1,yl) ->
	      mll := (m1::mr1,rem_dups (xl @ yl))::!mll)
	    mrl)
	ml;
      List.iter
	(fun (mr1,yl) ->
	  mll := (m::mr1,yl)::!mll)
	mrl;
      !mll

let delta_lits l =
  match l with
  | Eq(m,n) ->
      let ml = delta_tms m in
      let nl = delta_tms n in
      let ll = ref [] in
      List.iter
	(fun (m1,xl) ->
	  ll := (Eq(m1,n),xl)::!ll;
	  List.iter
	    (fun (n1,yl) ->
	      ll := (Eq(m1,n1),rem_dups (xl @ yl))::!ll)
	    nl)
	ml;
      List.iter
	(fun (n1,yl) ->
	  ll := (Eq(m,n1),yl)::!ll)
	nl;
      !ll
  | NEq(m,n) ->
      let ml = delta_tms m in
      let nl = delta_tms n in
      let ll = ref [] in
      List.iter
	(fun (m1,xl) ->
	  ll := (NEq(m1,n),xl)::!ll;
	  List.iter
	    (fun (n1,yl) ->
	      ll := (NEq(m1,n1),rem_dups (xl @ yl))::!ll)
	    nl)
	ml;
      List.iter
	(fun (n1,yl) ->
	  ll := (NEq(m,n1),yl)::!ll)
	nl;
      !ll

let rec delta_clauses cl =
  match cl with
  | [] -> []
  | l::cr ->
      let ll = delta_lits l in
      let crl = delta_clauses cr in
      let cll = ref [] in
      List.iter
	(fun (l1,xl) ->
	  cll := (l1::cr,xl)::!cll;
	  List.iter
	    (fun (cr1,yl) ->
	      cll := (l1::cr1,rem_dups (xl @ yl))::!cll)
	    crl)
	ll;
      List.iter
	(fun (cr1,yl) ->
	  cll := (l::cr1,yl)::!cll)
	crl;
      !cll

let delta_refuts r =
  let (n,cl) = clause_of_refut r in
  let dll = delta_clauses cl in
  List.map
    (fun (dl,xl) -> Def(n,dl,xl,r))
    dll

let hdist = ref false

let dist lhs rhs =
  if !hdist then
    let n = tm_var_upperbd lhs in
    let theta = Array.make n (Skol("fake")) in
    let (lhs,_) = okify_tm lhs theta 0 in
    let n = tm_var_upperbd rhs in
    let theta = Array.make n (Skol("fake")) in
    let (rhs,_) = okify_tm rhs theta 0 in
    let lr = tm_root lhs in
    let rr = tm_root rhs in
    begin
      if !verbose > 9 then
	begin
	  Printf.printf "dist:\n%s\n%s\n%s\n%s\n" (tm_str lhs) lr (tm_str rhs) rr;
	  flush stdout;
	end;
      try
	Hashtbl.find fakedist (lr,rr)
      with Not_found ->
	try
	  Hashtbl.find fakedist (rr,lr)
	with Not_found ->
	  if !verbose > 9 then (Printf.printf "computing fake dist by hash\n"; flush stdout);
	  let y = int_of_string (Printf.sprintf "0x%s" (String.sub lr 0 6)) in
	  let z = int_of_string (Printf.sprintf "0x%s" (String.sub rr 0 6)) in
	  (float_of_int (y lxor z)) /. 16777.215
    end
  else
    match !constdist with
    | Some(d) -> d
    | _ -> 5.0

let onestep_at vbd m lhs rhs =
  begin
    try
      let theta : tm option array = Array.make vbd None in
      unif_dpairs theta [(m,lhs)];
      let theta2 = ss vbd theta in
      let z : tm = tm_subst theta2 rhs in
      (***      Printf.printf "z = %s\n" (tm_str z); ***)
      Some(z,theta2)
    with UnifFail -> None
  end

let rec onestep_pos vbd m lhs rhs =
  let rl = ref [] in
  begin
    try
      let theta : tm option array = Array.make vbd None in
      unif_dpairs theta [(m,lhs)];
      let theta2 = ss vbd theta in
      rl := [(rhs,theta2)]
    with UnifFail -> ()
  end;
  match m with
  | Prim(x,sl) ->
      List.iter
	(fun (sp,theta) -> rl := (Prim(x,sp),theta)::!rl)
	(onestep_sp_pos vbd sl lhs rhs);
      !rl
  | Abbrev(x,sl) ->
      List.iter
	(fun (sp,theta) -> rl := (Abbrev(x,sp),theta)::!rl)
	(onestep_sp_pos vbd sl lhs rhs);
      !rl
  | _ -> !rl
and onestep_sp_pos vbd ml lhs rhs =
  match ml with
  | [] -> []
  | m::mr ->
      let rl = ref [] in
      List.iter (fun (n,theta) -> rl := (n::List.map (tm_subst theta) mr,theta)::!rl) (onestep_pos vbd m lhs rhs);
      List.iter (fun (nr,theta) -> rl := ((tm_subst theta m)::nr,theta)::!rl) (onestep_sp_pos vbd mr lhs rhs);
      !rl

let relnormvars_tm vbd vbd2 z theta =
  let theta2 = Array.make vbd2 (Skol("fake")) in
  let theta3 = Array.make vbd2 (Skol("fake")) in
  let vl = tm_frees z in
  for i = 0 to vbd-1 do
    theta3.(i) <- Var(i)
  done;
  let k = ref vbd in
  for j = vbd to vbd2 do
    if List.mem j vl then
      begin
	theta3.(j) <- Var(!k);
	incr k
      end
  done;
  for i = 0 to vbd do
    theta2.(i) <- tm_subst theta3 theta.(i)
  done;
  (tm_subst theta3 z,theta2)

let onestep vbd m =
  let rl = ref [] in
  List.iter
    (fun (nm,lhs,rhs) ->
      let lhs = push_tm_frees vbd lhs in
      let rhs = push_tm_frees vbd rhs in
      let vbd2 = clause_var_upperbd [Eq(lhs,rhs)] in
      List.iter
	(fun (z,theta) ->
	  (*	  let (z,theta) = relnormvars_tm vbd vbd2 z theta in *) (** normalizing here is not good enough **)
	  rl := (nm,z,theta) :: !rl)
	(onestep_pos vbd2 m lhs rhs);
      List.iter
	(fun (z,theta) ->
	  (* let (z,theta) = relnormvars_tm vbd vbd2 z theta in *)  (** normalizing here is not good enough **)
	  rl := (nm,z,theta) :: !rl)
	(onestep_pos vbd2 m rhs lhs))
    !usableeqns;
  !rl

let rec get_pos m pos : (tm -> tm) * tm =
(*  Printf.printf "get_pos %s" (tm_str m); List.iter (fun i -> Printf.printf " %d" i) pos; Printf.printf "\n"; *)
  match pos with
  | [] -> ((fun n -> n),m)
  | i::posr ->
      match m with
      | Prim(x,args) ->
	  let (f,m) = get_pos_spine args (i-1) posr in
	  ((fun n -> Prim(x,f n)),m)
      | Abbrev(x,args) ->
	  let (f,m) = get_pos_spine args (i-1) posr in
	  ((fun n -> Abbrev(x,f n)),m)
      | _ -> raise (Failure "bad 1")
and get_pos_spine s i pos : (tm -> tm list) * tm =
(*  Printf.printf "get_pos_spine %d" i; List.iter (fun m -> Printf.printf " %s" (tm_str m)) s; List.iter (fun i -> Printf.printf " %d" i) pos; Printf.printf "\n"; *)
  match s with
  | [] -> raise (Failure "bad 2")
  | k::r ->
      if i = 0 then
	let (f,m) = get_pos k pos in
	((fun n -> f n::r),m)
      else
	let (f,m) = get_pos_spine r (i-1) pos in
	((fun n -> k::f n),m)

let abstracttimecheck () =
  incr abstracttimecount;
  if !verbose > 2 then (Printf.printf "abstrtime %d %f\n" !abstracttimecount (Unix.gettimeofday()); flush stdout);
  match !abstracttimelimit with
  | None -> ()
  | Some(i) -> if !abstracttimecount >= i then raise AbstractTimeLimit

let send_advisor_pair m n =
  match (!advisorsocketin,!advisorsocketout) with
  | (Some(sin),Some(sout)) ->
      output_byte sout 0;
      send_tm sout m;
      send_tm sout n;
      flush sout;
      let by = input_byte sin in
      if by = 0 then (*** OK, and no special transitivity suggested ***)
	[]
      else (*** this should only be accepted if m and n share no vars and the suggesteds have no vars ***)
	begin
	  let xl = tm_frees m in
	  let yl = tm_frees n in
	  try
	    ignore (List.find (fun x -> List.mem x yl) xl);
	    []
	  with Not_found ->
	    let kl = ref [] in
	    for i = 1 to by do
	      let k = rec_tm sin in
	      if tm_frees k = [] then kl := k::!kl
	    done;
	    List.rev !kl
	end
  | _ -> []

let output_bool sout b =
  output_byte sout (if b then 1 else 0)

let output_string sout s =
  output_byte sout (String.length s);
  for i = 0 to String.length s - 1 do
    output_byte sout (Char.code s.[i])
  done

let send_advisor_state(vbd,m,n,sd) =
  match (!advisorsocketin,!advisorsocketout) with
  | (Some(sin),Some(sout)) ->
     output_byte sout 1;
     output_byte sout vbd;
     send_tm sout m;
     send_tm sout n;
     begin
       match sd with
       | None -> output_byte sout 0;
       | Some(b,pos) ->
          output_byte sout 1;
          output_bool sout b;
          output_byte sout (List.length pos);
          List.iter (fun i -> output_byte sout i) pos
     end;
     flush sout;
  | _ -> ()

let dist_if_move_default i =
  match !constdist with
  | Some(d) -> d
  | _ -> 5.0
  
let dist_if_move i =
  if !dyndist then
    begin
      match (!advisorsocketin,!advisorsocketout) with
      | (Some(sin),Some(sout)) ->
         output_byte sout 2;
         output_byte sout i;
         flush sout;
         let by = input_byte sin in
         if not (by = 2) then raise (Failure "advisor comm problem");
         let by1 = input_byte sin in
         let by2 = input_byte sin in
         let d = (float_of_int ((by1 lsl 8) lor by2)) /. 1000.0 in
         if !verbose > 9 then Printf.printf "advisor dist if move %d: %f\n" i d;
         d
      | _ -> dist_if_move_default i
    end
  else
    dist_if_move_default i

let dist_if_rew b nm m1 n1 =
  if m1 = n1 then
    0.0
  else if !dyndist then
    begin
      match (!advisorsocketin,!advisorsocketout) with
      | (Some(sin),Some(sout)) ->
         output_byte sout 3;
         output_bool sout b;
         output_string sout nm;
         send_tm sout m1;
         send_tm sout n1;
         flush sout;
         let by = input_byte sin in
         if not (by = 2) then raise (Failure "advisor comm problem");
         let by1 = input_byte sin in
         let by2 = input_byte sin in
         let d = (float_of_int ((by1 lsl 8) lor by2)) /. 1000.0 in
         if !verbose > 9 then Printf.printf "advisor dist if rew %b %s: %f\n" b nm d;
         d
      | _ -> dist m1 n1
    end
  else
    dist m1 n1

let dist_to_lhs k m =
  if k = m then
    0.0
  else if !dyndist then
    begin
      match (!advisorsocketin,!advisorsocketout) with
      | (Some(sin),Some(sout)) ->
         output_byte sout 2;
         send_tm sout k;
         flush sout;
         let by = input_byte sin in
         if not (by = 2) then raise (Failure "advisor comm problem");
         let by1 = input_byte sin in
         let by2 = input_byte sin in
         let d = (float_of_int ((by1 lsl 8) lor by2)) /. 1000.0 in
         if !verbose > 9 then Printf.printf "advisor dist to lhs %f %s\n" d (tm_str k);
         d
      | _ -> dist m k
    end
  else
    match !constdist with
    | None -> 0.0
    | Some(d) -> d

let dist_to_rhs k m =
  if k = m then
    0.0
  else if !dyndist then
    begin
      match (!advisorsocketin,!advisorsocketout) with
      | (Some(sin),Some(sout)) ->
         output_byte sout 1;
         send_tm sout k;
         flush sout;
         let by = input_byte sin in
         if not (by = 2) then raise (Failure "advisor comm problem");
         let by1 = input_byte sin in
         let by2 = input_byte sin in
         let d = (float_of_int ((by1 lsl 8) lor by2)) /. 1000.0 in
         if !verbose > 9 then Printf.printf "advisor dist to rhs %f %s\n" d (tm_str k);
         d
      | _ -> dist k m
    end
  else
    match !constdist with
    | None -> 0.0
    | Some(d) -> d

let pos_str l = List.fold_left (fun x y -> x ^ " " ^ string_of_int y) "" l

(** state:
  (vbd[maxfreevar],lhs,rhs,
    (chosen side,position of cursor in side) [unless root],
    maxremainingstepslimit) **)
    
type state = int * tm * tm * bool option * int list * int

type action =
  | MoveDown of int
  | RewWithEqn of string * bool * tm * tm * tm array

let batchdist al =
  match (!advisorsocketin,!advisorsocketout) with
  | (Some(sin),Some(sout)) ->
     output_byte sout 4;
     let poss : (action option) Array.t = Array.make 177 None in
     let ll = ref [] in
     List.iter
       (fun a ->
         match a with
         | MoveDown(i) ->
            if ((i > 0) && (i < 4)) then poss.(i-1) <- Some(a)
         | RewWithEqn(idnm,forw,_,_,_) ->
            try
              let eqnind = Hashtbl.find eqnindh idnm in
              poss.(2*eqnind + 3 + (if forw then 0 else 1)) <- Some(a)
            with Not_found -> ())
       al;
     for i = 0 to 176 do
       match poss.(i) with
       | None -> output_byte sout 0;
       | _ -> output_byte sout 1;
     done;
     flush sout;
     let by = input_byte sin in
     if not (by = 3) then raise (Failure "advisor comm problem");
     for i = 0 to 176 do
       match poss.(i) with
       | None -> ()
       | Some(a) ->
          let by1 = input_byte sin in
          let by2 = input_byte sin in
          let d = (float_of_int ((by1 lsl 8) lor by2)) /. 1000.0 in
          ll := List.merge (fun (_,d1) (_,d2) -> compare d1 d2) !ll [(a,d)]
     done;
     !ll
  | _ -> raise (Failure "no advisor given so cannot send batch distance request")

let search conjname lhs rhs =
  if !verbose > 2 then (Printf.printf "Searching for %s\n" conjname; flush stdout);
  let dn : (tm * tm,unit) Hashtbl.t = Hashtbl.create 100 in
  let rec bestsearch(vbd,m,n,sd,l) =
    if sd = None then Hashtbl.add dn (m,n) (); (* to prevent looping *)
    if !verbose > 4 then
      begin
        match sd with
        | None ->
           Printf.printf "bestsearch %f\n   %s\n=? %s\n" l (tm_str_1 m) (tm_str_1 n); flush stdout
        | Some(b,pos) ->
           Printf.printf "bestsearch %f %b%s\n   %s\n=? %s\n" l b (pos_str pos) (tm_str_1 m) (tm_str_1 n); flush stdout
      end;
    match sd with
    | None -> (** root **)
       begin
         try
           let theta : tm option array = Array.make vbd None in
           unif_dpairs theta [(m,n)];
           let theta2 = ss vbd theta in
           let m = tm_subst theta2 m in
           let n = tm_subst theta2 n in
           ([(m,"",n)],theta2)
         with UnifFail -> (*** must do another step, so communicate with the advisor ***)
           let kl = send_advisor_pair m n in
           bestsearchtrans kl vbd m n l
       end
    | Some(b,pos) ->
       bestsearch1b vbd b pos m n l
  and bestsearchtrans kl vbd m n l =
    match kl with
    | [] -> bestsearch1a vbd m n l (*** no more suggested transitive splits (usual case) ***)
    | k::kr -> (*** advisor suggested splitting into m = k and k = n (where k should have no vars and m and n should not share vars) ***)
	try
	  let (ch1,theta1) = bestsearch(vbd,m,k,None,(l -. 1.0)) in
	  let (ch2,theta2) = bestsearch(vbd,k,n,None,(l -. 1.0)) in
	  let theta : tm option array = Array.make vbd None in
	  for i = 0 to vbd - 1 do
	    if theta1.(i) = Var(i) then
	      theta.(i) <- Some(theta2.(i))
	    else if theta2.(i) = Var(i) || theta2.(i) = theta1.(i) then
	      theta.(i) <- Some(theta1.(i))
	    else
	      raise Not_found
	  done;
	  (ch1 @ ch2,ss vbd theta)
	with Not_found -> (*** failed with split; fall back on usual case ***)
	  bestsearchtrans kr vbd m n l
  and bestsearch1a vbd m n l = (** at the root; advisor chooses whether to work on the lhs or rhs **)
    send_advisor_state (vbd,m,n,None);
    let dl = dist_if_move 1 in
    let dr = dist_if_move 2 in
    let lm1 = l -. 1.0 in
    let ll = ref [] in
    if dl <= l then ll := [(MoveDown(1),dl)];
    if dr <= l then
      ll := List.merge (fun (_,d1) (_,d2) -> compare d1 d2)
	      !ll
	      [(MoveDown(2),dr)];
    if !usebatchdist then
      begin
        ll := batchdist (List.map (fun (o,_) -> o) !ll)
      end;
    bestsearch2a !ll vbd m n lm1
  and bestsearch2a ll vbd m n l =
    match ll with
    | (MoveDown(i),_)::lr when i = 1 ->
       abstracttimecheck();
       begin
         try
           if !verbose > 8 then (Printf.printf "(Root) Trying MoveDown %d\n" i);
           bestsearch(vbd,m,n,Some(true,[]),l)
         with Not_found ->
               if !verbose > 8 then (Printf.printf "(Root) Backtracking MoveDown %d\n" i);
               bestsearch2a lr vbd m n l
       end
    | (MoveDown(i),_)::lr when i = 2 ->
       begin
         abstracttimecheck();
         try
           if !verbose > 8 then (Printf.printf "(Root) Trying MoveDown %d\n" i);
           bestsearch(vbd,m,n,Some(false,[]),l)
         with Not_found ->
               if !verbose > 8 then (Printf.printf "(Root) Backtracking MoveDown %d\n" i);
               bestsearch2a lr vbd m n l
       end
    | _ -> raise Not_found (** no other action is valid at the root **)
  and bestsearch1b vbd b pos m n l =
    send_advisor_state (vbd,m,n,Some(b,pos));
    if !verbose > 4 then (Printf.printf "bestsearch1b %f\nb: %b; pos:%s\n    %s\n=? %s\n" l b (pos_str pos) (tm_str_1 m) (tm_str_1 n); flush stdout);
    let ll = ref [] in
    let lm1 = l -. 1.0 in
    let (f,k) = get_pos (if b then m else n) pos in
    begin
      match k with
      | Prim(_,args) ->
         for i = 1 to List.length args do
           let di = dist_if_move i in
           if di <= l then
             ll := List.merge (fun (_,d1) (_,d2) -> compare d1 d2)
                     [(MoveDown(i),di)]
                     !ll
         done
      | Abbrev(_,args) ->
         for i = 1 to List.length args do
           let di = dist_if_move i in
           if di <= l then
             ll := List.merge (fun (_,d1) (_,d2) -> compare d1 d2)
                     [(MoveDown(i),di)]
                     !ll
         done
      | _ -> ()
    end;
    List.iter
      (fun (nm,lhs,rhs) ->
        let lhs = push_tm_frees vbd lhs in
        let rhs = push_tm_frees vbd rhs in
        let vbd2 = clause_var_upperbd [Eq(lhs,rhs)] in
        begin
          match onestep_at vbd2 k lhs rhs with
          | None -> ()
          | Some(z,theta) ->
             let m1 = if b then tm_subst theta (f z) else tm_subst theta m in
             let n1 = if b then tm_subst theta n else tm_subst theta (f z) in
             if not (Hashtbl.mem dn (m1,n1)) then
               let d = dist_if_rew true nm m1 n1 in
               if d <= l then
                 ll := List.merge (fun (_,d1) (_,d2) -> compare d1 d2)
                         [(RewWithEqn(nm,true,m1,n1,theta),d)]
                         !ll
        end;
        begin
          match onestep_at vbd2 k rhs lhs with
          | None -> ()
          | Some(z,theta) ->
             let m1 = if b then tm_subst theta (f z) else tm_subst theta m in
             let n1 = if b then tm_subst theta n else tm_subst theta (f z) in
             if not (Hashtbl.mem dn (m1,n1)) then
               let d = dist_if_rew false nm m1 n1 in
               if d <= l then
                 ll := List.merge (fun (_,d1) (_,d2) -> compare d1 d2)
                         [(RewWithEqn(nm,false,m1,n1,theta),d)]
                         !ll
        end)
      !usableeqns;
    if !verbose > 2 then (Printf.printf "%d options\n" (List.length !ll); flush stdout);
    if !usebatchdist then
      begin
        ll := batchdist (List.map (fun (o,_) -> o) !ll)
      end;
    bestsearch2b !ll vbd b pos m n lm1
  and bestsearch2b ll vbd b pos m n l =
    match ll with
    | [] -> raise Not_found
    | (MoveDown(i),_)::lr ->
       begin
         try
           abstracttimecheck();
           if !verbose > 8 then (Printf.printf "Trying MoveDown %d\n" i);
           bestsearch(vbd,m,n,Some(b,pos @ [i]),l)
         with Not_found ->
           if !verbose > 8 then (Printf.printf "Backtracking from MoveDown %d\n" i);
           bestsearch2b lr vbd b pos m n l
       end
    | (RewWithEqn(nm,dir,m1,n1,theta),_)::lr ->
       begin
         try
           abstracttimecheck();
           if !verbose > 8 then (Printf.printf "Trying RewWithEqn %s %b\n" nm dir);
           let vbd1 = max (tm_var_upperbd m1) (tm_var_upperbd n1) in
           let (ch,theta2) = bestsearch(vbd1,m1,n1,None,l) in
	   let theta3 : tm array = Array.make vbd (Skol("fake")) in
	   for i = 0 to vbd-1 do
	     theta3.(i) <- tm_subst theta2 (theta.(i))
	   done;
           if b then
	     ((List.map
		 (fun (m,u,n) ->
		   (tm_subst theta3 m,u,tm_subst theta3 n))
		 ((tm_subst theta m,nm,m1)::ch)),theta3)
           else
	     ((List.map
		 (fun (m,u,n) ->
		   (tm_subst theta3 m,u,tm_subst theta3 n))
		 (ch @ [n1,nm,tm_subst theta n])),theta3)
         with Not_found ->
           if !verbose > 8 then (Printf.printf "Backtracking from RewWithEqn %s %b\n" nm dir);
           bestsearch2b lr vbd b pos m n l
       end
  in
  bestsearch(0,lhs,rhs,None,!maxdepth)

let evaluate_pftacitem thmname thmhash n goalstack pfitem =
  match pfitem with
  | PfStruct(i) when i < 3 ->
      begin
	match goalstack with
	| PfStateSep(j,true)::goalstackr ->
	    if i = j then
	      proving := Some(thmname,thmhash,n,goalstackr)
	    else
	      raise (Failure("Previous subproof not completed"))
	| PfStateGoal(g1a,g1b)::PfStateGoal(g2a,g2b)::goalstackr ->
	    proving := Some(thmname,thmhash,n,PfStateGoal(g1a,g1b)::structure_pfstate i (PfStateGoal(g2a,g2b)) goalstackr)
	| _ ->
	    raise (Failure("Inappropriate place for this proof structuring symbol"))
      end
  | PfStruct 4 ->
      begin
	match goalstack with
	| PfStateSep _::_ ->
	    raise (Failure("A subproof cannot be started here"))
	| [] ->
	    raise (Failure("No claim to prove here"))
	| PfStateGoal(g1a,g1b)::goalstackr ->
	    proving := Some(thmname,thmhash,n,PfStateGoal(g1a,g1b)::PfStateSep(5,true)::goalstackr)
      end
  | PfStruct 5 ->
      begin
	match goalstack with
	| PfStateSep(j,_)::goalstackr ->
	    if j = 5 then
	      begin
		proving := Some(thmname,thmhash,n,goalstackr);
		if !latex then Printf.fprintf !latexc "\n\n";
	      end
	    else
	      raise (Failure("Previous subproof not completed"))
	| [] ->
	    raise (Failure("Not proving a goal"))
	| _ -> raise (Failure("Subproof not completed"))
      end
  | SetTac(x,None,a) ->
      begin
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps),nll)::goalstackr) ->
	    let m = interp_aim_tm_r (ltree_to_atree a) fakefv nctx in
	    if !latex then
	      begin
		Printf.fprintf !latexc " Let $%s$ be " (latexify_mathname x);
		ltree_to_latex_math_or_matharray_top true !latexc a;
	      end;
	    if !html then
	      begin
		Printf.printf "  Let <i>%s</i> be " x;
		ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		Printf.printf ".\n";
	      end;
	    proving := Some(thmname,thmhash,n,((PfStateGoal(((x,m)::nctx,hyps),nll))::goalstackr))
	| [] ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end
  | CasesTac(a,thn,obyl) ->
      begin
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps),nll)::goalstackr) ->
	    let cl = interp_aim_clause_r (ltree_to_atree a) fakefv nctx in (*** no free vars ***)
	    if List.length cl < 2 then raise (Failure "cases only makes sense when given a clause with at least two literals");
	    let nll2 = negate_clause cl in
	    ensureobvious (nctx,hyps) nll2 thn obyl;
	    let gs = ref goalstackr in
	    List.iter (fun l -> gs := PfStateGoal((nctx,hyps),l::nll)::!gs) (List.rev cl);
	    if !latex then
	      begin
		if thn then
		  Printf.fprintf !latexc " Then "
		else
		  Printf.fprintf !latexc " We have ";
		ltree_to_latex_math_or_matharray_top false !latexc a;
		match obyl with
		| None ->
		    Printf.fprintf !latexc " and so we argue by cases.\n";
		| Some(byl) ->
		    printf_latex_by byl;
		    Printf.fprintf !latexc " and so we argue by cases.\n";
	      end;
	    if !html then
	      begin
		if thn then
		  Printf.printf " Then "
		else
		  Printf.printf " We have ";
		ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		match obyl with
		| None -> Printf.printf " and so we argue by cases.\n";
		| Some(byl) ->
		    printf_html_by stdout byl;
		    Printf.printf " and so we argue by cases.\n";
	      end;
	    proving := Some(thmname,thmhash,n,!gs);
	| [] ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end
  | ClaimTac(name,a) ->
      begin
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps),nll)::goalstackr) ->
	    let cl = interp_aim_clause_r (ltree_to_atree a) fakefv nctx in (*** no free vars ***)
	    if !latex then
	      begin
		Printf.fprintf !latexc "\n\n{\bf{Claim:}} ";
		Printf.fprintf !latexc "\n\\begin{eqnarray}\n";
		ltree_to_latex_matharray 0 !latexc a;
		Printf.fprintf !latexc "\\label{%s__%d}\n\\end{eqnarray}\n" name !local_latex_ref_counter;
		Hashtbl.add local_latex_ref name (Printf.sprintf "(\\ref{%s__%d})" name !local_latex_ref_counter);
		incr local_latex_ref_counter
	      end;
	    if !html then
	      begin
		Printf.printf "<div class='labeled'><b>Claim </b><a name=\"%s%d\">%s: " name !local_html_ref_counter name;
		ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		Printf.printf "</div>\n";
		Hashtbl.add local_html_ref name (Printf.sprintf "<a href=\"%s%d\">Claim %s</a>" name !local_html_ref_counter name);
		incr local_html_ref_counter
	      end;
	    if !sexpr then Hashtbl.replace hyprefs (Some(name)) false;
	    proving := Some(thmname,thmhash,n,(PfStateGoal((nctx,hyps),negate_clause cl)::PfStateGoal((nctx,((Some(name),cl)::hyps)),nll)::goalstackr));
	| [] ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end
  | ProveTac(a) ->
      begin
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps),nll)::goalstackr) ->
	    let cl = interp_aim_clause_r (ltree_to_atree a) fakefv nctx in (*** no free vars ***)
	    ensureobvious (nctx,(None,cl)::hyps) nll true None;
	    let nll2 = negate_clause cl in
	    proving := Some(thmname,thmhash,n,PfStateGoal((nctx,hyps),nll2)::goalstackr);
	    if !latex then
	      begin
		Printf.fprintf !latexc " We will prove ";
		ltree_to_latex_math_or_matharray_top true !latexc a;
	      end;
	    if !html then
	      begin
		Printf.printf "  We will prove ";
		ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		Printf.printf ".\n";
	      end;
	| _ ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end
  | AssumeTac(label,a) ->
      begin
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps),nll)::goalstackr) ->
	    let l = interp_aim_lit_r (ltree_to_atree a) fakefv nctx in
	    let fnd = ref false in
	    let nll2 = ref [] in
	    List.iter (fun l2 -> if l2 = l then fnd := true else nll2 := l2::!nll2) nll;
	    if not !fnd then
	      begin
		Printf.printf "cannot make assumption at line %d char %d\n" !lineno !charno;
		raise (Failure "bad assume")
	      end;
	    if !latex then
	      begin
		Printf.fprintf !latexc " Assume ";
		match label with
		| None ->
		    ltree_to_latex_math_or_matharray_top true !latexc a;		    
		| Some(name) ->
		    Printf.fprintf !latexc "\n\\begin{eqnarray}\n";
		    ltree_to_latex_matharray 0 !latexc a;
		    Printf.fprintf !latexc ".\\label{%s__%d}\n\\end{eqnarray}\n" name !local_latex_ref_counter;
		    Hashtbl.add local_latex_ref name (Printf.sprintf "(\\ref{%s__%d})" name !local_latex_ref_counter);
		    incr local_latex_ref_counter
	      end;
	    if !html then
	      begin
		Printf.printf "  Assume ";
		match label with
		| None ->
		    ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		    Printf.printf ".\n";
		| Some(name) ->
		    Printf.printf "<div class='labeled'><a name='%s%d'>(%s) " name !local_html_ref_counter name;
		    ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		    Printf.printf ".</div>\n";
		    Hashtbl.add local_html_ref name (Printf.sprintf "<a href='#%s%d'>(%s)</a>" name !local_html_ref_counter name);
		    incr local_html_ref_counter
	      end;
	    if !sexpr then Hashtbl.replace hyprefs label false;
	    proving := Some(thmname,thmhash,n,(PfStateGoal((nctx,(label,[l])::hyps),!nll2)::goalstackr))
	| _ ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end
  | HaveTac(label,a,thn,obyl) ->
      begin
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps) as pctx,nll)::goalstackr) ->
	    let l1 = interp_aim_lit_r (ltree_to_atree a) fakefv nctx in
	    ensureobvious pctx [lit_negate l1] thn obyl;
	    if !fakelearn then
	      begin
		match l1 with
		| Eq(l,r) -> output_fakedists l r
		| _ -> ()
	      end;
	    if !latex then
	      begin
		if thn then
		  Printf.fprintf !latexc " Then "
		else
		  Printf.fprintf !latexc " We have ";
		begin
		  match label with
		  | None ->
		      begin
			match obyl with
			| None -> ltree_to_latex_math_or_matharray_top true !latexc a;
			| Some(_) -> ltree_to_latex_math_or_matharray_top false !latexc a;
		      end;
		  | Some(name) ->
		      Printf.fprintf !latexc "\n\\begin{equation}\n";
		      if ltree_has_a_linebreak_p a then
			begin
			  Printf.fprintf !latexc "\\begin{array}{c}\n";
			  ltree_to_latex_matharray_top
			    (match obyl with None -> true | _ -> false)
			    0 !latexc a;
			  Printf.fprintf !latexc "\\end{array}\n";
			end
		      else
			begin
			  ltree_to_latex_math !latexc a;
			  match obyl with
			  | None -> Printf.fprintf !latexc ".";
			  | Some(_) -> ()
			end;
		      Printf.fprintf !latexc "\\label{%s__%d}\n\\end{equation}\n" name !local_latex_ref_counter;
		      Hashtbl.add local_latex_ref name (Printf.sprintf "(\\ref{%s__%d})" name !local_latex_ref_counter);
		      incr local_latex_ref_counter
		end;
		match obyl with
		| None -> ()
		| Some(byl) ->
		    printf_latex_by byl;
		    Printf.fprintf !latexc ".\n";
	      end;
	    if !html then
	      begin
		if thn then
		  Printf.printf " Then "
		else
		  Printf.printf " We have ";
		begin
		  match label with
		  | None ->
		      ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		  | Some(name) ->
		      Printf.printf "<div class='labeled'><a name=\"%s%d\">(%s) " name !local_html_ref_counter name;
		      ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		      Printf.printf "</div>\n";
		      Hashtbl.add local_html_ref name (Printf.sprintf "<a href=\"%s%d\">(%s)</a>" name !local_html_ref_counter name);
		      incr local_html_ref_counter
		end;
		match obyl with
		| None -> Printf.printf ".\n";
		| Some(byl) ->
		    printf_html_by stdout byl;
		    Printf.printf ".\n";
	      end;
	    if !sexpr then Hashtbl.replace hyprefs label false;
	    proving := Some(thmname,thmhash,n,(PfStateGoal((nctx,(label,[l1])::hyps),nll)::goalstackr))
	| [] ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end	    
  | ThusTac(label,a,thn,obyl) ->
      begin
	if !verbose > 19 then Printf.printf "DOING thus with goalstack of length %d\n" (List.length goalstack);
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps) as pctx,nll)::goalstackr) ->
	    if a = NaL("thesis") then
	      begin
		if !verbose > 19 then
		  begin
		    Printf.printf "here is nll:\n";
		    List.iter
		      (fun l ->
			Printf.printf ". %s\n" (lit_str l))
		      nll;
		  end;
		ensureobvious (nctx,hyps) nll thn obyl;
		if !fakelearn then
		  begin
		    match nll with
		    | [NEq(lhs,rhs)] -> output_fakedists lhs rhs
		    | _ -> ()
		  end;
		if !latex then
		  begin
		    if thn then
		      Printf.fprintf !latexc " Hence we are done"
		    else
		      Printf.fprintf !latexc " Thus we are done";
		    match obyl with
		    | None -> Printf.fprintf !latexc ".\n";
		    | Some(byl) ->
			printf_latex_by byl;
			Printf.fprintf !latexc ".\n";
		  end;
		if !html then
		  begin
		    if thn then
		      Printf.printf " Hence we are done"
		    else
		      Printf.printf " Thus we are done";
		    match obyl with
		    | None -> Printf.printf ".\n";
		    | Some(byl) ->
			printf_html_by stdout byl;
			Printf.printf ".\n";
		  end;
		proving := Some(thmname,thmhash,n,goalstackr)
	      end
	    else
	      let l1 = interp_aim_lit_r (ltree_to_atree a) fakefv nctx in
	      ensureobvious pctx [lit_negate l1] thn obyl;
	      if !sexpr then Hashtbl.replace hyprefs label false;
	      ensureobvious (nctx,(label,[l1])::hyps) nll true None;
	      if !latex then
		begin
		  if thn then
		    Printf.fprintf !latexc " Hence "
		  else
		    Printf.fprintf !latexc " Thus ";
		  begin
		    match label with
		    | None ->
			begin
			  match obyl with
			  | None -> ltree_to_latex_math_or_matharray_top true !latexc a;
			  | Some(_) -> ltree_to_latex_math_or_matharray_top false !latexc a;
			end;
		    | Some(name) ->
			Printf.fprintf !latexc "\n\\begin{equation}\n";
			if ltree_has_a_linebreak_p a then
			  begin
			    Printf.fprintf !latexc "\\begin{array}{c}\n";
			    ltree_to_latex_matharray_top
			      (match obyl with None -> true | _ -> false)
			      0 !latexc a;
			    Printf.fprintf !latexc "\\end{array}\n";
			  end
			else
			  begin
			    ltree_to_latex_math !latexc a;
			    match obyl with
			    | None -> Printf.fprintf !latexc ".";
			    | Some(_) -> ()
			  end;
			Printf.fprintf !latexc "\\label{%s__%d}\n\\end{equation}\n" name !local_latex_ref_counter;
			Hashtbl.add local_latex_ref name (Printf.sprintf "(\\ref{%s__%d})" name !local_latex_ref_counter);
			incr local_latex_ref_counter
		  end;
		  match obyl with
		  | None -> ()
		  | Some(byl) ->
		      printf_latex_by byl;
		      Printf.fprintf !latexc ".\n";
		end;
	      if !html then
		begin
		  if thn then
		    Printf.printf "  Hence "
		  else
		    Printf.printf "  Thus ";
		  begin
		    match label with
		    | None ->
			ltree_to_html_math stdout (html_outer_ref fakefvh) a;
		    | Some(name) ->
			Printf.printf "<div class='labeled'><a name='%s%d'>(%s) " name !local_latex_ref_counter name;
			ltree_to_html_math stdout (html_outer_ref fakefvh) a;
			Printf.printf "</div>\n";
			Hashtbl.add local_html_ref name (Printf.sprintf "<a href='#%s%d'>(%s)</a>" name !local_html_ref_counter name);
			incr local_html_ref_counter
		  end;
		  match obyl with
		  | None -> Printf.printf ".\n";
		  | Some(byl) ->
		      printf_html_by stdout byl;
		      Printf.printf ".\n";
		end;
	      proving := Some(thmname,thmhash,n,goalstackr)
	| [] ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end	    
  | EqChainTac(label,a,thn,obyl) ->
      begin
	if not (label = None) then raise (Failure "Please do not label eqn chains yet. Thanks!");
	match reduce_goalstack goalstack with
	| (PfStateSep(_)::_) -> raise (Failure "The current subproof needs to be closed before continuing the proof.")
	| (PfStateGoal((nctx,hyps) as pctx,nll)::goalstackr) ->
	    let eq1 = interp_aim_lit_r (ltree_to_atree a) fakefv nctx in
	    begin
	      match eq1 with
	      | Eq(l,r) ->
		  proving := None;
		  ensureobvious pctx [NEq(l,r)] thn obyl;
		  eqproving := Some(thmname,thmhash,n,pctx,label,l,r,nll,goalstackr);
		  if !latex then
		    begin
		      if thn then Printf.fprintf !latexc " Then ";
		      Printf.fprintf !latexc "\n\\begin{eqnarray*}\n";
		      ltree_to_latex_matharray_septopeqn !latexc a;
		      match obyl with
		      | None -> Printf.fprintf !latexc "\\\\\n";
		      | Some(byl) ->
			  Printf.fprintf !latexc "& {\\mbox{ by }}";
			  let fst = ref true in
			  List.iter (fun b -> if !fst then fst := false else Printf.fprintf !latexc ", "; Printf.fprintf !latexc "%s" (latex_ref b)) byl
		    end;
		  if !html then
		    begin
		      if thn then Printf.printf "  Then<br/><table><tr>" else Printf.printf "<br/><table><tr>";
		      ltree_to_html_math_septopeqn stdout (html_outer_ref fakefvh) a;
		      match obyl with
		      | None -> ()
		      | Some(byl) ->
			  Printf.printf "<td> by ";
			  let fst = ref true in
			  List.iter (fun b -> if !fst then fst := false else Printf.printf ", "; Printf.printf "%s" (html_ref b)) byl;
			  Printf.printf "</td>";
		    end
	      | _ ->
		  Printf.printf "non eqn at line %d char %d\n" !lineno !charno;
		  raise (Failure("non eqn"))
	    end
	| [] ->
	    Printf.printf "no goals on stack at line %d char %d\n" !lineno !charno;
	    raise (Failure "no goals were on the stack")
      end
  | RewriteTac(forw,eqname,pos) ->
      begin
	if not !rewritefailed then
	  match !pending with
	  | None -> ()
	  | Some(nm,lhs,rhs,vbd1) ->
	      try
		incr rewritecount;
		if !countoptions then
		  begin
		    Printf.printf "OPTIONS %s %d %d\n" nm !rewritecount (List.length (onestep vbd1 lhs) + List.length (onestep vbd1 rhs));
		  end;
		let (_,ulhs,urhs) = List.find (fun (x,_,_) -> x = eqname) !usableeqns in
		let (fromside,toside) = if forw then (push_tm_frees vbd1 ulhs,push_tm_frees vbd1 urhs) else (push_tm_frees vbd1 urhs,push_tm_frees vbd1 ulhs) in
		let vbd = clause_var_upperbd [Eq(lhs,rhs);Eq(fromside,toside)] in
		match pos with
		| (1::posr) ->
		    begin
		      try
			let (f,m) = get_pos lhs posr in
			if !verbose > 5 then (Printf.printf "rewrite %s 1 vbd %d\nlhs: %s\nm: %s\nfromside: %s\ntoside: %s\n" eqname vbd (tm_str lhs) (tm_str m) (tm_str fromside) (tm_str toside); flush stdout);
			let theta : tm option array = Array.make vbd None in
			unif_dpairs theta [(m,fromside)];
			let theta2 = ss vbd theta in
			let lhs2 = tm_subst theta2 (f toside) in
			begin
			  match norm_clause_frees [Eq(lhs2,rhs)] with
			  | ([Eq(lhs3,rhs3)],vbd3,_) ->
			      Printf.printf "INTERM %d %d @ %s =? # %s\n" !rewritecount vbd3 (tm_ivy lhs3) (tm_ivy rhs3);
			      pending := Some(nm,lhs3,rhs3,vbd3)
			  | _ -> raise (Failure("something wrong"))
			end
		      with _ -> rewritefailed := true;
		    end
		| (2::posr) ->
		    begin
		      try
			let (f,m) = get_pos rhs posr in
			let theta : tm option array = Array.make vbd None in
			unif_dpairs theta [(m,fromside)];
			let theta2 = ss vbd theta in
			let rhs2 = tm_subst theta2 (f toside) in
			begin
			  match norm_clause_frees [Eq(lhs,rhs2)] with
			  | ([Eq(lhs3,rhs3)],vbd3,_) ->
			      Printf.printf "INTERM %d %d @ %s =? # %s\n" !rewritecount vbd3 (tm_ivy lhs3) (tm_ivy rhs3);
			      pending := Some(nm,lhs3,rhs3,vbd3)
			  | _ -> raise (Failure("something wrong"))
			end
		      with _ -> rewritefailed := true;
		    end
		| _ ->
		    raise (Failure "first number in position should be 1 if rewriting on the lhs and 2 if rewriting on the rhs")
	      with Not_found ->
		raise (Failure (eqname ^ " not a usable eqn"))
      end
  | Admitted ->
      begin
	admits := thmname::!admits;
	Hashtbl.add knownclauses thmname thmhash;
	begin
	  match !pending with
	  | None -> ()
	  | Some(nm,lhs,rhs,_) ->
	      try
		let (ch,_) = search nm lhs rhs in
		Printf.printf "RESULT: Successfully Proved %s with abstract time %d real time %f\n" nm !abstracttimecount (Unix.gettimeofday() -. !starttime);
		Printf.printf "Pf of %s\n" nm;
		begin
		  match ch with
		  | [] -> Printf.printf "empty chain?";
		  | (m,u,n)::ch ->
		      Printf.printf "%s = %s by %s\n" (tm_str_1 m) (tm_str_1 n) u;
		      List.iter
			(fun (_,u,n) ->
			  Printf.printf " = %s by %s\n" (tm_str_1 n) u)
			ch
		end;
		flush stdout;
		pending := None
	      with
	      | Not_found ->
		  Printf.printf "RESULT: Search for %s failed. Abs time: %d, Real time: %f\n" nm !abstracttimecount (Unix.gettimeofday() -. !starttime);
		  pending := None
	      | AbstractTimeLimit ->
		  Printf.printf "RESULT: Search for %s failed due to abstract time limit. Abs time: %d, Real time: %f\n" nm !abstracttimecount (Unix.gettimeofday() -. !starttime);
		  pending := None
	end;
	if !latex then Printf.fprintf !latexc "{\\it Incomplete Proof}\\end{proof}\n";
	if !html then Printf.printf "<i>Incomplete Proof</i></div>\n";
	Hashtbl.clear local_latex_ref;
	(*** Allow anything as known in aimleap ***)
	(*** 
	known_lookup thmhash
	  (fun bib -> if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs)
	  (fun () ->
	    if !report then Printf.printf "Admitted As Known %s : %s\n" thmname thmhash;
	    allknowns_merkl := merkle_insert !allknowns_merkl thmhash !currentfiletitle);
	  ***)
	proving := None
      end
  | Qed ->
      begin
	begin
	  match !pending with
	  | None -> ()
	  | Some(nm,lhs,rhs,vbd) ->
	      if !fakelearn then
		begin
		  Printf.printf "FAKEDEPTH\n%s\n%d\n" nm !fakedepth;
		  Hashtbl.add fakedepthh nm !fakedepth
		end;
	      try
		if !rewritefailed then raise UnifFail;
		let theta : tm option array = Array.make vbd None in
		unif_dpairs theta [(lhs,rhs)];
		let theta2 = ss vbd theta in
		let lhs2 = tm_subst theta2 lhs in
		let rhs2 = tm_subst theta2 rhs in
		if lhs2 = rhs2 then
		  Printf.printf "SUCCESS %s\n" nm
		else
		  Printf.printf "FAILURE %s\n" nm;
		pending := None
	      with UnifFail ->
		Printf.printf "FAILURE %s\n" nm;
		pending := None
	end;
(**
	if !verbose > 19 then Printf.printf "DOING QED with goalstack of length %d\n" (List.length goalstack);
	List.iter
	  (fun s ->
	    match s with
	    | PfStateSep(_) -> raise (Failure "Qed before all subproofs have been closed")
	    | PfStateGoal(pctx,nll) ->
		try ensureobvious pctx nll true None with Failure _ -> ensureobvious pctx nll false None)
	  (reduce_goalstack goalstack);
**)
	Hashtbl.add knownclauses thmname thmhash;
	if !latex then Printf.fprintf !latexc "\\end{proof}\n";
	if !html then Printf.printf "</div>\n";
	Hashtbl.clear local_latex_ref;
(*** Allow anything as known in aimleap ***)
(***
	known_lookup thmhash
	  (fun bib -> if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs)
	  (fun () ->
	    if !report then Printf.printf "New Known %s : %s\n" thmname thmhash;
	    allknowns_merkl := merkle_insert !allknowns_merkl thmhash !currentfiletitle);
***)
	if !sexpr then (Hashtbl.iter (fun k v -> match k with None -> () | Some(k) -> if not v then Printf.printf "(UNUSEDHYPREF \"%s\" \"%s\")\n" thmname k) hyprefs; Hashtbl.clear hyprefs);
	proving := None
      end
  | _ ->
      Printf.printf "unhandled pf case line %d char %d\n" !lineno !charno;
      raise (Failure "unhandled pf case")
  
let evaluate_article_docitem ditem =
  match ditem with
  | Author(authnyml) ->
      if !articleauthors = None then
	begin
	  articleauthors := Some(authnyml);
	  List.iter
	    (fun a ->
	      let (_,_) = pubkey_from_aimnym a in
	      if Hashtbl.mem authornymh a then
		begin
		  let (x,y,z) = a in
		  Printf.printf "ERROR: Duplicate author %s %s %s\n" x y z;
		  exit 1
		end;
(**	      articleauthorpubkeys := ((x,y),a)::!articleauthorpubkeys; **)
	      Hashtbl.add authornymh a ())
	    authnyml;
	  if !latex then
	    begin
	      let fst = ref true in
	      Printf.fprintf !latexc "\\author{";
	      List.iter (fun (a1,a2,a3) ->
		if !fst then fst := false else Printf.fprintf !latexc " \\and ";
		Printf.fprintf !latexc "%s %s. %s" a1 (String.sub a2 0 1) a3)
		authnyml;
	      Printf.fprintf !latexc "}\n\\maketitle\n";
	    end;
	  if !html then
	    begin
	      let fst = ref true in
	      Printf.printf "<div class='authors'>";
	      List.iter (fun (a1,a2,a3) ->
		if !fst then fst := false else Printf.printf ", ";
		Printf.printf "<span class='authornym'>%s %s <a href='?n=%s>%s</a></a>" a1 a2 a3 a3)
		authnyml;
	      Printf.printf "</div><hr/>\n";
	    end;
	end
      else
	raise (Failure "authors can only be declared once per article")
  | ClaimItem(creditnyml,label,b,None) ->
      incr itemcount;
      addothercreditnyml creditnyml;
      let cl = interp_aim_clause (ltree_to_atree b) [] in
      let clr = clause_root cl in
      if !sexpr then Printf.printf "(CLAIM %s \"%s\")\n" (ostring_sexpr label) clr
  | ClaimItem(creditnyml,label,b,Some(pl)) ->
      incr itemcount;
      addothercreditnyml creditnyml;
      let cl = interp_aim_clause (ltree_to_atree b) [] in
      let clr = clause_root cl in
      if !sexpr then
	begin
	  Printf.printf "(CLAIMBY %s \"%s\"\n" (ostring_sexpr label) clr;
	  List.iter (fun p -> let pr = clause_root (interp_aim_clause (ltree_to_atree p) []) in Printf.printf "\"%s\"\n" pr) pl;
	  Printf.printf ")\n"
	end
  | ConjDecl(creditnyml,conjname,b) ->
      incr itemcount;
      addothercreditnyml creditnyml;
      let fvh = Hashtbl.create 10 in
      let fvc = ref 0 in
      let fv = (fvh,fvc) in
      let cl = interp_aim_clause_r (ltree_to_atree b) fv [] in
      let clr = clause_root cl in
      if !check_conjectures_for_subsumption then
	begin
	  let nm x y = match x with Some(x) -> x | None -> y in
	  List.iter
	    (fun (conjname2,clr2,cl2) ->
	      if clause_subsumes_p cl2 cl then Printf.printf "%s subsumes %s\n" (nm conjname2 clr2) (nm conjname clr);
	      if clause_subsumes_p cl cl2 then Printf.printf "%s subsumes %s\n" (nm conjname clr) (nm conjname2 clr2))
	    !conjl;
	  conjl := (conjname,clr,cl)::!conjl
	end;
      conj_lookup clr
	(fun clrcr ->
	  if !sexpr then Printf.printf "(REPEATCONJ \"%s\")\n" clr)
(*	  raise (Failure("Conjecture " ^ clr ^ " is not new -- was already conjectured in " ^ clrcr))) *)
	(fun () ->
	  known_lookup clr
	    (fun clrtr -> raise (Failure("Conjecture " ^ clr ^ " has been a theorem since " ^ clrtr)))
	    (fun () ->
	      nonthm_lookup clr
		(fun clrnr -> raise (Failure("Conjecture " ^ clr ^ " has been a nontheorem since " ^ clrnr)))
		(fun () ->
		  allconjs_merkl := merkle_insert !allconjs_merkl clr !currentfiletitle;
		  if !report then Printf.printf "New Conjecture %s\n" clr)));
      if !sexpr then
	begin
	  let n = clause_var_upperbd cl in
	  Printf.printf "(CONJ %s \"%s\" %d)\n" (ostring_sexpr conjname) clr n;
	  begin
	    match cl with
	    | [Eq(m1,m2)] ->
		let theta = Array.make n (Skol("fake")) in
		let (clsym,_) = okify_clause [Eq(m2,m1)] theta 0 in
		Printf.printf "(CONJSYM %s \"%s\" %d)\n" (ostring_sexpr conjname) (clause_root clsym) n;
	    | _ -> ()
	  end;
	  if n <= 5 && !evaluateconj then
	    begin
	      let dl = delta_normalize_clause cl (abbrevarity,abbrevdef) in
	      Hashtbl.iter
		(fun name l ->
		  if clause_valid_p l dl then
		    Printf.printf "(EVAL \"%s\" \"%s\" T)\n" name clr
		  else
		    Printf.printf "(EVAL \"%s\" \"%s\" NIL)\n" name clr)
		global_loops;
	    end;
	end;
      if !ivy then Printf.printf "(IVYCONJ %s %s)\n" (ostring_sexpr conjname) (clause_ivy cl);
      if !latex then
	begin
	  Printf.fprintf !latexc "\n\\begin{conjecture}%s\n"
	    (if List.mem clr main7conjectures then "{\\textup{(}cf.~\\cite{KinyonVV13}\\textup{)}}" else "");
	  ltree_to_latex_math_or_matharray_top true !latexc b;
	  Printf.fprintf !latexc "\n\\end{conjecture}\n";
	end;
      if !html then
	begin
	  Printf.printf "\n<div class='conj'>%s<b><a href='ref.php?h=%s'>Conjecture %d</a>%s:</b> "
	    (root_aname clr) clr !htmlthmcount
	    (if List.mem clr main7conjectures then " <a href='ref.php?b=KinyonVV13>[KinyonVV13]</a>" else "");
	  ltree_to_html_math stdout (html_outer_ref fvh) b;
	  Printf.printf "</div>\n";
	  incr htmlthmcount;
	end;
  | ThmDecl(thmkind,creditnyml,thmname,b) ->
     if !reverseusable then (usableeqns := List.rev !usableeqns; reverseusable := false); (** at the first sight of a thm, reverse usable; this is a simple test of the degree to which the order of the knowns matters **)
      if !fakelearn then fakedepth := 0;
      incr itemcount;
      addothercreditnyml creditnyml;
      let fvh = Hashtbl.create 10 in
      let fvc = ref 0 in
      let fv = (fvh,fvc) in
      let cl = interp_aim_clause_r (ltree_to_atree b) fv [] in
      let clr = clause_root cl in
      if List.mem clr axioms then raise (Failure(thmname ^ " is one of the AIM Axioms and should not be proven"));
      conj_lookup clr
	(fun clrcr ->
	  if clrcr = !currentfileh58 then
	    raise Not_found
	  else
	    known_lookup clr
	      (fun clrtr ->
		if clrtr = !currentfileh58 then
		  raise Not_found
		else
		  begin
		    articlerefs := clrcr::clrtr::!articlerefs;
		    if !latex then
		      Printf.fprintf !latexc "\nThe following was first conjectured in~\\cite{AIM:%s} and first proven in~\\cite{AIM:%s}.\n" clrcr clrtr
		  end)
	      (fun () ->
		articlerefs := clrcr::!articlerefs;
		if !latex then
		  Printf.fprintf !latexc "\nThe following was first conjectured in~\\cite{AIM:%s}.\n" clrcr))
	(fun () ->
	  known_lookup clr
	    (fun clrtr ->
	      if clrtr = !currentfileh58 then
		raise Not_found
	      else
		begin
		  if not (List.mem clrtr !articlerefs) then articlerefs := clrtr::!articlerefs;
		  if !latex then
		    Printf.fprintf !latexc "\nThe following was first proven in~\\cite{AIM:%s}.\n" clrtr
		end)
	    (fun () ->
	      if !summary then
		begin
		  Printf.printf "\n\\begin{proposition}\n";
		  ltree_to_latex_math_or_matharray_top true stdout b;
		  Printf.printf "\n\\end{proposition}\n";
		end;
	      ()));
      if !sexpr then
	begin
	  Printf.printf "(THM \"%s\" \"%s\" \"%s\")\n" thmkind thmname clr;
	  begin
	    match cl with
	    | [Eq(l,r)] ->
		let n = clause_var_upperbd cl in
		let theta = Array.make n (Skol("fake")) in
		let (clsym,_) = okify_clause [Eq(r,l)] theta 0 in
		Printf.printf "(SYMTHM \"%s\" \"%s\" \"%s\")\n" thmkind thmname (clause_root clsym);
		begin
		  try
		    let ld = dual_tm l dualprim dualabbrev in
		    let rd = dual_tm r dualprim dualabbrev in
		    let theta = Array.make n (Skol("fake")) in
		    let (cldual,_) = okify_clause [Eq(ld,rd)] theta 0 in
		    Printf.printf "(DUALTHM \"%s\" \"%s\" \"%s\")\n" thmkind thmname (clause_root cldual);
		    let theta = Array.make n (Skol("fake")) in
		    let (clsymdual,_) = okify_clause [Eq(rd,ld)] theta 0 in
		    Printf.printf "(SYMDUALTHM \"%s\" \"%s\" \"%s\")\n" thmkind thmname (clause_root clsymdual);
		  with Not_found -> ()
		end
	    | _ -> ()
	  end
	end;
      if !ivy then Printf.printf "(IVYTHM \"%s\" %s)\n" thmname (clause_ivy (let (cl,_) = okify_clause cl (Array.make (clause_var_upperbd cl) (Skol("fake"))) 0 in cl));
      if !latex then
	begin
	  if thmkind = "Lemma" then
	    begin
	      Hashtbl.add global_latex_ref thmname (Printf.sprintf "{\\mbox{Lemma}}~\\ref{%s}" thmname);
	      Printf.fprintf !latexc "\n\\begin{lemma}\\label{%s}\n" thmname;
	      ltree_to_latex_math_or_matharray_top true !latexc b;
	      Printf.fprintf !latexc "\n\\end{lemma}\n";
	      Printf.fprintf !latexc "\\begin{proof}\n";
	    end
	  else if thmkind = "Corollary" then
	    begin
	      Hashtbl.add global_latex_ref thmname (Printf.sprintf "{\\mbox{Corollary}}~\\ref{%s}" thmname);
	      Printf.fprintf !latexc "\n\\begin{corollary}\\label{%s}\n" thmname;
	      ltree_to_latex_math_or_matharray_top true !latexc b;
	      Printf.fprintf !latexc "\n\\end{corollary}\n";
	      Printf.fprintf !latexc "\\begin{proof}\n";
	    end
	  else
	    begin
	      Hashtbl.add global_latex_ref thmname (Printf.sprintf "{\\mbox{Theorem}}~\\ref{%s}" thmname);
	      Printf.fprintf !latexc "\n\\begin{theorem}\\label{%s}\n" thmname;
	      ltree_to_latex_math_or_matharray_top true !latexc b;
	      Printf.fprintf !latexc "\n\\end{theorem}\n";
	      Printf.fprintf !latexc "\\begin{proof}\n";
	    end
	end;
      if !html then
	begin
	  Hashtbl.add global_html_ref thmname (Printf.sprintf "%s <a href='#thm:%d'>%d</a>" thmkind !htmlthmcount !htmlthmcount);
	  Printf.printf "\n<div class='thm'><a name='thm:%d'/><b><a href='ref.php?h=%s'>%s %d:</a></b> " !htmlthmcount clr thmkind !htmlthmcount;
	  ltree_to_html_math stdout (html_outer_ref fvh) b;
	  Printf.printf "</div>\n<div class='proof'><b>Proof.</b>";
	  incr htmlthmcount;
	end;
      let nll = negate_clause cl in
      begin
	match nll with
	| [NEq(lhs,rhs)] -> pending := Some(thmname,lhs,rhs,0); rewritecount := 0; rewritefailed := false; abstracttimecount := 0; starttime := Unix.gettimeofday()
	| _ -> ()
      end;
      let vctx = ref [] in
      Hashtbl.clear skol_name;
      Hashtbl.iter
	(fun k v ->
	  let x = sha256reg (Printf.sprintf "SkolemFor:%s:%d" clr v) in
	  Hashtbl.add skol_name x k;
	  vctx := (k,Skol(x))::!vctx)
	fvh;
      proving := Some(thmname,clr,0,[PfStateGoal((!vctx,[]),nll)])
  | ParamDecl(parname,arity,r) ->
      if Hashtbl.mem abbrevroot parname then raise (Failure("redeclaration of " ^ parname));
      Hashtbl.add abbrevroot parname r;
      Hashtbl.add abbrevarity r arity;
      let k = sha256 (string_of_int arity ^ ":" ^ r) in
      let bibr = ref None in
      abbrev_lookup k
	(fun bib -> bibr := Some(bib); if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs)
	(fun () ->
	  raise (Failure("root given for " ^ parname ^ " does not correspond to a known abbreviation of arity " ^ string_of_int arity)));
      if !sexpr then Printf.printf "(PARAM \"%s\" %d \"%s\")\n" parname arity r;
      if !latex then
	begin
	  Printf.fprintf !latexc "\n\\begin{definition}%s %s is a function of arity %d whose definition we omit as it is not needed.\n"
	    (if List.mem (arity,r) main5abbrevs then
	      (match !bibr with None -> "{\\textup{(}cf.~\\cite{KinyonVV13}\\textup{)}}" | Some(bib) -> Printf.sprintf "{\\textup{(}cf.~\\cite{KinyonVV13,AIM:%s}\\textup{)}}" bib)
	    else
	      (match !bibr with None -> "" | Some(bib) -> Printf.sprintf "{\\textup{(}cf.~\\cite{AIM:%s}\\textup{)}}" bib))
	    parname arity;
	  Printf.fprintf !latexc "\\end{definition}\n";
	end;
      if !html then
	begin
	  Printf.printf "\n<div class='def'><a name='%s'/><b><a href='ref.php?h=%s'>Definition:</a></b> " parname r;
	  Printf.printf " %s is a function of arity %d whose definition we omit as it is not needed." parname arity;
	  Printf.printf "</div>\n";
	end;
  | DefDecl(creditnyml,defname,yl,b) ->
      incr itemcount;
      addothercreditnyml creditnyml;
      let fvh = Hashtbl.create 10 in
      let fvc = ref 0 in
      let fv = (fvh,fvc) in
      List.iter
	(fun y ->
	  if Hashtbl.mem fvh y then
	    raise (Failure("duplicate arg var " ^ y ^ " in defn of " ^ defname))
	  else
	    (Hashtbl.add fvh y !fvc; incr fvc))
	yl;
      let prevfvc = !fvc in
      let m = interp_aim_tm_r (ltree_to_atree b) fv [] in
      if !fvc > prevfvc then
	raise (Failure("seem to be extra vars"))
      else
	let mr = tm_root m in
	let ar = sha256 (string_of_int (List.length yl) ^ ":" ^ mr) in
	if !verbose > 10 then (Printf.printf "def %s %s\n" defname mr; flush stdout);
	Hashtbl.add abbr_name mr defname;
	let sp = ref [] in
	for i = !fvc - 1 downto 0 do
	  sp := Var(i)::!sp
	done;
	usableeqns := (defname,Abbrev(mr,!sp),m)::!usableeqns;
	let bibr = ref None in
	if !report then Printf.printf "Definition %s %d := %s.\n" defname (List.length yl) mr;
	abbrev_lookup ar
	  (fun bib ->
	    bibr := Some(bib);
	    if not (creditnyml = None) then raise (Failure(defname ^ " is not new, so no nyms should be associated"));
	    if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs)
	  (fun () ->
	    if !summary then
	      begin
		Printf.printf "\n\\begin{definition} $%s" defname;
		begin
		  match yl with
		  | [] -> Printf.printf "$ is defined to be $";
		  | y::yr ->
		      Printf.printf "(%s" y;
		      List.iter (fun z -> Printf.printf ",%s" z) yr;
		      Printf.printf ")$ is defined to be ";
		end;
		ltree_to_latex_math_or_matharray_top true stdout b;
		Printf.printf "\n\\end{definition}\n";
	      end;
	    allabbrevs_merkl := merkle_insert !allabbrevs_merkl ar !currentfiletitle);
	begin
	  if Hashtbl.mem abbrevroot defname then raise (Failure("redeclaration of " ^ defname));
	  if !sexpr then
	    begin
	      Printf.printf "(DEF \"%s\" %d \"%s\")\n" defname prevfvc mr
	    end;
	  if !ivy then Printf.printf "(IVYDEF \"%s\" %s)\n" defname (tm_ivy m);
	  Hashtbl.add abbrevroot defname mr;
	  Hashtbl.add abbrevarity mr prevfvc;
	  Hashtbl.add abbrevdef mr m;
	  if !latex then
	    begin
	      Hashtbl.add global_latex_ref defname (Printf.sprintf "{\\mbox{Definition}}~\\ref{def:%s}" defname);
	      Printf.fprintf !latexc "\n\\begin{definition}%s\\label{def:%s} $%s"
		(if List.mem (List.length yl,mr) main5abbrevs then
		  (match !bibr with None -> "{\\textup{(}cf.~\\cite{KinyonVV13}\\textup{)}}" | Some(bib) -> Printf.sprintf "{\\textup{(}cf.~\\cite{KinyonVV13,AIM:%s}\\textup{)}}" bib)
		else
		  (match !bibr with None -> "" | Some(bib) -> Printf.sprintf "{\\textup{(}cf.~\\cite{AIM:%s}\\textup{)}}" bib))
		defname defname;
	      begin
		match yl with
		| [] -> Printf.fprintf !latexc "$ is defined to be $";
		| y::yr ->
		    Printf.fprintf !latexc "(%s" y;
		    List.iter (fun z -> Printf.fprintf !latexc ",%s" z) yr;
		    Printf.fprintf !latexc ")$ is defined to be ";
	      end;
	      ltree_to_latex_math_or_matharray_top true !latexc b;
	      Printf.fprintf !latexc "\n\\end{definition}\n";
	    end;
	  if !html then
	    begin
	      Hashtbl.add global_html_ref defname (Printf.sprintf "Definition <a href='#def:%d'>%d</a>" !htmlthmcount !htmlthmcount);
	      Printf.printf "\n<div class='def'><a name='def:%d'/><b><a href='ref.php?h=%s'>Definition %d:</a></b> %s" !htmlthmcount mr !htmlthmcount defname;
	      begin
		match yl with
		| [] -> Printf.printf " is defined to be ";
		| y::yr ->
		    Printf.printf "(%s" y;
		    List.iter (fun z -> Printf.printf ",%s" z) yr;
		    Printf.printf ") is defined to be ";
	      end;
	      ltree_to_html_math stdout (html_outer_ref fvh) b;
	      Printf.printf "</div>\n";
	      incr htmlthmcount;
	    end
	end
  | AxDecl(name,b) ->
      let fvh = Hashtbl.create 10 in
      let fvc = ref 0 in
      let fv = (fvh,fvc) in
      let cl = interp_aim_clause_r (ltree_to_atree b) fv [] in
      let clr = clause_root cl in
      begin
	match cl with
	| [Eq(lhs,rhs)] -> usableeqns := (name,lhs,rhs)::!usableeqns
	| _ -> ()
      end;
      if not (List.mem clr axioms) then
	begin
	  Printf.printf "%s is not one of the AIM axioms. Use Known for lemmas that have been proven in previous articles.\n" name;
	  exit 1
	end;
      known_lookup clr
	(fun clrtr -> ())
	(fun () ->
	  allknowns_merkl := merkle_insert !allknowns_merkl clr !currentfiletitle);
      Hashtbl.add knownclauses name clr;
      if !sexpr then Printf.printf "(AXIOM \"%s\" \"%s\")\n" name clr;
      if !ivy then Printf.printf "(IVYAXIOM \"%s\" %s)\n" name (clause_ivy (let (cl,_) = okify_clause cl (Array.make (clause_var_upperbd cl) (Skol("fake"))) 0 in cl));
      if !latex then
	begin
	  Hashtbl.add global_latex_ref name (Printf.sprintf "{\\mbox{Axiom}}~\\ref{%s}" name);
	  Printf.fprintf !latexc "\n\\begin{axiom}{\\textup{(}cf.~\\cite{KinyonVV13}\\textup{)}}\\label{%s} " name;
	  ltree_to_latex_math_or_matharray_top false !latexc b;
	  Printf.fprintf !latexc "\n\\end{axiom}\n";
	end;
      if !html then
	begin
	  Hashtbl.add global_html_ref name (Printf.sprintf "Axiom <a href='#ax:%d'>%d</a>" !htmlthmcount !htmlthmcount);
	  Printf.printf "\n<div class='axiom'><a name='ax:%d'><b><a href='ref.php?h=%s'>Axiom %d:</a></b> " !htmlthmcount clr !htmlthmcount;
	  ltree_to_html_math stdout (html_outer_ref fvh) b;
	  Printf.printf "</div>\n";
	  incr htmlthmcount;
	end;
  | KnownDecl(name,b) ->
      let fvh = Hashtbl.create 10 in
      let fvc = ref 0 in
      let fv = (fvh,fvc) in
      let cl = interp_aim_clause_r (ltree_to_atree b) fv [] in
      let clr = clause_root cl in
      let bibr = ref None in
      begin
	match cl with
	| [Eq(lhs,rhs)] -> usableeqns := (name,lhs,rhs)::!usableeqns
	| _ -> ()
      end;
      if List.mem clr axioms then raise (Failure(name ^ " is one of the AIM Axioms and should be declared with Axiom instead of Known"));
(***
      known_lookup clr
	(fun bib ->
	  bibr := Some(bib);
	  if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs)
	(fun () -> raise (Failure(name ^ " is not known")));
***)
      Hashtbl.add knownclauses name clr;
      if !sexpr then Printf.printf "(KNOWN \"%s\" \"%s\")\n" name clr;
      if !ivy then Printf.printf "(IVYKNOWN \"%s\" %s)\n" name (clause_ivy (let (cl,_) = okify_clause cl (Array.make (clause_var_upperbd cl) (Skol("fake"))) 0 in cl));
      if !latex then
	begin
	  Hashtbl.add global_latex_ref name (Printf.sprintf "{\\mbox{Proposition}}~\\ref{%s}" name);
	  Printf.fprintf !latexc "\n\\begin{proposition}\\label{%s} " name;
	  (match !bibr with None -> () | Some(bib) -> Printf.fprintf !latexc "{\\textup{(}cf.~\\cite{AIM:%s}\\textup{)}} " bib);
	  ltree_to_latex_math_or_matharray_top true !latexc b;
	  Printf.fprintf !latexc "\n\\end{proposition}\n";
	end;
      if !html then
	begin
	  Hashtbl.add global_html_ref name (Printf.sprintf "Proposition <a href='#prop:%d'>%d</a>" !htmlthmcount !htmlthmcount);
	  Printf.printf "\n<div class='known'><a name='prop:%d'><b><a href='ref.php?h=%s'>Known Proposition:</a></b> " !htmlthmcount clr;
	  ltree_to_html_math stdout (html_outer_ref fvh) b;
	  Printf.printf "</div>\n";
	  incr htmlthmcount;
	end;
  | LoopDecl(creditnyml,name,lll) ->
      begin
	incr itemcount;
	addothercreditnyml creditnyml;
	try
	  let (l,lspec) = pretty_loop_to_loopspec lll in
	  let lspech = sha256 lspec in
	  loop_lookup lspech
	    (fun bib ->
	      if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs;
	      if !latex then
		if lspech = "d3a44d71fe731c393c7ffe6fe9394db284ba16cd400daef013efc4c6997e5fc0" then
		  Printf.fprintf !latexc "The following AIM loop was given in~\\cite{KinyonVV13} and~\\cite{AIM:%s}.\n" bib
		else
		  Printf.fprintf !latexc "The following AIM loop was first given in~\\cite{AIM:%s}.\n" bib;
	      if !report then (Printf.printf "Loop %s (%s) is not new, was first in %s\n" lspec lspech bib))
	    (fun () ->
	      if !latex && lspech = "d3a44d71fe731c393c7ffe6fe9394db284ba16cd400daef013efc4c6997e5fc0" then
		Printf.fprintf !latexc "The following AIM loop was given in~\\cite{KinyonVV13}.\n";
	      allloops_merkl := merkle_insert !allloops_merkl lspech !currentfiletitle;
	      if !report then (Printf.printf "Loop %s (%s) is new\n" lspec lspech));
	  begin
	    match name with
	    | Some(name) ->
		Hashtbl.add global_loops name l;
		Hashtbl.add global_loop_hash name lspech;
	    | None -> ()
	  end;
	  if !sexpr then
	    begin
	      Printf.printf "(AIMLOOP %s \"%s\" \"%s\")\n" (ostring_sexpr name) lspec lspech
	    end;
	  if !latex then
	    begin
	      let (card,_,_,_) = l in
	      begin
		match name with
		| Some(name) ->
		    Printf.fprintf !latexc "\\begin{example}\\label{ex:%s} The following is the multiplication table for an AIM loop with %d elements:\n$$" name card;
		| None ->
		    Printf.fprintf !latexc "\\begin{example} The following is the multiplication table for an AIM loop with %d elements:\n$$" card;
	      end;
	      Printf.fprintf !latexc "\\left|\\begin{array}{";
	      for i = 1 to card do Printf.fprintf !latexc "c" done;
	      Printf.fprintf !latexc "}\n";
	      for i = 0 to card-1 do
		Printf.fprintf !latexc "%s" (List.nth (List.nth lll 0) i);
		for j = 1 to card-1 do
		  Printf.fprintf !latexc " & %s" (List.nth (List.nth lll j) i);
		done;
		Printf.fprintf !latexc "\\\\\n";
	      done;
	      Printf.fprintf !latexc "\\end{array}\\right|\n";
	      Printf.fprintf !latexc "$$\n\\end{example}\n\n";
	    end;
	  if !html then
	    begin
	      let (card,_,_,_) = l in
	      (match name with Some(name) -> Hashtbl.add global_loop_htmlnum name !htmlthmcount | None -> ());
	      Printf.printf "<div class='example'><a name='ex:%d'/><b><a href='ref.php?h=%s'>Example %d:</a></b> The following is the multiplication table for an AIM loop with %d elements:<br/>\n" !htmlthmcount lspech !htmlthmcount card;
	      incr htmlthmcount;
	      Printf.printf "<table>";
	      for i = 0 to card-1 do
		Printf.printf "<tr>";
		let clsuf = if i = 0 then "t" else if i=card-1 then "b" else "" in
		Printf.printf "<td class='matr%sl'></td>" clsuf;
		for j = 0 to card-1 do
		  Printf.printf "<td>%s</td>" (List.nth (List.nth lll j) i);
		done;
		Printf.printf "<td class='matr%sr'></td>" clsuf;
		Printf.printf "</tr>\n";
	      done;
	      Printf.printf "<table><br/>\n</div>\n";
	    end;
	with (Failure(msg)) ->
	  match name with
	  | Some(name) ->
	      raise (Failure(name ^ " does not appear to be an AIM loop: " ^ msg))
	  | None ->
	      raise (Failure("Unnamed example does not appear to be an AIM loop: " ^ msg))
      end
  | NonThmDecl(creditnyml,name,b,byl) ->
      incr itemcount;
      addothercreditnyml creditnyml;
      let fvh = Hashtbl.create 10 in
      let fvc = ref 0 in
      let fv = (fvh,fvc) in
      let cl = interp_aim_clause_r (ltree_to_atree b) fv [] in
      let clr = clause_root cl in
      let dl = delta_normalize_clause cl (abbrevarity,abbrevdef) in
      conj_lookup clr
	(fun clrcr ->
	  if clrcr = !currentfileh58 then
	    raise Not_found
	  else
	    begin
	      if not (List.mem clrcr !articlerefs) then articlerefs := clrcr::!articlerefs;
	      nonthm_lookup clr
		(fun bib ->
		  if bib = !currentfileh58 then
		    raise Not_found
		  else
		    begin
		      if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs;
		      if !latex then
			Printf.fprintf !latexc "\nThe following was first conjectured in~\\cite{AIM:%s} and has been known to be a nontheorem since~\\cite{AIM:%s}.\n" clrcr bib
		    end)
		(fun () -> Printf.fprintf !latexc "\nThe following was first conjectured in~\\cite{AIM:%s}.\n" clrcr)
	    end)
	(fun () ->
	  nonthm_lookup clr
	    (fun bib ->
	      if bib = !currentfileh58 then
		raise Not_found
	      else
		begin
		  if not (List.mem bib !articlerefs) then articlerefs := bib::!articlerefs;
		  if !latex then
		    Printf.fprintf !latexc "\nThe following has been known to be a nontheorem since~\\cite{AIM:%s}.\n" bib
		end)
	    (fun () ->
	      allnonthms_merkl := merkle_insert !allnonthms_merkl clr !currentfiletitle));
      List.iter
	(fun loopname ->
	  try
	    let l = Hashtbl.find global_loops loopname in
	    let (card,_,_,_) = l in
	    let tocheck = ref 1 in
	    for i = 1 to !fvc do tocheck := !tocheck * card done;
	    if !tocheck > 1000000 then raise (Failure("not willing to check nontheorem in " ^ loopname ^ " since too many variables for a model this big"));
	    if clause_valid_p l dl then
	      raise (Failure("The clause" ^ (match name with Some(name) -> " " ^ name | None -> "") ^ " is valid in " ^ loopname))
	    else
	      if !report then (match name with Some(name) -> Printf.printf "%s is false in %s\n" name loopname | None -> Printf.printf "Nonthm is false in %s\n" loopname);
	  with Not_found ->
	    raise (Failure("Do not know loop " ^ loopname)))
	byl;
      if !sexpr then
	begin
	  Printf.printf "(NONTHM \"%s\"" clr;
	  List.iter (fun loopname -> let h = Hashtbl.find global_loop_hash loopname in Printf.printf " \"%s\"" h) byl;
	  Printf.printf ")\n";
	end;
      if !latex then
	begin
	  match byl with
	  | [] -> ()
	  | [by] ->
	      Printf.fprintf !latexc "\nExample~\\ref{ex:%s} provides a countermodel for the following: " by;
	      ltree_to_latex_math_or_matharray_top true !latexc b;
	      Printf.fprintf !latexc "\n";
	  | (by::byr) ->
	      Printf.fprintf !latexc "\nExamples~\\ref{ex:%s}" by;
	      for i = 0 to (List.length byr) - 2 do
		Printf.fprintf !latexc ", \\ref{ex:%s}" (List.nth byr i)
	      done;
	      Printf.fprintf !latexc " and~\\ref{ex:%s} provide countermodels for the following: " (List.nth byr ((List.length byr) -1));
	      ltree_to_latex_math_or_matharray_top true !latexc b;
	      Printf.fprintf !latexc "\n";
	end;
      if !html then
	begin
	  let clr = clause_root cl in
	  let getbyn by =
	    try
	      Hashtbl.find global_loop_htmlnum by
	    with Not_found ->
	      raise (Failure("unknown example ref " ^ by))
	  in
	  match byl with
	  | [] -> ()
	  | [by] ->
	      let byn = getbyn by in
	      Printf.printf "\n<div class='nonthm'>Example <a href='#ex:%d'>%d</a> provides a countermodel for the <a href='ref.php?h=%s'>following</a>: " byn byn clr;
	      ltree_to_html_math stdout (html_outer_ref fvh) b;
	      Printf.printf ".</div>\n";
	  | (by::byr) ->
	      let byn = getbyn by in
	      Printf.printf "\n<div class='nonthm'>Examples <a href='#ex:%d'>%d</a>" byn byn;
	      for i = 0 to (List.length byr) - 2 do
		let byn = getbyn (List.nth byr i) in
		Printf.printf ", <a href='#ex:%d'>%d</a>" byn byn
	      done;
	      let byn = getbyn (List.nth byr ((List.length byr) - 1)) in
	      Printf.printf " and <a href='#ex:%d'>%d</a> provide countermodels for the <a href='ref.php?h=%s'>following</a>: " byn byn clr;
	      ltree_to_html_math stdout (html_outer_ref fvh) b;
	      Printf.printf ".</div>\n";
	end
  | _ -> raise (Failure "Unhandled article docitem case");;
    
let file_contents f =
  let c = open_in f in
  let firstline = read_signatures_then_line c true in
  let firstlinel = split_string '@' firstline in
  let filetype =
    match firstlinel with
    | ["ISSUE"] -> "ISSUE"
    | ["ARTICLE"] -> "ARTICLE"
    | ["ARTICLE";title] -> articletitle := Some(title); "ARTICLE"
    | _ -> raise (Failure("bad first line " ^ firstline))
  in
  let b = Buffer.create 100 in
  Buffer.add_string b firstline;
  try
    while true do
      let l = input_line c in
      Buffer.add_char b '\n';
      Buffer.add_string b l;
    done;
    raise End_of_file
  with End_of_file ->
    close_in c;
    let bs = Buffer.contents b in
    (Sha256.sha256 bs,filetype,bs)

let cyrtaimarticle c =
  open_advisor_socket();
  let tl = ref (TokStrRest(Lexer.token,Lexing.from_channel c)) in
  lineno := 2;
  charno := 0;
  try
    while true do
      match !proving with
      | None ->
	  begin
	    match !eqproving with
	    | None ->
		let (ditem,tr) = parse_docitem !tl in
		tl := tr;
		evaluate_article_docitem ditem
	    | Some(thmname,thmhash,n,pctx,label,l,r,cl,goalstack) ->
		let (epfitem,tr) = parse_eqpftacitem !tl in
		tl := tr;
		evaluate_eqpftacitem thmname thmhash n pctx label l r cl goalstack epfitem
	  end
      | Some(thmname,thmhash,n,goalstack) ->
	  let (pfitem,tr) = parse_pftacitem !tl in
	  tl := tr;
	  evaluate_pftacitem thmname thmhash n goalstack pfitem
    done
  with
  | Lexer.Eof ->
      close_advisor_socket();
      close_in c
  | End_of_file ->
      close_advisor_socket();
      close_in c
  | ParsingError(x,l1,c1,l2,c2) ->
      close_advisor_socket();
      close_in c;
      Printf.printf "Syntax error:%s\nfrom line %d char %d to line %d char %d\n" x l1 c1 l2 c2;
      exit 1;;

let check_issue_sigs fileh currenteditors =
  editorslefttosign :=
    List.map
      (fun (r,n) ->
	let (x,y) = pubkey_from_aimnym n in
	(r,(x,y),n))
      currenteditors;
  editorssigned := [];
  begin
    List.iter
      (fun (sg,sgs) ->
	try
	  let (r,(x,y),a) =
	    List.find
	      (fun (_,(x,y),_) ->
		let msg = big_int_of_hex fileh in
		let b = check_sgn (Some(x,y)) sg msg in
		b)
	      !editorslefttosign
	  in
	  editorssigned := (r,(x,y),a)::!editorssigned;
	  editorslefttosign := List.filter (fun (_,(_,_),a2) -> not (a = a2)) !editorslefttosign;
	with Not_found ->
	  try
	    let (_,_,(a1,a2,a3)) =
	      List.find
		(fun (_,(x,y),_) ->
		  let msg = big_int_of_hex fileh in
		  check_sgn (Some(x,y)) sg msg)
		!editorssigned
	    in
	    Printf.printf "Signature %s is a second signature for the same nym %s %s %s\n" sgs a1 a2 a3;
	    exit 1
	  with Not_found ->
	    Printf.printf "Signature %s does not match an author's nym.\n" sgs;
	    exit 1)
      !sgns;
  end;;

let check_article_sigs fileh =
  authpubkeyslefttosign := !articleauthorpubkeys;
  authpubkeyssigned := [];
  begin
    List.iter
      (fun (sg,sgs) ->
	try
	  let ((x,y),a) =
	    List.find
	      (fun ((x,y),_) ->
		let msg = big_int_of_hex fileh in
		let b = check_sgn (Some(x,y)) sg msg in
		b)
	      !authpubkeyslefttosign
	  in
	  authpubkeyssigned := ((x,y),a)::!authpubkeyssigned;
	  authpubkeyslefttosign := List.filter (fun ((_,_),a2) -> not (a = a2)) !authpubkeyslefttosign;
	with Not_found ->
	  try
(*
	    let (_,(a1,a2,a3)) =
	      List.find
		(fun ((x,y),_) ->
		  let msg = big_int_of_hex fileh in
		  check_sgn (Some(x,y)) sg msg)
		!authpubkeyssigned
   in
	    
	    Printf.printf "Signature %s is a second signature for the same nym %s %s %s\n" sgs a1 a2 a3;
	    exit 1
*)
	    raise Not_found
	  with Not_found ->
	    Printf.printf "Signature %s does not match an author's nym.\n" sgs;
	    exit 1)
      !sgns;
  end;;

let writeaimbibfile () =
  let bibfilename = "aim.bib" in
  if not (Sys.file_exists bibfilename) then (** only create if seems to be missing **)
    let latexbibc = open_out bibfilename in
    Printf.fprintf latexbibc "@inproceedings{KinyonVV13,\n";
    Printf.fprintf latexbibc "  author    = {Michael K. Kinyon and\n";
    Printf.fprintf latexbibc "               Robert Veroff and\n";
    Printf.fprintf latexbibc "               Petr Vojt{\\v{e}}chovsk{\\'{y}}},\n";
    Printf.fprintf latexbibc "  title     = {Loops with Abelian Inner Mapping Groups: An Application of Automated\n";
    Printf.fprintf latexbibc "               Deduction},\n";
    Printf.fprintf latexbibc "  booktitle = {Automated Reasoning and Mathematics - Essays in Memory of William\n";
    Printf.fprintf latexbibc "               W. McCune},\n";
    Printf.fprintf latexbibc "  pages     = {151--164},\n";
    Printf.fprintf latexbibc "  year      = {2013},\n";
    Printf.fprintf latexbibc "  crossref  = {2013mccune}\n";
    Printf.fprintf latexbibc "}\n";
    Printf.fprintf latexbibc "\n";
    Printf.fprintf latexbibc "@proceedings{2013mccune,\n";
    Printf.fprintf latexbibc "  editor    = {Maria Paola Bonacina and\n";
    Printf.fprintf latexbibc "               Mark E. Stickel},\n";
    Printf.fprintf latexbibc "  title     = {Automated Reasoning and Mathematics - Essays in Memory of William\n";
    Printf.fprintf latexbibc "               W. McCune},\n";
    Printf.fprintf latexbibc "  series    = {Lecture Notes in Computer Science},\n";
    Printf.fprintf latexbibc "  volume    = {7788},\n";
    Printf.fprintf latexbibc "  publisher = {Springer},\n";
    Printf.fprintf latexbibc "  year      = {2013}\n";
    Printf.fprintf latexbibc "}\n";
    close_out latexbibc;;
    
let writedraftbibfile atitle = (** bib file for unpublished drafts **)
  let bibfilename = Printf.sprintf "%s.bib" atitle in (* local bib file *)
  if not (Sys.file_exists bibfilename) then (** do not overwrite since the bibfile made when the issue was published should be the official one with a volume and issue number **)
    let latexbibc = open_out bibfilename in
    Printf.fprintf latexbibc "@MISC{AIM:%s,\n" atitle;
    begin
      match !articletitle with
      | Some(title) ->
	  Printf.fprintf latexbibc "title = {{%s}},\n" title;
      | None ->
	  Printf.fprintf latexbibc "title = {{%s}},\n" (title_split atitle);
    end;
    begin
      match !articleauthors with
      | None -> ()
      | Some([]) -> ()
      | Some((a1,a2,a3)::ar) ->
	  Printf.fprintf latexbibc "author = {%s, %s. %s." a3 (String.sub a1 0 1) (String.sub a2 0 1);
	  List.iter
	    (fun (a1,a2,a3) -> Printf.fprintf latexbibc " and %s, %s. %s." a3 (String.sub a1 0 1) (String.sub a2 0 1))
	    ar;
	  Printf.fprintf latexbibc "},\n"
    end;
    Printf.fprintf latexbibc "note = {{Unpublished Draft}},\n";
    let ltm = Unix.localtime (Unix.time()) in
    let currentyear = 1900 + ltm.Unix.tm_year in
    let currentmonth = List.nth ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"] ltm.Unix.tm_mon in
    Printf.fprintf latexbibc "year = {%d},\nmonth = {%s}}\n" currentyear currentmonth;
    close_out latexbibc;;
      
let cyrtaimissue c currentissueno currentvolumeno =
  let tl = ref (TokStrRest(Lexer.token,Lexing.from_channel c)) in
  lineno := 2;
  charno := 0;
  try
    while true do
      let (ditem,tr) = parse_docitem !tl in
      tl := tr;
      begin
	match ditem with
	| PreviousIssue(x) -> () (*** handled earlier ***)
	| _ -> raise (Failure "no")
      end;
    done
  with
  | Lexer.Eof -> close_in c
  | End_of_file -> close_in c
  | ParsingError(x,l1,c1,l2,c2) ->
      close_in c;
      Printf.printf "Syntax error:%s\nfrom line %d char %d to line %d char %d\n" x l1 c1 l2 c2;
      exit 1;;

let main () =
  let i = Array.length Sys.argv in
  let j = ref 0 in
  while (!j < i - 2) do
    incr j;
    if Sys.argv.(!j) = "-sexpr" then
      sexpr := true
    else if Sys.argv.(!j) = "-evaluateconj" then
      evaluateconj := true
    else if Sys.argv.(!j) = "-countoptions" then
      countoptions := true
    else if Sys.argv.(!j) = "-ivy" then
      ivy := true
    else if Sys.argv.(!j) = "-latex" then
      latex := true
    else if Sys.argv.(!j) = "-fancylatex" then
      (latex := true; fancy := true)
    else if Sys.argv.(!j) = "-html" then
      html := true
    else if Sys.argv.(!j) = "-tptp" then
      tptpstyle := true
    else if Sys.argv.(!j) = "-publish" then
      publish := true
    else if Sys.argv.(!j) = "-prover9" then
      prover9style := true
    else if Sys.argv.(!j) = "-report" then
      report := true
    else if Sys.argv.(!j) = "-ensurecompletelysigned" then
      ensurecompletelysigned := true
    else if Sys.argv.(!j) = "-checkconjecturesforsubsumption" then
      check_conjectures_for_subsumption := true
    else if Sys.argv.(!j) = "-fakelearn" then
      fakelearn := true
    else if Sys.argv.(!j) = "-fakedist" then
      begin
	let f = open_in Sys.argv.(!j+1) in
	incr j;
	try
	  while true do
	    let l = input_line f in
	    if l = "FAKEDIST" then
	      let l1 = input_line f in
	      let l2 = input_line f in
	      let d = float_of_string (input_line f) in
	      Hashtbl.add fakedist (l1,l2) d
	    else if l = "FAKEDEPTH" then
	      let l1 = input_line f in
	      let d = int_of_string (input_line f) in
	      Hashtbl.add fakedepthh l1 d
	  done
	with End_of_file ->
	  close_in f
      end
    else if Sys.argv.(!j) = "-dyndist" then
      begin
        dyndist := true;
        usebatchdist := false
      end
    else if Sys.argv.(!j) = "-verb" then
      begin
	verbose := int_of_string Sys.argv.(!j+1);
	incr j;
      end
    else if Sys.argv.(!j) = "-maxdepth" then
      begin
	maxdepth := float_of_string Sys.argv.(!j+1);
	incr j;
      end
    else if Sys.argv.(!j) = "-abslimit" then
      begin
	abstracttimelimit := Some(int_of_string Sys.argv.(!j+1));
	incr j;
      end
    else if Sys.argv.(!j) = "-advisorsocket" then
      begin
	advisorsocket := Some(int_of_string Sys.argv.(!j+1));
	incr j;
      end
    else if Sys.argv.(!j) = "-constdist" then
      begin
	constdist := Some(float_of_string Sys.argv.(!j+1));
        usebatchdist := false;
	incr j;
      end
    else if Sys.argv.(!j) = "-summary" then
      begin
	summary := true
      end
    else if Sys.argv.(!j) = "-reverseusable" then
      begin
	reverseusable := true
      end
    else
      Printf.printf "Ignoring %s\n" (Sys.argv.(!j));
  done;
  if !j >= (i-1) then exit 0; (*** in this case no file was given to process as an article or issue ***)
  let (fileh,filetype,bs) = file_contents (Sys.argv.(i-1)) in
  currentfileh := fileh;
  currentfileh58 := base58 (big_int_of_hex fileh);
  currentfiletitle := !currentfileh58;
  begin
    if filetype = "ISSUE" then
      begin
	let c = open_in Sys.argv.(i-1) in
	let _ = read_signatures_then_line c false in
	let tl = ref (TokStrRest(Lexer.token,Lexing.from_channel c)) in
	lineno := 2;
	charno := 0;
	try
	  begin
	    let (ditem,tr) = parse_docitem !tl in
	    match ditem with
	    | PreviousIssue(x) ->
		previousissue := Some(x);
		close_in c;
	    | _ ->
		genesis_state();
		close_in c
	  end
	with e ->
	  Printf.printf "Error: %s\n" (Printexc.to_string e);
	  close_in c;
	  exit 1
      end
    else
      begin
	genesis_state()
      end;
  end;
  let currenteditors = !editors in
  let currentquorum = !quorum in
  let currentvolumeno = !volumeno in
  let currentissueno = !issueno in
  let _ = !allknowns_merkl in
  let _ = !allconjs_merkl in
  let _ = !allnonthms_merkl in
  let _ = !allloops_merkl in
  let _ = !allabbrevs_merkl in
  if filetype = "ISSUE" then incr issueno;
  if !latex then
    if filetype = "ISSUE" then
      begin
	latexc := open_out (Printf.sprintf "%s.tex" !currentfiletitle);
	Printf.fprintf !latexc "\\documentclass{book}\n";
	Printf.fprintf !latexc "\\usepackage{pdfpages}\n";
	Printf.fprintf !latexc "\n";
	Printf.fprintf !latexc "\\begin{document}\n";
	Printf.fprintf !latexc "\n";
	Printf.fprintf !latexc "\\begin{titlepage}\n\\begin{center}";
	Printf.fprintf !latexc "{\\huge{Journal of Certified AIM Proofs}}\\\\\n\\vspace{1cm}";
	Printf.fprintf !latexc "{\\Large{Volume %d Issue %d}}\\\\\\vspace{5mm}\n" currentvolumeno currentissueno;
	Printf.fprintf !latexc "{\\texttt{%s}}\\\\\n\\vspace{1cm}" (title_split (base58 (big_int_of_hex fileh)));
	let latex_editors rankname el =
	  match el with
	  | [] -> ()
	  | [(_,(e1,e2,e3))] ->
	      if rankname = "Chief" then
		Printf.fprintf !latexc "{\\sc{Editor-in-Chief}}\\\\\n"
	      else
		Printf.fprintf !latexc "{\\sc{%s Editor}}\\\\\n" rankname;
	      Printf.fprintf !latexc "%s %s %s\\\\\n\\vspace{1cm}" e1 e2 e3
	  | el ->
	      if rankname = "Chief" then
		Printf.fprintf !latexc "{\\sc{Editors-in-Chief}}\\\\\n"
	      else
		Printf.fprintf !latexc "{\\sc{%s Editors}}\\\\\n" rankname;
	      List.iter
		(fun (_,(e1,e2,e3)) ->
		  Printf.fprintf !latexc "%s %s %s\\\\\n\\vspace{5mm}" e1 e2 e3)
		el;
	      Printf.fprintf !latexc "\\vspace{5mm}";
	in
	latex_editors "Chief" (List.filter (fun (r,_) -> r = 4) currenteditors);
	latex_editors "Full" (List.filter (fun (r,_) -> r = 3) currenteditors);
	latex_editors "Associate" (List.filter (fun (r,_) -> r = 2) currenteditors);
	latex_editors "Assistant" (List.filter (fun (r,_) -> r = 1) currenteditors);
	Printf.fprintf !latexc "\\end{center}\\end{titlepage}\n"
      end
    else if filetype = "ARTICLE" then
      begin
	writeaimbibfile();
	latexc := open_out (Printf.sprintf "%s.tex" !currentfiletitle);
	Printf.fprintf !latexc "\\documentclass{article}\n";
	Printf.fprintf !latexc "\\usepackage[left=2cm, right=4cm, top=2cm]{geometry}\n";
	Printf.fprintf !latexc "\n";
	Printf.fprintf !latexc "\\usepackage{amsthm}\n";
(*	Printf.fprintf !latexc "\\usepackage{nopageno}\n"; *)
	if !fancy then
	  begin
	    Printf.fprintf !latexc "\\usepackage{fancyhdr}\n";
	    Printf.fprintf !latexc "\\usepackage{graphicx}\n";
	    Printf.fprintf !latexc "\\pagestyle{fancy}\n";
	    Printf.fprintf !latexc "\\fancypagestyle{plain}{\n";
	    Printf.fprintf !latexc "  \\renewcommand{\\headrulewidth}{0pt}\n";
	    Printf.fprintf !latexc "  \\fancyhf{}\n";
	    Printf.fprintf !latexc "  \\fancyfoot[L]{\\includegraphics[height=1cm]{JCAIMPlogo2.png}}\n";
	    Printf.fprintf !latexc "  \\fancyfoot[R]{\\thepage}\n";
	    Printf.fprintf !latexc "		     }\n";
	  end;
	Printf.fprintf !latexc "\n";
	Printf.fprintf !latexc "\\def\\limplies{\\rightarrow}\n";
	Printf.fprintf !latexc "\\newtheorem{axiom}{Axiom}\n";
	Printf.fprintf !latexc "\\newtheorem{definition}{Definition}\n";
	Printf.fprintf !latexc "\\newtheorem{conjecture}{Conjecture}\n";
	Printf.fprintf !latexc "\\newtheorem{proposition}{Proposition}\n";
	Printf.fprintf !latexc "\\newtheorem{lemma}{Lemma}\n";
	Printf.fprintf !latexc "\\newtheorem{theorem}{Theorem}\n";
	Printf.fprintf !latexc "\\newtheorem{corollary}{Corollary}\n";
	Printf.fprintf !latexc "\\newtheorem{example}{Example}\n";
	Printf.fprintf !latexc "\n";
	Printf.fprintf !latexc "\\begin{document}\n";
	Printf.fprintf !latexc "\n";
	if !fancy then
	  begin
	    Printf.fprintf !latexc "\\lhead{{\\sc{Journal of Certified AIM Proofs}}}\n";
	    Printf.fprintf !latexc "\\rhead{{\\sc{Volume %d Issue %d}}}\n" !volumeno !issueno;
	    Printf.fprintf !latexc "\\lfoot{\\includegraphics[height=1cm]{JCAIMPlogo2.png}}\n";
	    Printf.fprintf !latexc "\\cfoot{}\n";
	    Printf.fprintf !latexc "\\rfoot{\\thepage}\n";
	  end;
	match !articletitle with
	| Some(title) ->
	    Printf.fprintf !latexc "\\title{%s}\n" title
	| None ->
	    let atitle = base58 (big_int_of_hex fileh) in
	    let title = title_split atitle in
	    Printf.fprintf !latexc "\\title{%s}\n" title
      end;
  if !html then
    if filetype = "ISSUE" then
      begin
	Printf.printf "<div class='journalname'>Journal of Certified AIM Proofs</div>\n";
	Printf.printf "<div class='volumeissue'>Volume %d Issue %d</div>\n" currentvolumeno currentissueno;
	Printf.printf "<div class='issuetitle'>%s</div>\n" (title_split (base58 (big_int_of_hex fileh)));
	Printf.printf "<div class='editorialboard'>\n";
	let latex_editors rankname el =
	  match el with
	  | [] -> ()
	  | [(_,(e1,e2,e3))] ->
	      if rankname = "Chief" then
		Printf.printf "<div class='editorrank'>Editor-in-Chief</div>\n"
	      else
		Printf.printf "<div class='editorrank'>%s Editor</div>\n" rankname;
	      Printf.printf "<div class='editor'>%s %s %s</div>\n" e1 e2 e3
	  | el ->
	      if rankname = "Chief" then
		Printf.printf "<div class='editorrank'>Editors-in-Chief</div>\n"
	      else
		Printf.printf "<div class='editorrank'>%s Editors</div>\n" rankname;
	      List.iter
		(fun (_,(e1,e2,e3)) ->
		  Printf.printf "<div class='editor'>%s %s %s</div>\n" e1 e2 e3)
		el;
	in
	latex_editors "Chief" (List.filter (fun (r,_) -> r = 4) currenteditors);
	latex_editors "Full" (List.filter (fun (r,_) -> r = 3) currenteditors);
	latex_editors "Associate" (List.filter (fun (r,_) -> r = 2) currenteditors);
	latex_editors "Assistant" (List.filter (fun (r,_) -> r = 1) currenteditors);
	Printf.printf "</div>\n";
      end
    else if filetype = "ARTICLE" then
      begin
	match !articletitle with
	| Some(title) ->
	    Printf.printf "<div class='title'>%s</div>\n" title
	| None ->
	    Printf.printf "<div class='title'>%s</div>\n" (title_split (base58 (big_int_of_hex fileh)))
      end;
  begin
    let c = open_in Sys.argv.(i-1) in
    let firstline = read_signatures_then_line c false in
    let firstlinel = split_string '@' firstline in
    let filetype =
      match firstlinel with
      | ["ISSUE"] -> "ISSUE"
      | ["ARTICLE"] -> "ARTICLE"
      | ["ARTICLE";title] -> articletitle := Some(title); "ARTICLE"
      | _ -> raise (Failure("bad first line " ^ firstline))
    in
    if filetype = "ISSUE" then
      begin
	unlistededitors := !editors;
	cyrtaimissue c currentissueno currentvolumeno
      end
    else if filetype = "ARTICLE" then
      begin
        set_eqnindh();
	cyrtaimarticle c;
	begin
	  match !admits with
	  | [] -> ()
	  | _ ->
	      Printf.printf "%d incomplete proofs in article:\n" (List.length !admits);
	      List.iter (fun admit -> Printf.printf "%s\n" admit) !admits;
	      flush stdout;
	      if !latex then
		begin
		  Printf.fprintf !latexc "\n{\\it{Warning: Article has incomplete proofs}}\n\\end{document}\n";
		  close_out !latexc;
		  writedraftbibfile !currentfileh58;
		  Printf.printf "pdflatex %s\n" !currentfiletitle;
		  Printf.printf "bibtex %s\n" !currentfiletitle;
		  Printf.printf "pdflatex %s\n" !currentfiletitle;
		end;
	      exit 1
	end;
      end
    else
      raise (Failure "first line of file should indicate if it is an ISSUE or ARTICLE");
  end;
  if filetype = "ISSUE" then
    check_issue_sigs fileh currenteditors
  else if filetype = "ARTICLE" then
    check_article_sigs fileh;
  begin
    match !signwithprivkey with
    | Some(k) ->
	begin
	  if filetype = "ISSUE" then
	    begin
	      let _ = big_int_of_hex fileh in
	      exit 1
	    end
	  else if filetype = "ARTICLE" then
	    begin
	      let _ = big_int_of_hex fileh in
	      exit 1
	    end
	  else
	    raise (Failure "first line of file should be ISSUE or ARTICLE")
	end
    | None ->
	if !sign then (*** sign with all relevant known keys ***)
	  begin
	    if filetype = "ISSUE" then
	      begin
		let _ = big_int_of_hex fileh in
		List.iter (fun (sg,sgs) -> Printf.printf "SIG:%s\n" sgs) !sgns;
		Printf.printf "%s\n" bs;
	      end
	    else if filetype = "ARTICLE" then
	      begin
		let _ = big_int_of_hex fileh in
		List.iter (fun (sg,sgs) -> Printf.printf "SIG:%s\n" sgs) !sgns;
		Printf.printf "%s\n" bs;
	      end
	    else
	      raise (Failure "first line of file should be ISSUE or ARTICLE")
	  end
  end;
  if !latex then
    begin
      if filetype = "ARTICLE" then
	begin
	  let mostothercreditl = ref [] in
	  let someothercreditl = ref [] in
	  Hashtbl.iter
	    (fun a n ->
	      if 2 * n >= !itemcount then
		mostothercreditl := a::!mostothercreditl
	      else
		someothercreditl := a::!someothercreditl)
	    othercreditnymh;
	  begin
	    if not (!mostothercreditl = [] && !someothercreditl = []) then
	      begin
		Printf.fprintf !latexc "\n\n{\\bf{Acknowledgements.}}";
		let rec nyml x y z al =
		  match al with
		  | [] -> Printf.fprintf !latexc "\\\\ and %s %s. %s.\n" x (String.sub y 0 1) z
		  | ((x2,y2,z2)::ar) -> Printf.fprintf !latexc ",\\\\ %s %s. %s" x (String.sub y 0 1) z; nyml x2 y2 z2 ar
		in
		begin
		  match !mostothercreditl with
		  | [] -> ()
		  | [(x,y,z)] ->
		      Printf.fprintf !latexc " Most of the content in this article is based on work by\\\\%s %s. %s.\n" x (String.sub y 0 1) z
		  | ((x,y,z)::(x2,y2,z2)::ar) ->
		      Printf.fprintf !latexc " Most of the content in this article is based on work by\\\\%s %s. %s" x (String.sub y 0 1) z;
		      nyml x2 y2 z2 ar
		end;
		begin
		  match !someothercreditl with
		  | [] -> ()
		  | [(x,y,z)] ->
		      Printf.fprintf !latexc " Some of the content in this article is based on work by\\\\%s %s. %s.\n" x (String.sub y 0 1) z
		  | ((x,y,z)::(x2,y2,z2)::ar) ->
		      Printf.fprintf !latexc " Some of the content in this article is based on work by\\\\%s %s. %s" x (String.sub y 0 1) z;
		      nyml x2 y2 z2 ar
		end;
		Printf.fprintf !latexc " More detailed credit for each item is available in the formal source for this article.\n"
	      end;
	  end;
	  Printf.fprintf !latexc "\n\\bibliographystyle{plain}\n\\bibliography{aim";
	  List.iter
	    (fun bib ->
	      if not (bib = !currentfileh58) then (*** it should not even be on the articlerefs list in this case, but in case it is do not allow an article to reference itself ***)
		Printf.fprintf !latexc ",%s" bib)
	    !articlerefs;
	  Printf.fprintf !latexc "}\n"
	end;
      Printf.fprintf !latexc "\n\\end{document}\n";
      close_out !latexc;
      writedraftbibfile !currentfileh58;
      Printf.printf "pdflatex %s\n" !currentfiletitle;
      Printf.printf "bibtex %s\n" !currentfiletitle;
      Printf.printf "pdflatex %s\n" !currentfiletitle;
    end;
  if !ensurecompletelysigned || !publish then
    if filetype = "ISSUE" then
      begin
	let eranksum = List.fold_left (fun s (r,_,_) -> s + r) 0 !editorssigned in
	if eranksum < currentquorum then
	  begin
	    Printf.printf "A quorum of editors has not yet signed: have %d but need %d.\n" eranksum currentquorum;
	    exit 1
	  end;
      end
    else if filetype = "ARTICLE" then
      if !authpubkeyslefttosign = [] then
	begin
	end
      else
	begin
	  Printf.printf "Not completely signed. Left to sign are:\n";
	  (**
	  List.iter
	    (fun (_,(a1,a2,a3)) -> Printf.printf "%s %s %s\n" a1 a2 a3)
	    !authpubkeyslefttosign;
**)
	  exit 1
	end;;

try
  main();
  exit 0
with e ->
  Printf.printf "%s\n" (Printexc.to_string e);
  exit 1
