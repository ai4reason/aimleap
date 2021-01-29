open Syntax

let valid_aimnym_p _ = false;;

let lineno : int ref = ref 1
let charno : int ref = ref 0

(*** Assume no newlines ***)
let update_char_pos s =
  charno := !charno + String.length s

(*** May have newlines ***)
let rec update_pos_rec s i =
  try
    let i = String.index_from s (i + 1) '\n' in
    incr lineno;
    charno := 0;
    update_pos_rec s i
  with Not_found ->
    charno := String.length s - i

let update_pos s =
  try
    let i = String.index s '\n' in
    incr lineno;
    update_pos_rec s i
  with Not_found ->
    update_char_pos s

exception ParsingError of string * int * int * int * int

type token =
  | STRING of string
  | NAM of string
  | LPAREN
  | RPAREN
  | COMMA
  | COLON
  | DARR
  | DEQ
  | BY
  | IN
  | MEM
  | SUBEQ
  | VBAR
  | LCBRACE
  | RCBRACE
  | LBRACK
  | RBRACK
  | AT
  | IF
  | THEN
  | ELSE
  | SEMICOLON
  | BACKSLASH
  | DOLLAR
  | DOT
  | OPENCOM
  | CLOSECOM
  | TITLE
  | ADMITTED
  | ADMIT
  | SECTION
  | END
  | LETDEC
  | VAR
  | HYP
  | PARAM
  | AXIOM
  | DEF
  | THEOREM of string
  | CONJECTURE
  | SUBPFBEGIN
  | SUBPFEND
  | LET
  | ASSUME
  | CASES
  | APPLY
  | CLAIM
  | PROVE
  | EXACT
  | QED
  | TEXT
  | SPECCOMM of string

let tok_to_str tok =
  match tok with
  | STRING(x) -> "STR:" ^ x
  | NAM(x) -> "NAM:" ^ x
  | LPAREN -> "("
  | RPAREN -> ")"
  | COMMA -> ","
  | COLON -> ":"
  | DARR -> "=>"
  | DEQ -> ":="
  | BY -> "by"
  | IN -> "in"
  | MEM -> ":e"
  | SUBEQ -> "c="
  | VBAR -> "|"
  | LCBRACE -> "{"
  | RCBRACE -> "}"
  | LBRACK -> "["
  | RBRACK -> "]"
  | AT -> "at"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | SEMICOLON -> ";"
  | BACKSLASH -> "\\"
  | DOLLAR -> "$"
  | DOT -> "."
  | OPENCOM -> "(*"
  | CLOSECOM -> "*)"
  | TITLE -> "Title"
  | ADMITTED -> "Admitted"
  | ADMIT -> "admit"
  | SECTION -> "Section"
  | END -> "End"
  | LETDEC -> "Let"
  | VAR -> "Variable"
  | HYP -> "Hypothesis"
  | PARAM -> "Parameter"
  | AXIOM -> "Axiom"
  | DEF -> "Definition"
  | THEOREM(x) -> "Theorem:" ^ x
  | CONJECTURE -> "Conjecture"
  | SUBPFBEGIN -> "begin"
  | SUBPFEND -> "end"
  | LET -> "let"
  | ASSUME -> "assume"
  | CASES -> "cases"
  | APPLY -> "apply"
  | CLAIM -> "claim"
  | PROVE -> "prove"
  | EXACT -> "exact"
  | QED -> "Qed"
  | TEXT -> "TEXT"
  | SPECCOMM(x) -> "SPECCOMM:" ^ x

let rec whole_of_string x i l n = (5,5)

let rec dec_of_string x i l n e = (5,5,5)

let rec e_of_string x i l n e = (5,5,5)

let rec digit_substring x i l =
  if i < l then
    match x.[i] with
    | '0' -> let (j,r) = digit_substring x (i+1) l in (j,"0" ^ r)
    | '1' -> let (j,r) = digit_substring x (i+1) l in (j,"1" ^ r)
    | '2' -> let (j,r) = digit_substring x (i+1) l in (j,"2" ^ r)
    | '3' -> let (j,r) = digit_substring x (i+1) l in (j,"3" ^ r)
    | '4' -> let (j,r) = digit_substring x (i+1) l in (j,"4" ^ r)
    | '5' -> let (j,r) = digit_substring x (i+1) l in (j,"5" ^ r)
    | '6' -> let (j,r) = digit_substring x (i+1) l in (j,"6" ^ r)
    | '7' -> let (j,r) = digit_substring x (i+1) l in (j,"7" ^ r)
    | '8' -> let (j,r) = digit_substring x (i+1) l in (j,"8" ^ r)
    | '9' -> let (j,r) = digit_substring x (i+1) l in (j,"9" ^ r)
    | _ -> (i,"")
  else
    (i,"")

let int_substring x i l =
  if i < l then
    if x.[0] = '-' then
      let (j,r) = digit_substring x (i+1) l in
      (j,"-" ^ r)
    else
      digit_substring x i l
  else
    digit_substring x i l

type parseenv = string -> int option * (int * picase) option * token option

let proj_name x =
  let l = String.length x in
  if l > 1 then
    if x.[0] = '_' then
      let (i,n) = (5,5) in
      if i < l then
	raise Not_found
      else
	5
    else
      raise Not_found
  else
    raise Not_found

let prefixpriorities : (int,unit) Hashtbl.t = Hashtbl.create 100
let disallowedprefixpriorities : (int,unit) Hashtbl.t = Hashtbl.create 100
let rightinfixpriorities : (int,unit) Hashtbl.t = Hashtbl.create 100
let disallowedrightinfixpriorities : (int,unit) Hashtbl.t = Hashtbl.create 100
let infixsem : (string,atree) Hashtbl.t = Hashtbl.create 100
let postfixsem : (string,atree) Hashtbl.t = Hashtbl.create 100
let prefixsem : (string,atree) Hashtbl.t = Hashtbl.create 100
let bindersem : (string,bool * atree * atree option) Hashtbl.t = Hashtbl.create 100

let penv_preop : (string,int) Hashtbl.t = Hashtbl.create 100
let penv_postinfop : (string,int * picase) Hashtbl.t = Hashtbl.create 100
let penv_binder : (string,token) Hashtbl.t = Hashtbl.create 100;;

Hashtbl.add penv_binder "fun" DARR;;
Hashtbl.add penv_binder "forall" COMMA;;
Hashtbl.add penv_postinfop "->" (800,InfixRight);;
Hashtbl.add penv_postinfop "|" (780,InfixRight);;
Hashtbl.add penv_postinfop "&" (760,InfixRight);;
Hashtbl.add penv_postinfop "=" (700,InfixNone);;
Hashtbl.add penv_postinfop "!=" (700,InfixNone);;
Hashtbl.add penv_postinfop "*" (600,InfixNone);;
Hashtbl.add penv_postinfop "\\" (600,InfixNone);;
Hashtbl.add penv_postinfop "/" (600,InfixNone);;
Hashtbl.add penv_postinfop "\\\\" (500,Postfix);;
Hashtbl.add rightinfixpriorities 800 ();;
Hashtbl.add disallowedprefixpriorities 800 ();;

let penv : parseenv =
  fun x ->
    let p =
      try
	Some(Hashtbl.find penv_preop x)
      with Not_found -> None
    in
    let q =
      try
	let _ = proj_name x in (*** _0, _1, _2, etc. are the names of postfix operators for projections of metatuples ***)
	Some(1,Postfix)
      with Not_found ->
	try
	  Some(Hashtbl.find penv_postinfop x)
	with Not_found -> None
    in
    let r =
      try
	Some(Hashtbl.find penv_binder x)
      with Not_found -> None
    in
    (p,q,r)

type tokenstream =
  | TokStrBuff of token * tokenstream
  | TokStrRest of (Lexing.lexbuf -> token) * Lexing.lexbuf

let destr_ts (tl : tokenstream) : token * tokenstream =
  match tl with
  | TokStrBuff(tok,tr) -> (tok,tr)
  | TokStrRest(lf,lb) -> (lf lb,TokStrRest(lf,lb))

(*** parse_Comma_Names was not part of the formalized Coq version and is included to handle let [x,...,y] := S in S. - Feb 25 2014 ***)
let rec parse_Comma_Names (tl : tokenstream) : string list * tokenstream =
  match destr_ts tl with
  | (COMMA,tr) ->
      begin
	match destr_ts tr with
	| (NAM x,ts) ->
	    begin
	      match penv x with
	      | (None,None,None) ->
		  let (yl,tu) = parse_Comma_Names ts in
		  (x::yl,tu)
	      | _ -> raise (ParsingError("The name " ^ x ^ " cannot be used here since it has a special meaning.",!lineno,!charno,!lineno,!charno))
	    end
	| _ -> raise (ParsingError("Expected a name here.",!lineno,!charno,!lineno,!charno))
      end
  | (tok,tr) -> ([],TokStrBuff(tok,tr))

let rec parse_comma_nyms (tl : tokenstream) : (string * string * string) list * tokenstream =
  match destr_ts tl with
  | (COMMA,tr) ->
      begin
	match destr_ts tr with
	| (NAM x,ts) ->
	    begin
	      match destr_ts ts with
	      | (NAM y,tv) ->
		  begin
		    match destr_ts tv with
		    | (NAM c,tw) ->
			if not (valid_aimnym_p (x,y,c)) then raise (ParsingError((x ^ " is not a valid AIM nym"),!lineno,!charno,!lineno,!charno));
			let (yl,tu) = parse_comma_nyms ts in
			((x,y,c)::yl,tu)
		    | _ -> raise (ParsingError("Expected an AIM nym here.",!lineno,!charno,!lineno,!charno))
		  end
	      | _ -> raise (ParsingError("Expected an AIM nym here.",!lineno,!charno,!lineno,!charno))
	    end
	| _ -> raise (ParsingError("Expected an AIM nym here.",!lineno,!charno,!lineno,!charno))
      end
  | (tok,tr) -> ([],TokStrBuff(tok,tr))

let parse_nyms tl =
  parse_comma_nyms (TokStrBuff(COMMA,tl))

let rec parse_Names (tl : tokenstream) : string list * tokenstream =
  match destr_ts tl with
  | (NAM x,tr) ->
      begin
	match penv x with
	| (None,None,None) ->
	    let (yl,tu) = parse_Names tr in
	    (x::yl,tu)
	| _ -> ([],TokStrBuff(NAM x,tr))
      end
  | (tok,tr) ->
      ([],TokStrBuff(tok,tr))

let rec parse_byNames (tl : tokenstream) : string list * tokenstream =
  match destr_ts tl with
  | (NAM x,tr) when not (x = "=") ->
      begin
	match penv x with
	| (None,None,None) ->
	    begin
	      match destr_ts tr with
	      | (COMMA,tw) ->
		  let (yl,tu) = parse_byNames tw in
		  (x::yl,tu)
	      | (tok,tw) ->
		  ([x],TokStrBuff(tok,tw))
	    end
	| _ -> ([],TokStrBuff(NAM x,tr))
      end
  | (tok,tr) ->
      raise (ParsingError((tok_to_str tok ^ " should be a name"),!lineno,!charno,!lineno,!charno))
	
let namorasctokb (tok : token) : bool =
  match tok with
  | NAM _ -> true
  | COLON -> true
  | MEM -> true
  | SUBEQ -> true
  | _ -> false

let parse_S'_Infix q p pr i a tl tr fS' fS =
  if q <= p then
    (a,tl)
  else
    let (b,ts) = fS pr tr in
    fS' q (InfoL(i,a,b)) ts

let rec parse_S'_ (q : int) (a : ltree) (tl : tokenstream) : ltree * tokenstream =
  if q = 0 then
    (a,tl)
  else
    let (tok,tr) = destr_ts tl in
    let tl = TokStrBuff(tok,tr) in (*** Put it back together in case we need it below ***)
    match tok with
    | NAM x ->
	begin
	  match penv x with
	  | (_,Some(p,Postfix),_) ->
	      if q <= p then
		(a,tl)
	      else if x = "\\\\" then
		parse_S'_ q (Linebreak(a)) tr
	      else
		raise (Failure("general postfix not allowed: " ^ x))
	  | (_,Some(p,InfixNone),_) ->
	      parse_S'_Infix q p p (InfNam x) a tl tr parse_S'_ parse_S_
	  | (_,Some(p,InfixLeft),_) ->
	      parse_S'_Infix q p p (InfNam x) a tl tr parse_S'_ parse_S_
	  | (_,Some(p,InfixRight),_) ->
	      parse_S'_Infix q p (p+1) (InfNam x) a tl tr parse_S'_ parse_S_
	  | (None,None,None) ->
	      parse_S'_ q (ImplopL(a,NaL x)) tr
	  | (_,_,_) -> (a,tl)
	end
    | LPAREN ->
	let lpli = !lineno in
	let lpch = !charno in
	begin
	  let (b,ts) = parse_S_ 1023 tr in
	  let (cl,tv) = parse_N ts in
	  match destr_ts tv with
	  | (RPAREN,tu) -> parse_S'_ q (ImplopL(a,ParenL(b,cl))) tu
	  | _ -> raise (ParsingError("Unmatched (",lpli,lpch,!lineno,!charno))
	end
    | _ -> (a,tl)
and parse_S_ (q : int) (tl : tokenstream) : ltree * tokenstream =
  match destr_ts tl with
  | (NAM x,tr) ->
      begin
	match penv x with
	| (None,None,None) -> (*** Ordinary Name ***)
	    parse_S'_ q (NaL x) tr
	| (Some p,_,_) -> (*** Prefix Operator ***)
	    if q <= p
	    then raise(ParsingError("Prefix operator " ^ x ^ " needs parenthesis here",!lineno,!charno,!lineno,!charno))
	    else
	      let (_,_) = parse_S_ (p+1) tr in
	      raise (Failure "no prefix ops in AIM")
	| (_,_,Some mtok) -> (*** Binder ***)
	    raise (Failure "no binders in AIM")
	| _ -> (*** Infix operator: error ***)
	    raise (ParsingError("Unexpected infix operator " ^ x,!lineno,!charno,!lineno,!charno))
      end
  | (LET,tr0) ->
      let li = !lineno in
      let ch = !charno in
      raise (ParsingError("no let in AIM",li,ch,li,ch))
  | (LPAREN,tr) ->
      let lpli = !lineno in
      let lpch = !charno in
      begin
	let (a,ts) = parse_S_ 1023 tr in
	let (bl,tu0) = parse_N ts in
	match destr_ts tu0 with
	| (RPAREN,tu) -> parse_S'_ q (ParenL(a,bl)) tu
	| _ -> raise (ParsingError("Unmatched (",lpli,lpch,!lineno,!charno))
      end
  | _ -> raise (ParsingError("Unwritten case",!lineno,!charno,!lineno,!charno))
and parse_N (tl : tokenstream) : ltree list * tokenstream =
  match destr_ts tl with
  | (COMMA,tr) ->
      let (a,ts) = parse_S_ 1023 tr in
      let (bl,tu) = parse_N ts in
      (a::bl,tu)
  | (tok,tr) -> ([],TokStrBuff(tok,tr))

let read_name_ts tl =
  match destr_ts tl with
  | (NAM x,tr) -> (x,tr)
  | _ -> raise (ParsingError("Name expected",!lineno,!charno,!lineno,!charno))

let read_string_ts tl =
  match destr_ts tl with
  | (STRING x,tr) -> (x,tr)
  | _ -> raise (ParsingError("String (in double quotes) expected",!lineno,!charno,!lineno,!charno))

let rec read_strings_ts tl =
  match destr_ts tl with
  | (STRING x,tr) ->
      let (xl,ts) = read_strings_ts tr in
      (x::xl,ts)
  | (tok,tr) -> ([],TokStrBuff(tok,tr))

let rec read_expected_ts tokl tl =
  match tokl with
  | (tok::tokr) ->
      begin
	match destr_ts tl with
	| (tok1,tr) when tok = tok1 -> read_expected_ts tokr tr
	| _ -> raise (ParsingError("Syntax error: unexpected token, expected " ^ (tok_to_str tok),!lineno,!charno,!lineno,!charno))
      end
  | [] -> tl

let rec parse_matrix tl =
  match destr_ts tl with
  | (NAM("|"),tr) ->
      let (xl,ts) = parse_Names tr in
      let tu = read_expected_ts [NAM("|")] ts in
      let (lr,tv) = parse_matrix tu in
      (xl::lr,tv)
  | (tok,tr) -> ([],TokStrBuff(tok,tr))

let parse_ltree tl =
  let (a,tr) = parse_S_ 1023 tl in
  (a,tr)

type indexitem =
  | IndexTm of string * ltree
  | IndexKnown of string

let parse_indexitem tl =
  let (x,tr) = read_name_ts tl in
  if x = "Known" then
    let (y,ts) = read_name_ts tr in
    let tu = read_expected_ts [DOT] ts in
    (IndexKnown(y),tu)
  else
    let (a,ts) = parse_ltree tr in
    let tu = read_expected_ts [DOT] ts in
    (IndexTm(x,a),tu)

let parse_optional_label tr =
  match destr_ts tr with
  | (NAM(x),ts) ->
      begin
	match destr_ts ts with
	| (COLON,tv) -> (Some(x),tv)
	| (tok2,tv) -> (None,TokStrBuff(NAM(x),TokStrBuff(tok2,tv)))
      end
  | (tok1,ts) -> (None,TokStrBuff(tok1,ts))

let parse_optional_nyms tr =
  let li = !lineno in
  let ch = !charno in
  match destr_ts tr with
  | (LBRACK,ts) ->
      begin
	match destr_ts ts with
	| (NAM("+"),tw) ->
	    begin
	      let (yl,tu) = parse_nyms tw in
	      match destr_ts tu with
	      | (RBRACK,tv) -> (Some(true,yl),tv)
	      | _ -> raise (ParsingError("Invalid list of nyms",li,ch,!lineno,!charno))
	    end
	| (tok,tw) ->
	    let (yl,tu) = parse_nyms (TokStrBuff(tok,tw)) in
	    match destr_ts tu with
	    | (RBRACK,tv) -> (Some(false,yl),tv)
	    | _ -> raise (ParsingError("Invalid list of nyms",li,ch,!lineno,!charno))
      end
  | (tok0,ts) -> (None,TokStrBuff(tok0,ts))

let parse_editor_rank tl =
  let li = !lineno in
  let ch = !charno in
  match destr_ts tl with
  | (NAM("Chief"),tr) -> (4,tr)
  | (NAM("Full"),tr) -> (3,tr)
  | (NAM("Associate"),tr) -> (2,tr)
  | (NAM("Assistant"),tr) -> (1,tr)
  | _ -> raise (ParsingError("Expected an editor rank",li,ch,!lineno,!charno))
  
let parse_docitem tl =
  let li = !lineno in
  let ch = !charno in
  match destr_ts tl with
  | (NAM(x),tr) when x = "Author" || x = "Authors" ->
      begin
	let (nyml,tu) = parse_nyms tr in
	let tv = read_expected_ts [DOT] tu in
	(Author(nyml),tv)
      end
  | (NAM(x) as tok,tr) when x = "Chief" || x = "Full" || x = "Associate" || x = "Assistant" ->
      begin
	let (erank,tw) = parse_editor_rank (TokStrBuff(tok,tr)) in
	match destr_ts tw with
	| (NAM(y),ts) when y = "Editor" ->
	    let (nyml,tu) = parse_nyms tw in
	    if not (List.length nyml = 1) then raise (ParsingError("Editor should Editors",li,ch,!lineno,!charno));
	    let tv = read_expected_ts [DOT] tu in
	    (Editors(erank,nyml),tv)
	| (NAM(y),ts) when y = "Editors" ->
	    let (nyml,tu) = parse_nyms tw in
	    if List.length nyml = 1 then raise (ParsingError("Editors should Editor",li,ch,!lineno,!charno));
	    let tv = read_expected_ts [DOT] tu in
	    (Editors(erank,nyml),tv)
	| _ -> raise (ParsingError("Expected Editor or Editors",li,ch,!lineno,!charno));
      end
  | (NAM(x),tr) when x = "AddEditor" || x = "AddEditors" ->
      begin
	let (erank,tw) = parse_editor_rank tr in
	let (nyml,tu) = parse_nyms tw in
	let tv = read_expected_ts [DOT] tu in
	(AddEditors(erank,nyml),tv)
      end
  | (NAM(x),tr) when x = "RemEditor" || x = "RemEditors" ->
      begin
	let (nyml,tu) = parse_nyms tr in
	let tv = read_expected_ts [DOT] tu in
	(RemEditors(nyml),tv)
      end
  | (NAM("LastIssueOfVolume"),tr) ->
      begin
	let tv = read_expected_ts [DOT] tr in
	(LastIssueOfVolume,tv)
      end
  | (NAM("NextEditorQuorum"),tr) ->
      begin
	match destr_ts tr with
	| (NAM(x),tw) ->
	    begin
	      try
		let q = int_of_string x in
		let tv = read_expected_ts [DOT] tw in
		(EditorQuorum(q),tv)
	      with _ ->
		raise (ParsingError("Expected a number to set quorum",li,ch,!lineno,!charno))
	    end
	| (tok,_) ->
	    raise (ParsingError("Expected a number to set quorum",li,ch,!lineno,!charno))
      end
  | (NAM("PreviousIssue"),tr) ->
      let (x,tv) = read_name_ts tr in
      let tw = read_expected_ts [DOT] tv in
      (PreviousIssue(x),tw)
  | (NAM("Article"),tr) ->
      let (x,tv) = read_name_ts tr in
      let tw = read_expected_ts [DOT] tv in
      (Article(x),tw)
  | (CLAIM,tr) ->
      begin
	let (nyml,tu) = parse_optional_nyms tr in
	let (label,tr) = parse_optional_label tu in
	let (a,tv) = parse_ltree tr in
	match destr_ts tv with
	| (DOT,tw) -> (ClaimItem(nyml,label,a,None),tw)
	| (BY,tw) ->
	    let (p,tu) = parse_ltree tw in
	    let (pl,ts) = parse_N tu in
	    let tx = read_expected_ts [DOT] ts in
	    (ClaimItem(nyml,label,a,Some(p::pl)),tx)
	| _ ->
	    raise (ParsingError("expected . or by",li,ch,!lineno,!charno))
      end
  | (NAM("Claim"),tr) ->
      begin
	let (nyml,tu) = parse_optional_nyms tr in
	let (label,tr) = parse_optional_label tu in
	let (a,tv) = parse_ltree tr in
	match destr_ts tv with
	| (DOT,tw) -> (ClaimItem(nyml,label,a,None),tw)
	| (BY,tw) ->
	    let (p,tu) = parse_ltree tw in
	    let (pl,ts) = parse_N tu in
	    let tx = read_expected_ts [DOT] ts in
	    (ClaimItem(nyml,label,a,Some(p::pl)),tx)
	| _ ->
	    raise (ParsingError("expected . or by",li,ch,!lineno,!charno))
      end
  | (NAM("Nontheorem"),tr) ->
      begin
	let (nyml,tu) = parse_optional_nyms tr in
	let (label,tr) = parse_optional_label tu in
	let (a,tv) = parse_ltree tr in
	match destr_ts tv with
	| (BY,tw) ->
	    let (byl,tv) = parse_Comma_Names (TokStrBuff(COMMA,tw)) in
	    let tx = read_expected_ts [DOT] tv in
	    (NonThmDecl(nyml,label,a,byl),tx)
	| _ ->
	    raise (ParsingError("expected by",li,ch,!lineno,!charno))
      end
  | (PARAM,tr) ->
      begin
	let (x,ts) = read_name_ts tr in
	let (arity,tz) = read_name_ts ts in
	try
	  let arity = int_of_string arity in
	  let tu = read_expected_ts [DEQ] tz in
	  let (r,tv) = read_name_ts tu in
	  let tw = read_expected_ts [DOT] tv in
	  (ParamDecl(x,arity,r),tw)
	with _ ->
	  raise (ParsingError("expected arity for " ^ x,li,ch,!lineno,!charno))
      end
  | (DEF,tr) ->
      begin
	let (nyml,tu) = parse_optional_nyms tr in
	let (x,ts) = read_name_ts tu in
	match destr_ts ts with
	| (LPAREN,tu) ->
	    let (yl,tv) = parse_Comma_Names (TokStrBuff(COMMA,tu)) in
	    let tw = read_expected_ts [RPAREN;DEQ] tv in
	    let (b,tx) = parse_ltree tw in
	    let ty = read_expected_ts [DOT] tx in
	    (DefDecl(nyml,x,yl,b),ty)
	| (DEQ,tu) ->
	    let (b,tv) = parse_ltree tu in
	    let tw = read_expected_ts [DOT] tv in
	    (DefDecl(nyml,x,[],b),tw)
	| _ -> raise (ParsingError("Syntax error in definition declaration",li,ch,!lineno,!charno))
      end
  | (SECTION,tr) ->
      let (x,ts) = read_name_ts tr in
      let tu = read_expected_ts [DOT] ts in
      (Section(x),tu)
  | (END,tr) ->
      let (x,ts) = read_name_ts tr in
      let tu = read_expected_ts [DOT] ts in
      (End(x),tu)
  | (HYP,tr) ->
      let (x,ts) = read_name_ts tr in
      let tu = read_expected_ts [COLON] ts in
      let (a,tv) = parse_ltree tu in
      let tw = read_expected_ts [DOT] tv in
      (HypDecl(x,a),tw)
  | (AXIOM,tr) ->
      let (x,ts) = read_name_ts tr in
      let tu = read_expected_ts [COLON] ts in
      let (a,tv) = parse_ltree tu in
      let tw = read_expected_ts [DOT] tv in
      (AxDecl(x,a),tw)
  | (THEOREM(c),tr) ->
      let (nyml,tu) = parse_optional_nyms tr in
      let (x,ts) = read_name_ts tu in
      let tu = read_expected_ts [COLON] ts in
      let (a,tv) = parse_ltree tu in
      let tw = read_expected_ts [DOT] tv in
      (ThmDecl(c,nyml,x,a),tw)
  | (NAM("Known"),tr) ->
      let (x,ts) = read_name_ts tr in
      let tu = read_expected_ts [COLON] ts in
      let (a,tv) = parse_ltree tu in
      let tw = read_expected_ts [DOT] tv in
      (KnownDecl(x,a),tw)
  | (CONJECTURE,tr) ->
      let (nyml,tu) = parse_optional_nyms tr in
      let (label,ts) = parse_optional_label tu in
      let (a,tv) = parse_ltree ts in
      let tw = read_expected_ts [DOT] tv in
      (ConjDecl(nyml,label,a),tw)
  | (NAM("Example"),tr) ->
      let (nyml,tu) = parse_optional_nyms tr in
      let (label,ts) = parse_optional_label tu in
      let (lll,tv) = parse_matrix ts in
      let tw = read_expected_ts [DOT] tv in
      (LoopDecl(nyml,label,lll),tw)
  | _ -> raise (ParsingError("Expected a Document Item",li,ch,!lineno,!charno))

let rec read_case tl =
  let (x,tr) = read_name_ts tl in
  let ts = read_expected_ts [COLON] tr in
  let (b,tu) = parse_ltree ts in
  match destr_ts tu with
  | (COMMA,tv) ->
      let (xbl,tok,tw) = read_case tv in
      ((x,b)::xbl,tok,tw)
  | (tok,tv) ->
      ([],tok,tv)

let rec read_cases tl =
  let (xbl,tok,tr) = read_case tl in
  match tok with
  | VBAR ->
      let (xbll,tu) = read_cases tr in
      (xbl::xbll,tu)
  | DOT ->
      ([],tr)
  | _ -> raise (ParsingError("Expected | or .",!lineno,!charno,!lineno,!charno))

let parse_optional_by tl =
  match destr_ts tl with
  | (BY,tr) ->
      let (yl,ts) = parse_byNames tr in
      (Some(yl),ts)
  | (tok,tr) -> (None,TokStrBuff(tok,tr))

let parse_cases li ch tl thn =
  let (a,ts) = parse_ltree tl in
  let (byl,tv) = parse_optional_by ts in
  let tu = read_expected_ts [DOT] tv in
  (CasesTac(a,thn,byl),tu)

let parse_havish li ch tl thusish thn =
  let (label,tr) = parse_optional_label tl in
  let (a,tv) = parse_ltree tr in
  let (byl,tu) = parse_optional_by tv in
  begin
    match destr_ts tu with
    | (DOT,tw) ->
	if thusish then
	  (ThusTac(label,a,thn,byl),tw)
	else
	  (HaveTac(label,a,thn,byl),tw)
    | (NAM("=") as tok,tw) ->
	if thusish then
	  raise (Failure("Equation chain cannot be combined with thus/hence"))
	else
	  (EqChainTac(label,a,thn,byl),TokStrBuff(tok,tw))
    | (tok,_) ->
	raise (ParsingError("expected . or =",li,ch,!lineno,!charno))
  end

let rec read_rewrite_tac forw x tl pl =
  match destr_ts tl with
  | (DOT,tr) -> (RewriteTac(forw,x,List.rev pl),tr)
  | (NAM(y),tr) ->
      begin
	try
	  read_rewrite_tac forw x tr (int_of_string y::pl)
	with _ -> raise (ParsingError("bad rewrite tac -- non int pos num",!lineno,!charno,!lineno,!charno))
      end
  | _ -> raise (ParsingError("bad rewrite tac -- bad pos info",!lineno,!charno,!lineno,!charno))

let parse_pftacitem tl =
  let li = !lineno in
  let ch = !charno in
  match destr_ts tl with
  | (NAM "-",tr) -> (PfStruct 1,tr)
  | (NAM "+",tr) -> (PfStruct 2,tr)
  | (NAM "*",tr) -> (PfStruct 3,tr)
  | (LCBRACE,tr) -> (PfStruct 4,tr)
  | (RCBRACE,tr) -> (PfStruct 5,tr)
  | (SUBPFBEGIN,tr) -> (PfStruct 4,tr)
  | (SUBPFEND,tr) -> (PfStruct 5,tr)
  | (QED,tr) -> let tu = read_expected_ts [DOT] tr in (Qed,tu)
  | (ADMITTED,tr) -> let tu = read_expected_ts [DOT] tr in (Admitted,tu)
  | (NAM "rewrite",tr) ->
      begin
	match destr_ts tr with
	| (NAM "<-",ts) ->
	    begin
	      match destr_ts ts with
	      | (NAM x,tu) -> read_rewrite_tac false x tu []
	      | _ -> raise (ParsingError("bad rewrite tac",!lineno,!charno,!lineno,!charno))
	    end
	| (NAM x,ts) -> read_rewrite_tac true x ts []
	| _ -> raise (ParsingError("bad rewrite tac",!lineno,!charno,!lineno,!charno))
      end
  | (ADMIT,tr) -> let tu = read_expected_ts [DOT] tr in (Admit,tu)
  | (PROVE,tr) ->
      let (a,tv) = parse_ltree tr in
      let tw = read_expected_ts [DOT] tv in
      (ProveTac(a),tw)
  | (ASSUME,tr) ->
      let (label,tr) = parse_optional_label tr in
      let (a,tv) = parse_ltree tr in
      let tw = read_expected_ts [DOT] tv in
      (AssumeTac(label,a),tw)
  | (NAM "have",tr) -> parse_havish li ch tr false false
  | (THEN,tr) ->
      begin
	match destr_ts tr with
	| (NAM "have",tr) -> parse_havish li ch tr false true
	| (NAM "thus",tr) -> parse_havish li ch tr true true
	| (CASES,tr) -> parse_cases li ch tr true
	| (tok,tr) -> parse_havish li ch (TokStrBuff(tok,tr)) false true
      end
  | (NAM "thus",tr) -> parse_havish li ch tr true false
  | (NAM "hence",tr) -> parse_havish li ch tr true true
  | (NAM "set",tr) ->
      let (x,ts) = read_name_ts tr in
      begin
	match destr_ts ts with
	| (COLON,tu) ->
	    let (a,tv) = parse_ltree tu in
	    let tw = read_expected_ts [DEQ] tv in
	    let (b,tx) = parse_ltree tw in
	    let ty = read_expected_ts [DOT] tx in
	    (SetTac(x,Some(a),b),ty)
	| (DEQ,tu) ->
	    let (b,tv) = parse_ltree tu in
	    let tw = read_expected_ts [DOT] tv in
	    (SetTac(x,None,b),tw)
	| _ ->
	    raise (ParsingError("Expected set <name> [: <tp>] := <tm>",li,ch,!lineno,!charno))
      end
  | (CASES,tr) -> parse_cases li ch tr false
  | (CLAIM,tr) ->
      let (x,ts) = read_name_ts tr in
      let tu = read_expected_ts [COLON] ts in
      let (a,tv) = parse_ltree tu in
      let tw = read_expected_ts [DOT] tv in
      (ClaimTac(x,a),tw)
  | (tok,tr) -> parse_havish li ch (TokStrBuff(tok,tr)) false false

let parse_eqpftacitem tl =
  let li = !lineno in
  let ch = !charno in
  match destr_ts tl with
  | (NAM("="),tr) ->
      let (a,tv) = parse_ltree tr in
      let (byl,tu) = parse_optional_by tv in
      (EqLink(a,byl),tu)
  | (DOT,tr) -> (EqDone,tr)
  | _ -> raise (ParsingError("expected . or =",li,ch,!lineno,!charno))
	
(*** Given a popfn for how to undo everything when the current section ends,
     return a new popfn that will also undo what I'm doing here. ***)
let declare_postinfix x a p pic popfn =
  if (p <= 0 || p >= 1000) then
    raise (Failure("Priorities must be an positive integer and < 1000"))
  else if (p = 500 && pic = InfixRight) then
    raise (Failure("Right infix operators cannot have priority 500."))
  else if (Hashtbl.mem prefixpriorities p) then
    raise (Failure(x ^ " cannot be given priority " ^ (string_of_int p) ^ " since a prefix has this priority already."))
  else if (pic = InfixRight && Hashtbl.mem disallowedrightinfixpriorities p) then
    raise (Failure(x ^ " cannot be given priority " ^ (string_of_int p) ^ " since a nonright infix operator or postfix operator has this priority already."))
  else if (pic <> InfixRight && Hashtbl.mem rightinfixpriorities p) then
    raise (Failure(x ^ " cannot be given priority " ^ (string_of_int p) ^ " since a right infix operator has this priority already."))
  else
    let f = ref (fun () -> Hashtbl.remove disallowedprefixpriorities p; popfn ()) in
    begin
      Hashtbl.add penv_postinfop x (p,pic);
      let fnow = !f in
      f := (fun () -> Hashtbl.remove penv_postinfop x; fnow ());
      Hashtbl.add disallowedprefixpriorities p ();
      if (pic = InfixRight) then
	begin
	  Hashtbl.add rightinfixpriorities p ();
	  let fnow = !f in
	  f := (fun () -> Hashtbl.remove rightinfixpriorities p; fnow ())
	end
      else
	begin
	  Hashtbl.add disallowedrightinfixpriorities p ();
	  let fnow = !f in
	  f := (fun () -> Hashtbl.remove disallowedrightinfixpriorities p; fnow ())
	end;
      begin
	match pic with
	| Postfix ->
	    Hashtbl.add postfixsem x a;
	    let fnow = !f in
	    f := (fun () -> Hashtbl.remove postfixsem x; fnow ())
	| _ ->
	    Hashtbl.add infixsem x a;
	    let fnow = !f in
	    f := (fun () -> Hashtbl.remove infixsem x; fnow ())
      end;
      !f
    end

(*** auxiliary function to add parens if needed ***)
let a2l_paren q (a,p) : ltree =
  if p <= q then
    a
  else
    ParenL(a,[])

(***
    convert an atree to an ltree (using penv info);
    return a level to determine where parens are needed
 ***)
let rec atree_to_ltree_level (a:atree) : ltree * int =
  match a with
  | Na(x) -> (NaL(x),0)
  | Info(InfNam x,a,b) -> (*** if binderishp al ... ***)
      begin
	match penv x with
	| (_,Some(p,InfixNone),_) ->
	    let al = a2l_paren p (atree_to_ltree_level a) in
	    let bl = a2l_paren p (atree_to_ltree_level b) in
	    (InfoL(InfNam x,al,bl),p+1)
	| (_,Some(p,InfixLeft),_) ->
	    let al = a2l_paren (p+1) (atree_to_ltree_level a) in
	    let bl = a2l_paren p (atree_to_ltree_level b) in
	    (InfoL(InfNam x,al,bl),p+1)
	| (_,Some(p,InfixRight),_) ->
	    let al = a2l_paren p (atree_to_ltree_level a) in
	    let bl = a2l_paren (p+1) (atree_to_ltree_level b) in
	    (InfoL(InfNam x,al,bl),p+1)
	| (_,_,_) -> raise (Failure ("Undeclared infix operator " ^ x))
      end
  | Implop(a,b) ->
      let al = a2l_paren 1 (atree_to_ltree_level a) in
      let bl = a2l_paren 0 (atree_to_ltree_level b) in
      (ImplopL(al,bl),1)
  | Tuple(a,b,cl) ->
      let (al,_) = atree_to_ltree_level a in
      let (bl,_) = atree_to_ltree_level b in
      (ParenL(al,bl::List.map
		       (fun a ->
			 let (l,_) = atree_to_ltree_level a in
			 l)
		       cl),0)

let atree_to_ltree (a:atree) : ltree =
  let (l,_) = atree_to_ltree_level a in
  l
