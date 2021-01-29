open Syntax

val lineno : int ref
val charno : int ref
val update_char_pos : string -> unit
val update_pos : string -> unit
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

val infixsem : (string,atree) Hashtbl.t

type parseenv = string -> int option * (int * picase) option * token option

type tokenstream =
  | TokStrBuff of token * tokenstream
  | TokStrRest of (Lexing.lexbuf -> token) * Lexing.lexbuf

val destr_ts : tokenstream -> token * tokenstream

val parse_S_ : int -> tokenstream -> ltree * tokenstream

val parse_docitem : tokenstream -> docitem * tokenstream
val parse_pftacitem : tokenstream -> pftacitem * tokenstream
val parse_eqpftacitem : tokenstream -> eqpftacitem * tokenstream

val atree_to_ltree : atree -> ltree

    
