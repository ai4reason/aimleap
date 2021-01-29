(* File: lexer.mll *)

{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
| [' ' '\t' '\r']     { incr charno; token lexbuf }     (* skip white space *)
| ['\n']         { incr lineno; charno := 0; token lexbuf }     (* skip white space *)
| ['%'][^'\n']*['\n'] { incr lineno; charno := 0; token lexbuf }
| ['#'][^'\n']*['\n'] { incr lineno; charno := 0; token lexbuf }
| ['(']['*']['*']*[^'*']*['*']+[')'] as lxm { update_pos lxm; token lexbuf }     (* skip comments *)
| ['"'][^'"']*['"'] as lxm           { update_char_pos lxm; STRING(String.sub lxm 1 (String.length lxm - 2)) }
| '('           { incr charno; LPAREN }
| ')'           { incr charno; RPAREN }
| '['           { incr charno; LBRACK }
| ']'           { incr charno; RBRACK }
| '{'           { incr charno; LCBRACE }
| '}'           { incr charno; RCBRACE }
| '.'           { incr charno; DOT }
| ':'           { incr charno; COLON }
| "\\\\"          { incr charno; NAM("\\\\") }
| '\\'          { incr charno; NAM("\\") }
| '/'          { incr charno; NAM("/") }
| '*'          { incr charno; NAM("*") }
| "="           { incr charno; NAM("=") }
| "!=" as lxm          { update_char_pos lxm; NAM("!=") }
| "<>" as lxm           { update_char_pos lxm; NAM("!=") }
| '|'            { incr charno; NAM("|") }
| '&'            { incr charno; NAM("&") }
| ":e" as lxm           { update_char_pos lxm; MEM }
| "/:e" as lxm           { update_char_pos lxm; NAM(lxm) }
| "c=" as lxm           { update_char_pos lxm; SUBEQ }
| "/c=" as lxm           { update_char_pos lxm; NAM(lxm) }
| '~'           { incr charno; NAM("~") }
| '+'           { incr charno; NAM("+") }
| '*'           { incr charno; NAM("*") }
| '^'          { incr charno; NAM("^") }
| '-'           { incr charno; NAM("-") }
| ';'            { incr charno; SEMICOLON }
| ','            { incr charno; COMMA }
| '!'                     { incr charno; NAM("!") }
| "at" as lxm          { update_char_pos lxm; AT }
| "if" as lxm          { update_char_pos lxm; IF }
| "then" as lxm        { update_char_pos lxm; THEN }
| "else" as lxm        { update_char_pos lxm; ELSE }
| "let" as lxm         { update_char_pos lxm; LET }
| "assume" as lxm         { update_char_pos lxm; ASSUME }
| "cases" as lxm         { update_char_pos lxm; CASES }
| "apply" as lxm         { update_char_pos lxm; APPLY }
| "claim" as lxm         { update_char_pos lxm; CLAIM }
| "prove" as lxm         { update_char_pos lxm; PROVE }
| "begin" as lxm         { update_char_pos lxm; SUBPFBEGIN }
| "end" as lxm         { update_char_pos lxm; SUBPFEND }
| ":=" as lxm          { update_char_pos lxm; DEQ }
| "in" as lxm         { update_char_pos lxm; IN }
| "by" as lxm         { update_char_pos lxm; BY }
| "=>" as lxm          { update_char_pos lxm; DARR }
| "exists!" as lxm           { update_char_pos lxm; NAM("exists!") }
| "some" as lxm           { update_char_pos lxm; NAM("some") }
| "/\\" as lxm         { update_char_pos lxm; NAM(lxm) }
| "\\/" as lxm           { update_char_pos lxm; NAM(lxm) }
| "/\\_" as lxm         { update_char_pos lxm; NAM(lxm) }
| "\\/_" as lxm           { update_char_pos lxm; NAM(lxm) }
| "<->" as lxm           { update_char_pos lxm; NAM(lxm) }
| "<=>" as lxm           { update_char_pos lxm; NAM(lxm) }
| "fun" as lxm         { update_char_pos lxm; NAM(lxm) }
| "->" as lxm          { update_char_pos lxm; NAM(lxm) }
| "<-" as lxm          { update_char_pos lxm; NAM(lxm) }
| '>'          { incr charno; NAM(">") }
| '<'          { incr charno; NAM("<") }
| ">=" as lxm          { update_char_pos lxm; NAM(lxm) }
| "<=" as lxm          { update_char_pos lxm; NAM(lxm) }
| "'"          { incr charno; NAM("'") }
| "Section" as lxm         { update_char_pos lxm; SECTION }
| "End" as lxm         { update_char_pos lxm; END }
| "Let" as lxm         { update_char_pos lxm; LETDEC }
| "Variable" as lxm         { update_char_pos lxm; VAR }
| "Hypothesis" as lxm         { update_char_pos lxm; HYP }
| "Parameter" as lxm       { update_char_pos lxm; PARAM }
| "Axiom" as lxm       { update_char_pos lxm; AXIOM }
| "Lemma" as lxm       { update_char_pos lxm; THEOREM(lxm) }
| "Theorem" as lxm       { update_char_pos lxm; THEOREM(lxm) }
| "Corollary" as lxm       { update_char_pos lxm; THEOREM(lxm) }
| "exact" as lxm       { update_char_pos lxm; EXACT }
| "Qed" as lxm       { update_char_pos lxm; QED }
| "Axiom" as lxm       { update_char_pos lxm; AXIOM }
| "Conjecture" as lxm       { update_char_pos lxm; CONJECTURE }
| "Definition" as lxm         { update_char_pos lxm; DEF }
| "Title" as lxm { update_char_pos lxm; TITLE }
| "Admitted" as lxm { update_char_pos lxm; ADMITTED }
| "admit" as lxm { update_char_pos lxm; ADMIT }
| "TEXT" as lxm { update_char_pos lxm; TEXT }
| ['_''0'-'9''a'-'z''A'-'Z']['_''\'''^''0'-'9''a'-'z''A'-'Z']* as lxm { update_char_pos lxm; NAM(lxm) }
| eof            { raise Eof }
