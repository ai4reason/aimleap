type tm =
| Var of int
| Skol of string
| Prim of string * tm list
| Abbrev of string * tm list

val skol_name : (string,string) Hashtbl.t
val abbr_name : (string,string) Hashtbl.t

val dual_tm : tm -> (string,tm list -> tm) Hashtbl.t -> (string,tm list -> tm) Hashtbl.t -> tm
val tm_str : tm -> string
val tm_str_1 : tm -> string
val tm_root : tm -> string
val tm_subst : tm array -> tm -> tm
val tm_frees : tm -> int list

val tm_var_upperbd : tm -> int

val send_tm : out_channel -> tm -> unit
val rec_tm : in_channel -> tm

type lit =
  | Eq of tm * tm
  | NEq of tm * tm

val lit_negate : lit -> lit
val lit_str : lit -> string

val lit_subst : tm array -> lit -> lit

type clause = lit list

val clause_subst : tm array -> clause -> clause

val clause_root : clause -> string

val clause_with_root : string -> clause

val clause_frees : clause -> int list
val clause_var_upperbd : clause -> int

val push_tm_frees : int -> tm -> tm
val push_lit_frees : int -> lit -> lit
val push_clause_frees : int -> clause -> clause
val norm_clause_frees : clause -> clause * int * tm array

val negate_clause : clause -> lit list
val clause_str : clause -> string

val okify_tm : tm -> tm array -> int -> tm * int

val okify_clause : clause -> tm array -> int -> clause * int

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

val output_atree : out_channel -> atree -> unit
val output_ltree : out_channel -> ltree -> unit
val ltree_to_atree : ltree -> atree
      
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

val latexify_mathname : string -> string
val ltree_has_a_linebreak_p : ltree -> bool
val ltree_to_latex_math : out_channel -> ltree -> unit
val ltree_to_latex_matharray : int -> out_channel -> ltree -> unit
val ltree_to_latex_matharray_top : bool -> int -> out_channel -> ltree -> unit
val ltree_to_latex_math_or_matharray_top : bool -> out_channel -> ltree -> unit
val ltree_to_latex_matharray_septopeqn : out_channel -> ltree -> unit
val ltree_to_html_math : out_channel -> (string -> string) -> ltree -> unit
val ltree_to_html_math_septopeqn : out_channel -> (string -> string) -> ltree -> unit

type loopimpl = int * int array array * int array array * int array array

val clause_valid_p : loopimpl -> clause -> bool
val pretty_loop_to_loopspec : string list list -> loopimpl * string

val delta_normalize_clause : clause -> ((string,int) Hashtbl.t * (string,tm) Hashtbl.t) -> clause

val tm_ivy : tm -> string
val lit_ivy : lit -> string
val clause_ivy : clause -> string
