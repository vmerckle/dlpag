module AST = struct module T = struct
type cname = string
type vname = string

type eoperator = Add | Mult | Max | Min
type foperator = Conj | Disj
type poperator = Seq | U
type roperator = Eq | Neq | Lt | Gt | Leq | Geq
type soperator = Union | Intersect | Setminus

type pure_term = Var of vname | Fun of (cname * pure_term list) | Int of int

type set = Set of (tuple list * vdecls) | Name of callable | List of (soperator * set * set list)
and vdecls = vdecl list
and vdecl = FromSet of (pure_term * set) | Constraint of constraints
and constraints = Relation of (roperator * term * term) | Notin of (term * set)
and term = Fun of (cname * term list) | Int of expr | Var of vname
and tuple = Term of term | Range of (expr * expr)
and expr = Var of vname | Int of int | ListE of (eoperator * expr * expr list) | VarE of (eoperator * vdecls * expr) | Subtract of (expr * expr list)
and callable = cname * term list

type formula = CallF of callable | Top | Neg of formula | ListF of (foperator * formula * formula list) | VarF of (foperator * vdecls * formula) | Diamond of (program * formula)
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (poperator * program * program list) | VarP of (poperator * vdecls * program) | Converse of program | Kleene of program

type 'a decl = vdecls * callable * 'a
type file = set decl list * formula decl list * program decl list * callable
end end

module CIRCUIT = struct module T = struct
type ground_term = Fun of (AST.T.cname * ground_term list) | Int of int
type callable = AST.T.cname * ground_term list
type formula = CallF of callable | Top | Neg of formula | ListF of (AST.T.foperator * formula list) | Diamond of program * formula
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (AST.T.poperator * program list) | Converse of program | Kleene of program

type 'a decl = callable * 'a
type file = formula decl list * program decl list * callable
end end
