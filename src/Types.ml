module AST = struct module T = struct
type constant = string
type cname = string
type vname = string

type eoperator = Add | Mult | Max | Min
type foperator = Conj | Disj
type poperator = Seq | U


type set = Set of (tuple list * vdecls) | Name of callable
and vdecls = vdecl list
and vdecl = FromSet of (vname list * set) | Relation
and tuple = Tuple of expr list | Range of (expr * expr)
and expr = Var of vname | Const of constant | Int of int | ListE of (eoperator * expr list) | VarE of (eoperator * vdecls * expr) | Subtract of (expr * expr list)
and callable = cname * expr list

type formula = CallF of callable | Top | Neg of formula | ListF of (foperator * formula list) | VarF of (foperator * vdecls * formula) | Diamond of (program * formula)
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (poperator * program list) | VarP of (poperator * vdecls * program) | Converse of program | Kleene of program

type 'a decl = vdecls * callable * 'a
type file = set decl list * formula decl list * program decl list * callable
end end

module CIRCUIT = struct module T = struct
type callable = AST.T.cname * AST.T.constant list
type formula = CallF of callable | Top | Neg of formula | ListF of (AST.T.foperator * formula list) | Diamond of program * formula
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (AST.T.poperator * program list) | Converse of program | Kleene of program

type 'a decl = callable * 'a
type file = formula decl list * program decl list * callable
end end
