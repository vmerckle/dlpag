type constant = string
type cname = string
type vname = string
type ground = CallG of cname | Set of constant list
type ground_decl = cname * constant list

type var_decl = vname * ground
type var_decls = var_decl list
type eoperator = Add | Mult
type foperator = Conj | Disj
type poperator = Seq | U
type expr = Var of vname | Const of constant | Int of int | ListE of (eoperator * expr list) | VarE of (eoperator * var_decls * expr) | Subtract of (expr * expr list)
type callable = cname * expr list
type formula = CallF of callable | Top | Neg of formula | ListF of (foperator * formula list) | VarF of (foperator * var_decls * formula) | Diamond of (program * formula)
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (poperator * program list) | VarP of (poperator * var_decls * program) | Converse of program | Kleene of program

type formula_decl = var_decls * callable * formula
type program_decl = var_decls * callable * program

type file = ground_decl list * formula_decl list * program_decl list * callable

module Print :
sig
  val constant : constant -> string
  val cname : cname -> string
  val eoperator : eoperator -> string
  val foperator : foperator -> string
  val poperator : poperator -> string
  val file : file -> string
end
