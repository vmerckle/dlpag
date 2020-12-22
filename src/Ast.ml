open Printf

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

type file = ground_decl list * formula_decl list * program_decl list * callable (*[@@deriving show]*)

module Print =
struct
  let id x = x
  let constant = id
  let cname = id

  let ground = function
    | CallG n -> n
    | Set cs -> Print.list' "{" ", " "}" id cs
  let ground_decl (n, cs) = sprintf "%s := { %s }." n (Print.list' "" ", " "" id cs)
  let var_decl (n, t) = sprintf "%s \\in %s" n (ground t)
  let var_decls vs = Print.list' "" " " "" var_decl vs

  let eoperator_aux = function
    | Add -> "+"
    | Mult -> "*"
  let foperator_aux = function
    | Conj -> "and"
    | Disj -> "or"
  let poperator_aux = function
    | Seq -> "seq"
    | U -> "cup"
  let eoperator o = "\\" ^ eoperator_aux o
  let foperator o = "\\" ^ foperator_aux o
  let poperator o = "\\" ^ poperator_aux o
  let bigeoperator o = "\\big" ^ eoperator_aux o
  let bigfoperator o = "\\big" ^ foperator_aux o
  let bigpoperator o = "\\big" ^ poperator_aux o

  let rec expr = function
    | ListE (a, es) -> Print.list' "" (sprintf " %s " (eoperator a)) "" inner_expr es
    | VarE (a, vs, e) -> sprintf "%s %s, %s" (bigeoperator a) (var_decls vs) (expr e)
    | Subtract (v, vs) -> Print.list' "" " - " "" inner_expr (v :: vs)
    | Var _ | Const _ | Int _ as e -> inner_expr e
  and inner_expr = function
    | Var n -> n
    | Const n -> n
    | Int i -> Print.int i
    | ListE _ | VarE _ | Subtract _ as e -> sprintf "(%s)" (expr e)
  let callable (n, ts) = match ts with
    | [] -> cname n
    | _ :: _ -> sprintf "%s(%s)" n (Print.list' "" ", " "" expr ts)

  let rec formula = function
    | ListF (a, fs) -> Print.list' "" (sprintf " %s " (foperator a)) "" inner_formula fs
    | VarF (a, vs, f) -> sprintf "%s %s, %s" (bigfoperator a) (var_decls vs) (formula f)
    | CallF _ | Top | Neg _ | Diamond _ as f -> inner_formula f
  and inner_formula = function
    | CallF a -> callable a
    | Top -> "\\top"
    | Neg f -> sprintf "\\neg %s" (inner_formula f)
    | Diamond (p, f) -> sprintf "<%s>%s" (program p) (inner_formula f)
    | ListF _ | VarF _  as f -> sprintf "(%s)" (formula f)
  and program = function
    | ListP (a, ps) -> Print.list' "" (sprintf " %s " (poperator a)) "" inner_program ps
    | VarP (a, vs, p) -> sprintf "%s %s, %s" (bigpoperator a) (var_decls vs) (program p)
    | CallP _ | Assign _ | Test _ | Converse _ | Kleene _ as p -> inner_program p
  and inner_program = function
    | CallP a -> callable a
    | Assign (a, f) -> sprintf "%s <- %s" (callable a) (formula f)
    | Test f -> sprintf "%s?" (formula f)
    | Converse p -> sprintf "%s^" (program p)
    | Kleene p -> sprintf "%s*" (program p)
    | ListP _ | VarP _ as p -> sprintf "(%s)" (program p)

  let forall_decls vs = match vs with
    | [] -> ""
    | _ :: _ -> sprintf "\\forall %s, " (var_decls vs)
  let formula_decl (vs, c, f) = sprintf "%s%s := %s." (forall_decls vs) (callable c) (formula f)
  let program_decl (vs, c, f) = sprintf "%s%s := %s." (forall_decls vs) (callable c) (program f)
  let main_decl m = sprintf "%s." (callable m)

  let file (gs, fs, ps, m) = sprintf "grounding:\n%s\nformula:\n%s\nprogram:\n%smain:\n%s" (Print.unlines ground_decl gs) (Print.unlines formula_decl fs) (Print.unlines program_decl ps) (main_decl m)
end
