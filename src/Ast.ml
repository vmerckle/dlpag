open Printf

include Types.AST
open T

module Print =
struct
  let id x = x
  let constant = id
  let cname = id

  let eoperator_aux = function
    | Add -> "+"
    | Mult -> "*"
    | Max -> "\\max"
    | Min -> "\\min"
  let foperator_aux = function
    | Conj -> "and"
    | Disj -> "or"
  let poperator_aux = function
    | Seq -> "seq"
    | U -> "cup"
  let roperator = function
    | Eq -> "="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
  let eoperator o = "\\" ^ eoperator_aux o
  let foperator o = "\\" ^ foperator_aux o
  let poperator o = "\\" ^ poperator_aux o
  let bigeoperator o = "\\big" ^ eoperator_aux o
  let bigfoperator o = "\\big" ^ foperator_aux o
  let bigpoperator o = "\\big" ^ poperator_aux o

  let rec set = function
    | Set (t, []) -> sprintf "{ %s }" (Print.unspaces tuple t)
    | Set (t, (_ :: _ as vs)) -> sprintf "{ %s | %s }" (Print.unspaces tuple t) (vdecls vs)
    | Name c -> callable c
  and vdecls ds = Print.list' "" ", " "" vdecl ds
  and vdecl = function
    | FromSet (names, s) -> sprintf "%s \\in %s" (Print.unspaces id names) (set s)
    | Relation (r, e1, e2) -> sprintf "%s %s %s" (expr e1) (roperator r) (expr e2)
  and tuple = function
    | Tuple es -> Print.list' "(" ", " ")" expr es
    | Range (e1, e2) -> sprintf "%s..%s" (expr e1) (expr e2)
  and expr = function
    | ListE (a, es) -> Print.list' "" (sprintf " %s " (eoperator a)) "" inner_expr es
    | VarE (a, vs, e) -> sprintf "%s %s, %s" (bigeoperator a) (vdecls vs) (expr e)
    | Subtract (v, vs) -> Print.list' "" " - " "" inner_expr (v :: vs)
    | Var _ | Const _ | Int _ as e -> inner_expr e
  and inner_expr = function
    | Var n -> n
    | Const n -> n
    | Int i -> Print.int i
    | ListE _ | VarE _ | Subtract _ as e -> sprintf "(%s)" (expr e)
  and callable (n, ts) = match ts with
    | [] -> cname n
    | _ :: _ -> sprintf "%s(%s)" n (Print.list' "" ", " "" expr ts)

  let rec formula = function
    | ListF (a, fs) -> Print.list' "" (sprintf " %s " (foperator a)) "" inner_formula fs
    | VarF (a, vs, f) -> sprintf "%s %s, %s" (bigfoperator a) (vdecls vs) (formula f)
    | CallF _ | Top | Neg _ | Diamond _ as f -> inner_formula f
  and inner_formula = function
    | CallF a -> callable a
    | Top -> "\\top"
    | Neg f -> sprintf "\\neg %s" (inner_formula f)
    | Diamond (p, f) -> sprintf "<%s>%s" (program p) (inner_formula f)
    | ListF _ | VarF _  as f -> sprintf "(%s)" (formula f)
  and program = function
    | ListP (a, ps) -> Print.list' "" (sprintf " %s " (poperator a)) "" inner_program ps
    | VarP (a, vs, p) -> sprintf "%s %s, %s" (bigpoperator a) (vdecls vs) (program p)
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
    | _ :: _ -> sprintf "\\forall %s, " (vdecls vs)
  let decl pr (vs, c, a) = sprintf "%s%s := %s." (forall_decls vs) (callable c) (pr a)
  let main_decl m = sprintf "%s." (callable m)

  let file (gs, fs, ps, m) = sprintf "grounding:\n%s\nformula:\n%s\nprogram:\n%smain:\n%s" (Print.unlines (decl set) gs) (Print.unlines (decl formula) fs) (Print.unlines (decl program) ps) (main_decl m)
end
