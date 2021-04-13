open Printf

type formula = Call of Circuit.callable | Top | Neg of formula | Disj of (formula list) | Diamond of (program * formula)
(*type formula = CallF of Circuit.callable | Top | Neg of formula | ListF of (Ast.foperator * formula list) | Diamond of (program * formula)*)
and program  = Assign of (Circuit.callable * formula) | Test of formula | ListP of (Ast.poperator * program list) | Converse of program | Kleene of program

module F = Formula

let rec formula = function
  | F.CallF (true, c) -> Call c
  | F.CallF (false, c) -> Neg (Call c)
  | F.Base true -> Top
  | F.Base false -> Neg Top
  | F.ListF (Ast.Disj, fs) -> Disj (List.map formula fs)
  | F.ListF (Ast.Conj, fs) -> Neg (Disj (List.map (fun f -> Neg (formula f)) fs))
  | F.Modal (true, p, f) -> Diamond (program p, formula f)
  | F.Modal (false, p, f) -> Neg (Diamond (program p, Neg (formula f)))
and program = function
  | F.Assign (c, f) -> Assign (c, formula f)
  | F.Test f -> Test (formula f)
  | F.ListP (o, ps) -> ListP (o, List.map program ps)
  | F.Converse p -> Converse (program p)
  | F.Kleene p -> Kleene (program p)

module Print =
struct
  let rec formula = function
    | Disj fs -> Print.list' "" " \\or " "" inner_formula fs
    | Neg _ | Call _ | Top | Diamond _ as f -> inner_formula f
  and inner_formula = function
    | Call a -> Circuit.Print.callable a
    | Neg f -> sprintf "\\neg %s" (inner_formula f)
    | Top -> "\\top"
    | Diamond (p, f) -> sprintf "<%s>%s" (program p) (inner_formula f)
    | Disj _  as f -> sprintf "(%s)" (formula f)
  and program = function
    | ListP (a, ps) -> Print.list' "" (sprintf " %s " (Ast.Print.poperator a)) "" inner_program ps
    | Assign _ | Test _ | Converse _ | Kleene _ as p -> inner_program p
  and inner_program = function
    | Assign (a, f) -> sprintf "%s <- %s" (Circuit.Print.callable a) (formula f)
    | Test f -> sprintf "%s?" (formula f)
    | Converse p -> sprintf "%s^" (program p)
    | Kleene p -> sprintf "%s*" (program p)
    | ListP _ as p -> sprintf "(%s)" (program p)
end
