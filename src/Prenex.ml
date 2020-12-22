open Printf

type callable = Circuit.callable * int
type qf_formula = Atom of (bool * QBF.callable) | ListQ of (Ast.foperator * qf_formula list) | Xor of (qf_formula * qf_formula) | Ite of (qf_formula * qf_formula * qf_formula)

type formula = (QCIR.quant * callable list) list * qf_formula


let rec turn = function
  | QBF.Atom a -> ([], Atom a)
  | QBF.Quantifier (q, cs, f) -> let (l, qff) = turn f in ((q, cs) :: l, qff)
  | QBF.ListQ (, fs) -> 
  | QBF.Xor (f1, f2) -> 
  | QBF.Ite (f1, f2, f3) -> 
