type formula = CallF of (bool * Circuit.T.callable) | Base of bool | ListF of (Ast.T.foperator * formula list) | Modal of (bool * program * formula)
and program  = Assign of (Circuit.T.callable * formula) | Test of formula | ListP of (Ast.T.poperator * program list) | Converse of program | Kleene of program

module Print : sig
  val formula : formula -> string
  val program : program -> string
end


val extract_atoms : formula -> Circuit.T.callable list
val file : Circuit.T.file -> formula
