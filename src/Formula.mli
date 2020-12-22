type formula = CallF of (bool * Circuit.callable) | Base of bool | ListF of (Ast.foperator * formula list) | Modal of (bool * program * formula)
and program  = Assign of (Circuit.callable * formula) | Test of formula | ListP of (Ast.poperator * program list) | Converse of program | Kleene of program

module Print : sig
  val formula : formula -> string
  val program : program -> string
end


val extract_atoms : formula -> Circuit.callable list
val file : Circuit.file -> formula
