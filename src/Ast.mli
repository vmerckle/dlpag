include module type of Types.AST with module T = Types.AST.T

module Print :
sig
  val constant : T.constant -> string
  val cname : T.cname -> string
  val eoperator : T.eoperator -> string
  val foperator : T.foperator -> string
  val poperator : T.poperator -> string
  val file : T.file -> string
end
