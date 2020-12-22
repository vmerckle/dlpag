type var = string
type literal = bool * string
type operator = And | Or
type quant = Exists | Forall
type gate_body = Group of (operator * literal list) | Xor of (literal * literal) | Ite of (literal * literal * literal) | Quantifier of (quant * var list * literal)
type gate_statement = var * gate_body
type qblock = quant * var list
type file = { free_vars : var list;
              qblocks : qblock list;
              output : literal;
              gates : gate_statement list }

module Print : sig
  val literal : literal -> string
  val operator : operator -> string
  val quant : quant -> string
  val gate_body : gate_body -> string
  val gate_statement : gate_statement -> string
  val qblock : qblock -> string
  val file : file -> string
end

val sanitize_names : file -> file
