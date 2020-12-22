open Printf

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

module UPrint = Print
module Print = struct
  let var = UPrint.string
  let comma_list f l = UPrint.list' "" ", " "" f l
  let literal (b, v) = if b then var v else "-" ^ var v
  let operator = function
    | And -> "and"
    | Or -> "or"
  let quant = function
    | Exists -> "exists"
    | Forall -> "forall"
  let gate_body = function
    | Group (o, l) -> sprintf "%s(%s)" (operator o) (comma_list literal l)
    | Xor (l1, l2) -> sprintf "xor(%s, %s)" (literal l1) (literal l2)
    | Ite (l1, l2, l3) -> sprintf "ite(%s, %s, %s)" (literal l1) (literal l2) (literal l3)
    | Quantifier (q, vs, l) -> sprintf "%s(%s; %s)" (quant q) (comma_list var vs) (literal l)
  let gate_statement (v, body) = sprintf "%s = %s" (var v) (gate_body body)
  let qblock (q, vs) = sprintf "%s(%s)" (quant q) (comma_list var vs)
  let free_vars vs = if vs = [] then "" else sprintf "free(%S)\n" (comma_list var vs)
  let output lit = sprintf "output(%s)\n" (literal lit)
  let format_id = "#QCIR-G14\n"
  let file f =
    sprintf "%s%s%s%s%s"
      format_id
      (free_vars f.free_vars)
      (UPrint.unlines qblock f.qblocks)
      (output f.output)
      (UPrint.unlines gate_statement f.gates)
end


module SMap = Map.Make (String)
module SSet = Set.Make (String)

let sset_unions l = List.fold_left (fun accu s -> SSet.union accu s) SSet.empty l
let sset_map_unions f l = List.fold_left (fun accu elt -> SSet.union accu (f elt)) SSet.empty l

let replace_names_var map var = SMap.find var map
let replace_names_lit map (b, var) = (b, SMap.find var map)
let replace_names_gate_body map = function
  | Group (o, lits) -> Group (o, List.map (replace_names_lit map) lits)
  | Xor (l1, l2) -> Xor (replace_names_lit map l1, replace_names_lit map l2)
  | Ite (l1, l2, l3) -> Ite (replace_names_lit map l1, replace_names_lit map l2, replace_names_lit map l3)
  | Quantifier (q, vars, lit) -> Quantifier (q, List.map (replace_names_var map) vars, replace_names_lit map lit)
let replace_names_gate_statement map (var, body) = (replace_names_var map var, replace_names_gate_body map body)
let replace_names_qblock map (q, vars) = (q, List.map (replace_names_var map) vars)
let replace_names_file map f = { free_vars = List.map (replace_names_var map) f.free_vars;
                                 qblocks = List.map (replace_names_qblock map) f.qblocks;
                                 output = replace_names_lit map f.output;
                                 gates = List.map (replace_names_gate_statement map) f.gates }

let extract_names_var var = SSet.singleton var
let extract_names_lit (_, var) = SSet.singleton var
let extract_names_gate_body = function
  | Group (_, lits) -> sset_map_unions extract_names_lit lits
  | Xor (l1, l2) -> SSet.union (extract_names_lit l1) (extract_names_lit l2)
  | Ite (l1, l2, l3) -> sset_unions [extract_names_lit l1; extract_names_lit l2; extract_names_lit l3]
  | Quantifier (_, vars, lit) -> SSet.union (sset_map_unions extract_names_var vars) (extract_names_lit lit)
let extract_names_gate_statement (var, body) = SSet.union (extract_names_var var) (extract_names_gate_body body)
let extract_names_qblock (_, vars) = sset_map_unions extract_names_var vars
let extract_names_file f =
  let free_vars = sset_map_unions extract_names_var f.free_vars
  and qblocks = sset_map_unions extract_names_qblock f.qblocks
  and output = extract_names_lit f.output
  and gates = sset_map_unions extract_names_gate_statement f.gates in
  sset_unions [free_vars; qblocks; output; gates]


let string_escape s =
  let replace s rep char = String.concat rep (String.split_on_char char s) in
  let rec replace_mult s rep = function
    | [] -> s
    | char :: rest -> replace_mult (replace s rep char) rep rest in
  let rec remove_chars s = function
    | [] -> s
    | char :: rest -> remove_chars (String.concat "" (String.split_on_char char s)) rest in
  let s = remove_chars s [' '] in
  replace_mult s "_" [','; '('; ')']

let new_var =
  let counter = ref 0 in
  let f () = incr counter; sprintf "__v%d" !counter in
  f

let new_int_var =
  let counter = ref 0 in
  let f () = incr counter; sprintf "%d" !counter in
  f

let readable_sanitize_one set name =
  let escaped = string_escape name in
  if SSet.mem escaped set then new_var ()
  else escaped
let int_sanitize_one _ _ =
  new_int_var ()

let sanitize_aux sani_one f =
  let names = extract_names_file f in
  let aux elt accu =
    let sname = sani_one names elt in
    SMap.add elt sname accu in
  let map = SSet.fold aux names SMap.empty in
  replace_names_file map f

let sanitize_names_readable f = sanitize_aux readable_sanitize_one f
let sanitize_names f = sanitize_aux int_sanitize_one f

