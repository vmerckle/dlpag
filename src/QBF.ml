open Printf

type callable = Circuit.T.callable * int
type formula = Atom of (bool * callable) | Quantifier of (QCIR.quant * callable list * formula) | ListQ of (Ast.T.foperator * formula list) | Xor of (formula * formula) | Ite of (formula * formula * formula)

type context = { n : int;
                 vars : Circuit.T.callable list }

module Print =
struct
  let callable (n, i) = sprintf "%s_%d" (Circuit.Print.callable n) i
  let rec formula = function
    | ListQ (a, fs) -> Print.list' "" (sprintf " %s " (Ast.Print.foperator a)) "" inner_formula fs
    | Xor _ | Atom _ | Quantifier _ | Ite _ as f -> inner_formula f
  and inner_formula = function
    | Atom (true, a) -> callable a
    | Atom (false, a) -> sprintf "\\neg %s" (callable a)
    | Quantifier (q, cs, f) -> sprintf "\\%s %s, %s" (QCIR.Print.quant q) (Print.list callable cs) (formula f)
    | Xor (f1, f2) -> sprintf "xor(%s, %s)" (formula f1) (formula f2)
    | ListQ _  as f -> sprintf "(%s)" (formula f)
    | Ite (f1, f2, f3) -> sprintf "ite(%s, %s, %s)" (formula f1) (formula f2) (formula f3)
end

let new_valuation =
  let counter = ref 0 in
  let new_v context =
    incr counter;
    (List.map (fun var -> (var, !counter)) context.vars, !counter) in
  new_v

let new_var =
  let counter = ref 0 in
  let new_v () =
    incr counter;
    (("fresh", []), !counter) in
  new_v

let conj2 x y = ListQ (Ast.T.Conj, [x; y])
let top = ListQ (Ast.T.Conj, [])
let bottom = ListQ (Ast.T.Disj, [])

let assign context vx vy name f =
  let aux othername = if othername <> name then Some (Xor (Atom (false, (othername, vx)), Atom (true, (othername, vy)))) else None in
  let keep_others = List.filter_map aux context.vars in
  let set_one = Xor (Atom (false, (name, vy)), f) in
  ListQ (Ast.T.Conj, set_one :: keep_others)

let make_equiv context vx vy =
  let aux var = Xor (Atom (false, (var, vx)), Atom (true, (var, vy))) in
  List.map aux context.vars

let rec reduction_f context vx = function
  | Formula.CallF (polarity, name) -> Atom (polarity, (name, vx))
  | Base true -> ListQ (Ast.T.Conj, [])
  | Base false -> ListQ (Ast.T.Disj, [])
  | ListF (op, fs) -> ListQ (op, List.map (reduction_f context vx) fs)
  | Modal (true, p, f) ->
     let vary, vy = new_valuation context in
     Quantifier (QCIR.Exists, vary, conj2 (reduction_g context vx vy p) (reduction_f context vy f))
  | Modal (false, p, f) ->
     let vary, vy = new_valuation context in
     let prog = reduction_g context vx vy p
     and form = reduction_f context vy f in
     Quantifier (QCIR.Forall, vary, Ite (prog, form, bottom))

and reduction_g context vx vy = function
  | Formula.Assign (name, f) -> assign context vx vy name (reduction_f context vx f)
  | Test f -> ListQ (Ast.T.Conj, reduction_f context vx f :: make_equiv context vx vy)
  | ListP (Ast.T.U, ps) -> ListQ (Ast.T.Disj, List.map (reduction_g context vx vy) ps)
  | ListP (Ast.T.Seq, ps) -> reduction_i context vx vy ps
  | Converse p -> reduction_g context vy vx p
  | Kleene p -> reduction_h context context.n vx vy (Formula.ListP (U, [Formula.Test (Formula.Base true); p]))

and reduction_h context m vx vy p =
  if m <= 0 then ListQ (Ast.T.Disj, reduction_g context vx vy p :: make_equiv context vx vy)
  else
    let varz,  vz  = new_valuation context in
    let varx1, vx1 = new_valuation context in
    let vary1, vy1 = new_valuation context in
    let vart = new_var () in
    let ite_then = ListQ (Ast.T.Conj, (make_equiv context vx1 vx) @ (make_equiv context vy1 vz)) in
    let ite_else = ListQ (Ast.T.Conj, (make_equiv context vx1 vz) @ (make_equiv context vy1 vy)) in
    let ite = Ite (Atom (true, vart), ite_then, ite_else) in
    let conj = [reduction_h context (m-1) vx1 vy1 p; ite] in
    Quantifier (QCIR.Exists, varz, Quantifier (QCIR.Forall, [vart], Quantifier (QCIR.Exists, varx1 @ vary1, ListQ (Ast.T.Conj, conj))))

and reduction_i context vx vy ps = match ps with
  | [] -> assert false
  | p :: [] -> reduction_g context vx vy p
  | pa :: rest ->
     let varz,  vz  = new_valuation context in
     let first_step = reduction_g context vx vz pa in
     let rest_step = reduction_i context vz vy rest in
     Quantifier (QCIR.Exists, varz, conj2 first_step rest_step)

let form main =
  let vars = Formula.extract_atoms main in
  let context = { n = List.length vars;
                  vars } in
  let varx, vx = new_valuation context in
  (varx, reduction_f context vx main)

module CMap = Map.Make (struct type t = callable let compare = compare end)

let rec substitute map = function
  | Atom (b1, call) as a -> (match CMap.find_opt call map with
                       | None -> a
                       | Some b2 -> if b1 = b2 then top else bottom)
  | Quantifier (q, vars, phi) -> assert (List.for_all (fun x -> not (CMap.mem x map)) vars); Quantifier (q, vars, substitute map phi)
  | ListQ (op, phis) -> ListQ (op, List.map (substitute map) phis)
  | Xor (phi1, phi2) -> Xor (substitute map phi1, substitute map phi2)
  | Ite (phi1, phi2, phi3) -> Ite (substitute map phi1, substitute map phi2, substitute map phi3)

(** Remove free variables by setting them to bot *)
let pre_model_checking_empty f =
  let vars, phi = form f in
  let map = List.fold_left (fun accu var -> CMap.add var false accu) CMap.empty vars in
  substitute map phi


let new_gate =
  let counter = ref 0 in
  let new_g () =
    incr counter;
    sprintf "g%d" !counter in
  new_g

let to_qcir f =
  let gates = Stack.create () in
  let rec aux = function
    | Atom (b, c) -> (b, Print.callable c)
    | Quantifier (q, cs, f) ->
       let cs = List.map Print.callable cs in
       let l = aux f in
       let g = QCIR.Quantifier (q, cs, l) in
       let gn = new_gate () in
       Stack.push (gn, g) gates;
       (true, gn)
    | ListQ (o, fs) ->
       let ls = List.map aux fs in
       let op = match o with Ast.T.Conj -> QCIR.And | Ast.T.Disj -> QCIR.Or in
       let g = QCIR.Group (op, ls) in
       let gn = new_gate () in
       Stack.push (gn, g) gates;
       (true, gn)
    | Xor (f1, f2) ->
       let l1, l2 = aux f1, aux f2 in
       let g = QCIR.Xor (l1, l2) in
       let gn = new_gate () in
       Stack.push (gn, g) gates;
       (true, gn)
    | Ite (f1, f2, f3) ->
       let l1, l2, l3 = aux f1, aux f2, aux f3 in
       let g = QCIR.Ite (l1, l2, l3) in
       let gn = new_gate () in
       Stack.push (gn, g) gates;
       (true, gn) in
  let output = aux f in
  let gates = List.of_seq (Stack.to_seq gates) in
  let gates = List.rev gates in
  { QCIR.free_vars = [];
    qblocks = [];
    output;
    gates }


let model_checking_empty f =
  let phi = pre_model_checking_empty f in
  to_qcir phi
