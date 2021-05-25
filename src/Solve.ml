open Printf

type callable = int
type formula = CallF of callable | Atom of callable | Neg of formula | ListF of (Ast.foperator * formula list) | Diamond of (program * formula)
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (Ast.poperator * program list) | Converse of program | Kleene of program

type formula_decl = callable * formula
type program_decl = callable * program
type file = formula_decl list * program_decl list * callable

module CMap = Map.Make (struct type t = Circuit.callable let compare = compare end)
module CSet = Set.Make (struct type t = Circuit.callable let compare = compare end)

let rec formula (mapa, mapf, mapp) = function
  | Circuit.CallF c -> if CMap.mem c mapf then CallF (CMap.find c mapf) else (assert (CMap.mem c mapa); Atom (CMap.find c mapa))
  | Circuit.Top -> ListF (Ast.Conj, [])
  | Circuit.Neg f -> Neg (formula (mapa, mapf, mapp) f)
  | Circuit.ListF (o, fs) -> ListF (o, List.map (formula (mapa, mapf, mapp)) fs)
  | Circuit.Diamond (p, f) -> Diamond (program (mapa, mapf, mapp) p, formula (mapa, mapf, mapp) f)
and program (mapa, mapf, mapp) = function
  | Circuit.CallP c -> assert (CMap.mem c mapp); CallP (CMap.find c mapp)
  | Circuit.Assign (c, f) -> assert (CMap.mem c mapa); Assign (CMap.find c mapa, formula (mapa, mapf, mapp) f)
  | Circuit.Test f -> Test (formula (mapa, mapf, mapp) f)
  | Circuit.ListP (o, ps) -> ListP (o, List.map (program (mapa, mapf, mapp)) ps)
  | Circuit.Converse p -> Converse (program (mapa, mapf, mapp) p)
  | Circuit.Kleene p -> Kleene (program (mapa, mapf, mapp) p)

let extract_names decls (fdecs, pdecs, call) =
  let rec aux_f (seta, setf, setp) = function
    | Circuit.CallF c -> if List.mem c decls then (seta, CSet.add c setf, setp) else (CSet.add c seta, setf, setp)
    | Top -> (seta, setf, setp)
    | Neg f -> aux_f (seta, setf, setp) f
    | ListF (_, fs) -> List.fold_left aux_f (seta, setf, setp) fs
    | Diamond (p, f) -> aux_f (aux_p (seta, setf, setp) p) f
  and aux_p (seta, setf, setp) = function
    | Circuit.CallP c -> (seta, setf, CSet.add c setp)
    | Assign (a, f) -> aux_f (CSet.add a seta, setf, setp) f
    | Test f -> aux_f (seta, setf, setp) f
    | ListP (_, ps) -> List.fold_left aux_p (seta, setf, setp) ps
    | Converse p | Kleene p -> aux_p (seta, setf, setp) p in
  let (seta, setf, setp) = List.fold_left aux_f (CSet.empty, CSet.empty, CSet.empty) fdecs in
  let (seta, setf, setp) = List.fold_left aux_p (seta, setf, setp) pdecs in
(*  eprintf "%s\n" (Print.unlines (fun (f) -> sprintf "%s" (Circuit.Print.formula f)) fdecs);
  eprintf "%s %s\n" (Circuit.Print.callable call) (Print.list Circuit.Print.callable (CSet.elements setf));
  assert (CSet.mem call setf);*)
  (seta, setf, setp)

let convert_to_ints (fdecs, pdecs, call) =
  let decls = List.map fst fdecs in
  assert (List.mem call decls);
  let (seta, setf, setp) = extract_names decls (List.map snd fdecs, List.map snd pdecs, call) in
(*  eprintf "%s\n\n%s\n" (Print.unspaces (fun (c, f) -> sprintf "%s  " (Circuit.Print.callable c)) fdecs) (Print.unspaces (fun (f) -> sprintf "%s  " (Circuit.Print.callable f)) (CSet.elements setf));*)
  assert (List.mem call (List.map fst fdecs));
  assert (CSet.for_all (fun c -> List.mem c (List.map fst fdecs)) setf);
  assert (CSet.for_all (fun c -> List.mem c (List.map fst pdecs)) setp);
(*  assert (List.length fdecs > 0 && List.length pdecs > 0);*)
  let array_fdecs = Array.of_list fdecs
  and array_pdecs = Array.of_list pdecs in
  let make_map array =
    let map = ref CMap.empty in
    Array.iteri (fun i (c, _) -> map := CMap.add c i !map) array;
    !map in
  let mapa = make_map (Array.map (fun c -> (c, c)) (Array.of_seq (CSet.to_seq seta)))
  and mapf = make_map array_fdecs
  and mapp = make_map array_pdecs in
  let array_fdecs = Array.map (fun (_, f) -> formula (mapa, mapf, mapp) f) array_fdecs
  and array_pdecs = Array.map (fun (_, p) -> program (mapa, mapf, mapp) p) array_pdecs in
  assert (CMap.mem call mapf);
  let call = CMap.find call mapf in
  (call, mapa, array_fdecs, array_pdecs)

module Valuation =
struct
  type t = bool Array.t
  let compare = compare
  let test_atom v a = v.(a)
  let copy = Array.copy
  let print a = "a" ^ Print.array (sprintf "%b") a
end

module VSet = Set.Make (Valuation)

let vset_unions l = List.fold_left (fun accu s -> VSet.union accu s) VSet.empty l
let vset_map_unions f l = List.fold_left (fun accu elt -> VSet.union accu (f elt)) VSet.empty l
let vset_map_unions2 f l = VSet.fold (fun elt accu -> VSet.union accu (f elt)) l VSet.empty

let rec naive_f domain valuation = function
  | CallF c -> let (fs, _) = domain in assert (c < Array.length fs); naive_f domain valuation fs.(c)
  | Atom a -> Valuation.test_atom valuation a
  | Neg f -> not (naive_f domain valuation f)
  | ListF (Ast.Conj, fs) -> List.for_all (naive_f domain valuation) fs
  | ListF (Ast.Disj, fs) -> List.exists (naive_f domain valuation) fs
  | Diamond (p, f) -> VSet.exists (fun v -> naive_f domain v f) (naive_p domain (Array.copy valuation) p)
and naive_p domain valuation = function
  | CallP c -> let (_, ps) = domain in assert (c < Array.length ps); naive_p domain valuation ps.(c)
  | Assign (c, f) -> assert (c < Array.length valuation); valuation.(c) <- naive_f domain valuation f; VSet.singleton valuation
  | Test f -> if naive_f domain (Valuation.copy valuation) f then VSet.singleton valuation else VSet.empty
  | ListP (Ast.U, ps) -> let map p = naive_p domain (Valuation.copy valuation) p in vset_map_unions map ps
  | ListP (Ast.Seq, []) -> VSet.singleton valuation
  | ListP (Ast.Seq, p :: ps) ->
     let first = naive_p domain valuation p in
     vset_map_unions2 (fun valua -> naive_p domain valua (ListP (Ast.Seq, ps))) first
  | Converse p -> assert false
  | Kleene p ->
     let all = ref (VSet.singleton valuation) in
     let stack = Stack.create () in
     Stack.push valuation stack;
     (*eprintf "%s\n" (Valuation.print valuation);*)
     let treat v = if not (VSet.mem v !all) then (all := VSet.add v !all; Stack.push v stack) in
     while not (Stack.is_empty stack) do
       (*eprintf "hello\n";*)
       let news = naive_p domain (Stack.pop stack) p in
       (*eprintf "%s\n" (Print.list Valuation.print (VSet.elements news));*)
       VSet.iter treat news
     done;
     !all

(*let rec run_f (domain : formula_decl list * program_decl list) valuation = function
  | CallF c -> let (fs, _) = domain in assert (c < Array.length fs); run_f domain valuation fs.(c)
  | Atom a -> Valuation.test_atom valuation a
  | Neg f -> not (run_f domain valuation f)
  | ListF (Ast.Conj, fs) -> List.for_all (run_f domain valuation) fs
  | ListF (Ast.Disj, fs) -> List.exists (run_f domain valuation) fs
  | Diamond (p, f) -> run_p domain [(valuation, [p], f)]
and run_p domain = function
  | [] -> false
  | (valuation, [], f) :: rest -> run_f domain valuation f
  | (valuation, p :: rest_seq, f) :: rest_nondet ->
     match p with
     | CallP c -> let (_, ps) = domain in assert (c < Array.length ps); run_p domain ((valuation, ps.(c) :: rest_seq, f) :: rest_nondet)
     | Assign (c, f) -> assert (c < Array.length valuation); valuation.(c) <- run_f domain valuation f; run_p domain ((valuation, rest_seq, f) :: rest_nondet)
     | Test f -> run_f domain (Valuation.copy valuation) f && run_p domain ((valuation, rest_seq, f) :: rest_nondet)
     | ListP (Ast.U, ps) -> run_p domain (List.map (fun prog -> (Valuation.copy valuation, prog :: rest_seq, f)) ps @ rest_nondet)
     | ListP (Ast.Seq, ps) -> run_p domain ((valuation, ps @ rest_seq, f) :: rest_nondet)
     | Converse p -> assert false
     | Kleene p -> run_star domain valuation p rest_seq f rest_nondet
and run_star domain valuation p rest_seq f rest_nondet =
  let vset = ref VSet.empty in
  let found = ref false in
  while not (VSet.mem valuation !vset) do
    let vp
    if run_p
  done;
  for i = 0 to 2
  let zero = run_p domain ((valuation, rest_seq, f) :: rest_nondet) in
 
  assert false
*)
(*  let zero = Valuation.copy valuation in
  let vset = ref (VSet.singleton zero) in
  let test valu =
    if VSet.mem valu !vset then false
    else (vset := VSet.add (Valuation.copy valu) !vset; cont_test valu) in
  let rec aux valu =
    if VSet.mem valu !vset then false
    else
      (let vc = Valuation.copy valu in
       if run_p domain valu then true
      (vset := VSet.add vc !vset;
       cont_test valu)

    if cont_test valuation then true
    else*)


let model_checking (fdecs, pdecs, call) valuation =
  let (call, mapa, array_fdecs, array_pdecs) = convert_to_ints (fdecs, pdecs, call) in
  let array_val = Array.make (CMap.cardinal mapa) false in
  let add_to_val elt = assert (CMap.mem elt mapa); array_val.(CMap.find elt mapa) <- true in
  List.iter add_to_val valuation;
  naive_f (array_fdecs, array_pdecs) array_val array_fdecs.(call)

