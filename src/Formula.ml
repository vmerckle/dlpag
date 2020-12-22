open Printf

module CMap = Map.Make (struct type t = Circuit.callable let compare = compare end)
module CSet = Set.Make (struct type t = Circuit.callable let compare = compare end)

type formula = CallF of (bool * Circuit.callable) | Base of bool | ListF of (Ast.foperator * formula list) | Modal of (bool * program * formula)
(*type formula = CallF of Circuit.callable | Top | Neg of formula | ListF of (Ast.foperator * formula list) | Diamond of (program * formula)*)
and program  = Assign of (Circuit.callable * formula) | Test of formula | ListP of (Ast.poperator * program list) | Converse of program | Kleene of program

module UPrint = Print
module Print =
struct
  let rec formula = function
    | ListF (a, fs) -> Print.list' "" (sprintf " %s " (Ast.Print.foperator a)) "" inner_formula fs
    | CallF _ | Base _ | Modal _ as f -> inner_formula f
  and inner_formula = function
    | CallF (true, a) -> Circuit.Print.callable a
    | CallF (false, a) -> sprintf "\\neg %s" (Circuit.Print.callable a)
    | Base true -> "\\top"
    | Base false -> "\\bot"
    | Modal (true, p, f) -> sprintf "<%s>%s" (program p) (inner_formula f)
    | Modal (false, p, f) -> sprintf "[%s]%s" (program p) (inner_formula f)
    | ListF _  as f -> sprintf "(%s)" (formula f)
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

let append_pairs (a, b) (c, d) = (a @ c, b @ d)
let concat_map_pairs f l =
  let aux (ones, twos) elt = let (o, t) = f elt in (o @ ones, t @ twos) in
  let (ones, twos) = List.fold_left aux ([], []) l in
  (List.rev ones, List.rev twos)

let rec extract_dependencies_f = function
  | Circuit.CallF c -> [c]
  | Top -> []
  | Neg f -> extract_dependencies_f f
  | ListF (_, fs) -> List.concat_map extract_dependencies_f fs
  | Diamond (p, f) -> (extract_dependencies_f f) @(extract_dependencies_p p)
and extract_dependencies_p = function
  | Circuit.CallP c -> [c]
  | Assign (_, f) | Test f -> extract_dependencies_f f
  | ListP (_, ps) -> List.concat_map extract_dependencies_p ps
  | Converse p | Kleene p -> extract_dependencies_p p

let error_f name = eprintf "warning: function %s defined multiple times\n" (Circuit.Print.callable name)
and error_p name = eprintf "warning: program %s defined multiple times\n" (Circuit.Print.callable name)
and error_fp name = eprintf "warning: %s refers to both a program and a function\n" (Circuit.Print.callable name)

let add_decl_f (map_f, dep_map) (c, f) =
  if CMap.mem c map_f then (error_f c; (map_f, dep_map))
  else (CMap.add c f map_f, (c, extract_dependencies_f f) :: dep_map)
let add_decl_p (map_p, dep_map) (c, p) =
  if CMap.mem c map_p then (error_p c; (map_p, dep_map))
  else (CMap.add c p map_p, (c, extract_dependencies_p p) :: dep_map)

let print_deps = UPrint.list (UPrint.list' "{" ", " "}" Circuit.Print.callable)

let topo_sort decls_f decls_p =
  let names_f, dep_map_f = List.fold_left add_decl_f (CMap.empty, []) decls_f
  and names_p, dep_map_p = List.fold_left add_decl_p (CMap.empty, []) decls_p in
  let just_names_f, just_names_p = List.map fst (CMap.bindings names_f), List.map fst (CMap.bindings names_p) in
  let dup_names = List.filter (fun x -> List.mem x just_names_f) just_names_p in
  List.iter error_fp dup_names;
  let sorted_names = Tsort.sort_strongly_connected_components (dep_map_f @ dep_map_p) in
  (*eprintf "deps: %s\n" (print_deps sorted_names);*)
  (sorted_names, names_f, names_p)

let find_f polarity (names_f, names_p) name = match CMap.find_opt name names_f with
  | Some f -> f
  | None -> match CMap.find_opt name names_p with
            | Some _ -> eprintf "type error: %s is used as a function but defined as a program\n" (Circuit.Print.callable name); CallF (polarity, name)
            | None -> CallF (polarity, name)
let find_p (names_f, names_p) name = match CMap.find_opt name names_p with
  | Some p -> p
  | None -> match CMap.find_opt name names_f with
            | Some _ -> eprintf "type error: %s is used as a program but defined as a function\n" (Circuit.Print.callable name); assert false
            | None -> eprintf "error: program macro %s is undefined\n" (Circuit.Print.callable name); assert false

let operator polarity op = if polarity then op else match op with
  | Ast.Conj -> Ast.Disj
  | Ast.Disj -> Ast.Conj
let rec substitute_f polarity (names_map : formula CMap.t * program CMap.t) = function
  | Circuit.CallF c -> find_f polarity names_map c
  | Top -> Base polarity
  | Neg f -> substitute_f (not polarity) names_map f
  | ListF (op, fs) -> ListF (operator polarity op, List.map (substitute_f polarity names_map) fs)
  | Diamond (p, f) -> Modal (polarity, substitute_p names_map p, substitute_f polarity names_map f)
and substitute_p names_map = function
  | Circuit.CallP c -> find_p names_map c
  | Assign (c, f) ->
     if CMap.mem c (fst names_map) || CMap.mem c (snd names_map) then eprintf "type error: trying to assign to %s which isn't an atomic proposition\n" (Circuit.Print.callable c);
     Assign (c, substitute_f true names_map f)
  | Test f -> Test (substitute_f true names_map f)
  | ListP (op, ps) -> ListP (op, List.map (substitute_p names_map) ps)
  | Converse p -> Converse (substitute_p names_map p)
  | Kleene p -> Kleene (substitute_p names_map p)

let ground_up (names_f_old, names_p_old) sorted_names =
  let update (names_f_new, names_p_new) name = match CMap.find_opt name names_f_old with
    | Some f ->
       let new_f = substitute_f true (names_f_new, names_p_new) f in
       (CMap.add name new_f names_f_new, names_p_new)
    | None ->
       match CMap.find_opt name names_p_old with
       | Some p ->
          let new_p = substitute_p (names_f_new, names_p_new) p in
          (names_f_new, CMap.add name new_p names_p_new)
       | None -> (names_f_new, names_p_new) (*eprintf "oups %s\n" (Circuit.Print.callable name); assert false*) in
  List.fold_left update (CMap.empty, CMap.empty) sorted_names

let file (decls_f, decls_p, main) =
  let (sorted_names, names_f, names_p) = topo_sort decls_f decls_p in
  assert (not (List.mem [] sorted_names));
  let sorted_names = List.map List.hd sorted_names in
  let (names_f, names_p) = ground_up (names_f, names_p) sorted_names in
  match CMap.find_opt main names_f with
  | Some f -> f
  | None -> eprintf "error: undefined main function %s\n" (Circuit.Print.callable main); CallF (true, main)


let extract_atoms main =
  let rec aux_f set = function
    | CallF (_, a) -> CSet.add a set
    | Base _ -> set
    | ListF (_, fs) -> List.fold_left aux_f set fs
    | Modal (_, p, f) -> aux_f (aux_p set p) f
  and aux_p set = function
    | Assign (a, f) -> aux_f (CSet.add a set) f
    | Test f -> aux_f set f
    | ListP (_, ps) -> List.fold_left aux_p set ps
    | Converse p | Kleene p -> aux_p set p in
  let set = aux_f CSet.empty main in
  CSet.elements set
