open Printf

include Types.CIRCUIT
open T

module P = Print
module Print =
struct
  let callable (n, cs) = match cs with
    | [] -> Ast.Print.cname n
    | _ :: _ -> sprintf "%s(%s)" (Ast.Print.cname n) (Print.list' "" ", " "" Ast.Print.constant cs)
  let rec formula = function
    | ListF (a, fs) -> Print.list' "" (sprintf " %s " (Ast.Print.foperator a)) "" inner_formula fs
    | CallF _ | Top | Neg _ | Diamond _ as f -> inner_formula f
  and inner_formula = function
    | CallF a -> callable a
    | Top -> "\\top"
    | Neg f -> sprintf "\\neg %s" (inner_formula f)
    | Diamond (p, f) -> sprintf "<%s>%s" (program p) (inner_formula f)
    | ListF _  as f -> sprintf "(%s)" (formula f)
  and program = function
    | ListP (a, ps) -> Print.list' "" (sprintf " %s " (Ast.Print.poperator a)) "" inner_program ps
    | CallP _ | Assign _ | Test _ | Converse _ | Kleene _ as p -> inner_program p
  and inner_program = function
    | CallP a -> callable a
    | Assign (a, f) -> sprintf "%s <- %s" (callable a) (formula f)
    | Test f -> sprintf "%s?" (formula f)
    | Converse p -> sprintf "%s^" (program p)
    | Kleene p -> sprintf "%s*" (program p)
    | ListP _ as p -> sprintf "(%s)" (program p)
  let formula_decl (c, f) = sprintf "%s := %s." (callable c) (formula f)
  let program_decl (c, p) = sprintf "%s := %s." (callable c) (program p)
  let file (fds, pds, m) = sprintf "formula:\n%s\nprogram:\n%s\nmain:\n%s." (Print.unlines formula_decl fds) (Print.unlines program_decl pds) (callable m)
end

module CMap = Map.Make (struct type t = T.callable let compare = compare end)
module SMap = Map.Make (String)
module TupleSet = Set.Make (struct type t = string list let compare = compare end)
let unions ls = List.fold_left TupleSet.union TupleSet.empty ls

let turn_to_int str =
  try Some (int_of_string str) with
  | _ -> eprintf "Warning: cannot convert '%s' to int\n" str; None
let add_strings es =
  (*let ints = try List.map int_of_string es with exn -> eprintf "%s\n" (Printexc.to_string exn); assert false in*)
  let ints = List.filter_map turn_to_int es in
  let int = Misc.sum ints in
  string_of_int int

let mult_strings es =
  let ints = List.filter_map turn_to_int es in
  let int = Misc.product ints in
  string_of_int int

let max_strings es =
  let ints = List.filter_map turn_to_int es in
  let int = List.fold_left max min_int ints in
  string_of_int int

let min_strings es =
  let ints = List.filter_map turn_to_int es in
  let int = List.fold_left min max_int ints in
  string_of_int int

let subtract_strings e es =
  try
    let i1 = int_of_string e
    and i2 = int_of_string (add_strings es) in
    string_of_int (i1 - i2)
  with exn -> eprintf "%s\n" (Printexc.to_string exn); assert false

let perform_eop eop is = match eop with
  | Ast.T.Add -> add_strings is
  | Ast.T.Mult -> mult_strings is
  | Ast.T.Max -> max_strings is
  | Ast.T.Min -> min_strings is

let rec set gmap vmap : Ast.T.set -> TupleSet.t = function
  | Ast.T.Set (ts, vs) ->
     let maps = vdecls gmap vmap vs in
     let treat_tuple t : TupleSet.t = unions (List.map (fun m -> tuple gmap m t) maps) in
     unions (List.map treat_tuple ts)
 | Ast.T.Name c -> CMap.find (callable gmap vmap c) gmap (* SMap.find c gmap*)
and vdecls gmap vmap : Ast.T.vdecls -> Ast.T.constant SMap.t list =
  let f vmaps vdec =
  List.concat_map (fun vmap -> vdecl gmap vmap vdec) vmaps in
  List.fold_left f [vmap]

and vdecl gmap vmap = function
  | Ast.T.FromSet (names, s) ->
     let n = List.length names in
     let tuples = set gmap vmap s in
     let aux vmapo name value = match vmapo with
       | None -> None
       | Some m -> match SMap.find_opt name m with
         | None -> Some (SMap.add name value m)
         | Some v -> if v = value then Some vmap else None in
     let make_map t =
       if List.length t <> n then None
       else
         List.fold_left2 aux (Some vmap) names t in
     List.filter_map make_map (TupleSet.elements tuples)
and tuple gmap vmap : Ast.T.tuple -> TupleSet.t  = function
  | Ast.T.Tuple es -> TupleSet.singleton (List.map (expr gmap vmap) es)
  | Ast.T.Range (e1, e2) ->
     let e1, e2 = (expr gmap vmap e1, expr gmap vmap e2) in
     let i1, i2 = int_of_string e1, int_of_string e2 in
     let l = List.map (fun i -> [string_of_int i]) (Misc.range i1 (i2+1)) in
     TupleSet.of_list l

and expr gmap vmap : Ast.T.expr -> Ast.T.constant = function
  | Ast.T.Var n -> if not (SMap.mem n vmap) then failwith (sprintf "unknown %s\n" n); assert (SMap.mem n vmap); SMap.find n vmap
  | Const c -> c
  | Int i -> string_of_int i
  | ListE (eop, es) -> perform_eop eop (List.map (expr gmap vmap) es)
  | VarE (eop, vs, e) ->
     let vmaps = vdecls gmap vmap vs in
     let ints = List.map (fun m -> expr gmap m e) vmaps in
     perform_eop eop ints
  | Subtract (e, es) -> subtract_strings (expr gmap vmap e) (List.map (expr gmap vmap) es)

and callable gmap vmap (name, es) = (name, List.map (expr gmap vmap) es)

let rec formula gmap vmap : Ast.T.formula -> T.formula = function
  | Ast.T.CallF c -> CallF (callable gmap vmap c)
  | Top -> Top
  | Neg f -> Neg (formula gmap vmap f)
  | ListF (fop, fs) -> ListF (fop, List.map (formula gmap vmap) fs)
  | VarF (fop, vs, f) ->
     let vmaps = vdecls gmap vmap vs in
     let fs = List.map (fun m -> formula gmap m f) vmaps in
     ListF (fop, fs)
  | Diamond (p, f) -> Diamond (program gmap vmap p, formula gmap vmap f)

and program gmap vmap : Ast.T.program -> T.program = function
  | Ast.T.CallP c -> CallP (callable gmap vmap c)
  | Assign (c, f) -> Assign (callable gmap vmap c, formula gmap vmap f)
  | Test f -> Test (formula gmap vmap f)
  | ListP (pop, ps) -> ListP (pop, List.map (program gmap vmap) ps)
  | VarP (pop, vs, p) ->
     let vmaps = vdecls gmap vmap vs in
     let ps = List.map (fun m -> program gmap m p) vmaps in
     ListP (pop, ps)
  | Converse p -> Converse (program gmap vmap p)
  | Kleene p -> Kleene (program gmap vmap p)

let make_decl aux gmap ((vs, c, a) : 'a Ast.T.decl) : 'b T.decl list =
  let vmaps = vdecls gmap SMap.empty vs in
  List.map (fun m -> (callable gmap m c, aux gmap m a)) vmaps

let make_decl_set gmap ((vs, c, a) : Ast.T.set Ast.T.decl) : TupleSet.t CMap.t =
  let vmaps = vdecls gmap SMap.empty vs in
  let aux gm vmap =
    CMap.add (callable gm vmap c) (set gm vmap a) gm in
  List.fold_left aux gmap vmaps

let add_decl map (cname, constants) =
  assert (not (SMap.mem cname map));
  SMap.add cname constants map

let file ((grounds, formulas, programs, main) : Ast.T.file) : T.file =
  let gmap = List.fold_left make_decl_set CMap.empty grounds in
(*  eprintf "%s\n" (P.unlines (P.couple Print.callable (fun s -> P.list (P.list' "(" ", " ")" (fun x -> x)) (TupleSet.elements s))) (CMap.bindings gmap));*)
  let fs = List.concat_map (make_decl formula gmap) formulas
  and ps = List.concat_map (make_decl program gmap) programs
  and m = callable gmap SMap.empty main in
  (fs, ps, m)
