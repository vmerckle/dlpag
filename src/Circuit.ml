open Printf

include Types.CIRCUIT
open T

module P = Print
module Print =
struct
  let rec ground_term = function
    | Int i -> sprintf "%d" i
    | Fun (c, ts) -> sprintf "%s(%s)" c (Print.list' "" ", " "" ground_term ts)
  let callable (n, cs) = match cs with
    | [] -> Ast.Print.cname n
    | _ :: _ -> sprintf "%s(%s)" (Ast.Print.cname n) (Print.list' "" ", " "" ground_term cs)
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
module GTermSet = Set.Make (struct type t = ground_term let compare = compare end)
let unions ls = List.fold_left GTermSet.union GTermSet.empty ls

let subtract_strings i1 is =
  let i2 = Misc.sum is in
  (i1 - i2)

let perform_eop eop is = match eop with
  | Ast.T.Add -> Misc.sum is
  | Ast.T.Mult -> Misc.product is
  | Ast.T.Max -> List.fold_left max min_int is
  | Ast.T.Min -> List.fold_left min max_int is

let perform_rop rop v1 v2 = match rop with
  | Ast.T.Eq -> compare v1 v2 = 0
  | Ast.T.Neq -> compare v1 v2 <> 0
  | Ast.T.Lt -> compare v1 v2 < 0
  | Ast.T.Gt -> compare v1 v2 > 0
  | Ast.T.Leq -> compare v1 v2 <= 0
  | Ast.T.Geq -> compare v1 v2 >= 0

let perform_sop sop s1 s2 = match sop with
  | Ast.T.Union -> GTermSet.union s1 s2
  | Ast.T.Intersect -> GTermSet.inter s1 s2
  | Ast.T.Setminus -> GTermSet.diff s1 s2

let rec pure_term vmapo (gterm : ground_term) (pterm : Ast.T.pure_term) : ground_term SMap.t option = match (pterm, gterm) with
  | Ast.T.Var v, _ -> (match vmapo with | None -> None
                                        | Some m -> match SMap.find_opt v m with | Some gt -> if gt = gterm then Some m else None
                                                                                 | None -> Some (SMap.add v gterm m))
  | Ast.T.Fun (c, ts), Fun (c', ts') -> if c <> c' || List.length ts <> List.length ts' then None else List.fold_left2 pure_term vmapo ts' ts
  | Ast.T.Int i, Int j -> if i = j then vmapo else None
  | Ast.T.Fun _, Int _
  | Ast.T.Int _, Fun _ -> None
let rec set gmap vmap : Ast.T.set -> GTermSet.t = function
  | Ast.T.Set (ts, vs) ->
     let maps = vdecls gmap vmap vs in
     let treat_tuple t : GTermSet.t = unions (List.map (fun m -> tuple gmap m t) maps) in
     unions (List.map treat_tuple ts)
 | Ast.T.Name c -> CMap.find (callable gmap vmap c) gmap (* SMap.find c gmap*)
 | Ast.T.List (o, s, ss) ->
    let s' = set gmap vmap s
    and ss' = List.map (set gmap vmap) ss in
    List.fold_left (perform_sop o) s' ss'
and vdecls gmap vmap : Ast.T.vdecls -> ground_term SMap.t list =
  let f vmaps vdec =
  List.concat_map (fun vmap -> vdecl gmap vmap vdec) vmaps in
  List.fold_left f [vmap]

and vdecl gmap vmap = function
  | Ast.T.FromSet (term, s) ->
     let tuples = set gmap vmap s in
     let aux t = pure_term (Some vmap) t term in
     List.filter_map aux (GTermSet.elements tuples)
  | Ast.T.Constraint c -> let sat = constraints gmap vmap c in
                          if sat then [vmap] else []
and constraints gmap vmap = function
  | Ast.T.Relation (r, t1, t2) ->
     let t1 = term gmap vmap t1
     and t2 = term gmap vmap t2 in
     perform_rop r t1 t2
  | Ast.T.Notin (t, s) ->
     let tuples = set gmap vmap s in
     let t = term gmap vmap t in
     GTermSet.mem t tuples

and tuple gmap vmap : Ast.T.tuple -> GTermSet.t  = function
  | Ast.T.Term t -> GTermSet.singleton (term gmap vmap t)
  | Ast.T.Range (e1, e2) ->
     let i1, i2 = expr gmap vmap e1, expr gmap vmap e2 in
     let l = List.map (fun i -> Int i) (Misc.range i1 (i2+1)) in
     GTermSet.of_list l

and term gmap vmap = function
  | Fun (c, ts) -> Fun (c, List.map (term gmap vmap) ts)
  | Int e -> Int (expr gmap vmap e)
  | Ast.T.Var n -> if not (SMap.mem n vmap) then failwith (sprintf "unknown %s\n" n); assert (SMap.mem n vmap); SMap.find n vmap
and expr gmap vmap : Ast.T.expr -> int = function
  | Ast.T.Var n -> if not (SMap.mem n vmap) then failwith (sprintf "unknown %s\n" n); assert (SMap.mem n vmap); (match SMap.find n vmap with | Int i -> i | Fun _ as g -> failwith (sprintf "Variable %s ground to a non-int %s" n (Print.ground_term g)))
  | Int i -> i
  | ListE (eop, e, es) -> perform_eop eop (List.map (expr gmap vmap) (e :: es))
  | VarE (eop, vs, e) ->
     let vmaps = vdecls gmap vmap vs in
     let ints = List.map (fun m -> expr gmap m e) vmaps in
     perform_eop eop ints
  | Subtract (e, es) -> subtract_strings (expr gmap vmap e) (List.map (expr gmap vmap) es)
(*and int_expr gmap vmap e = match expr gmap vmap e with
  | Int i -> i
  | Fun _ as g -> failwith (sprintf "Expression %s ground to a non-int %s" (Ast.Print.expr e) (Print.ground_term g))*)

and callable gmap vmap (name, es) = (name, List.map (term gmap vmap) es)

let rec formula gmap vmap : Ast.T.formula -> T.formula = function
  | Ast.T.CallF c -> CallF (callable gmap vmap c)
  | Top -> Top
  | Neg f -> Neg (formula gmap vmap f)
  | ListF (fop, f, fs) -> ListF (fop, List.map (formula gmap vmap) (f :: fs))
  | VarF (fop, vs, f) ->
     let vmaps = vdecls gmap vmap vs in
     let fs = List.map (fun m -> formula gmap m f) vmaps in
     ListF (fop, fs)
  | Diamond (p, f) -> Diamond (program gmap vmap p, formula gmap vmap f)

and program gmap vmap : Ast.T.program -> T.program = function
  | Ast.T.CallP c -> CallP (callable gmap vmap c)
  | Assign (c, f) -> Assign (callable gmap vmap c, formula gmap vmap f)
  | Test f -> Test (formula gmap vmap f)
  | ListP (pop, p, ps) -> ListP (pop, List.map (program gmap vmap) (p :: ps))
  | VarP (pop, vs, p) ->
     let vmaps = vdecls gmap vmap vs in
     let ps = List.map (fun m -> program gmap m p) vmaps in
     ListP (pop, ps)
  | Converse p -> Converse (program gmap vmap p)
  | Kleene p -> Kleene (program gmap vmap p)

let make_decl aux gmap ((vs, c, a) : 'a Ast.T.decl) : 'b T.decl list =
  let vmaps = vdecls gmap SMap.empty vs in
  List.map (fun m -> (callable gmap m c, aux gmap m a)) vmaps

let make_decl_set gmap ((vs, c, a) : Ast.T.set Ast.T.decl) : GTermSet.t CMap.t =
  let vmaps = vdecls gmap SMap.empty vs in
  let aux gm vmap =
    CMap.add (callable gm vmap c) (set gm vmap a) gm in
  List.fold_left aux gmap vmaps

let add_decl map (cname, constants) =
  assert (not (SMap.mem cname map));
  SMap.add cname constants map

let file ((grounds, formulas, programs, main) : Ast.T.file) : T.file =
  let gmap = List.fold_left make_decl_set CMap.empty grounds in
(*  eprintf "%s\n" (P.unlines (P.couple Print.callable (fun s -> P.list (P.list' "(" ", " ")" (fun x -> x)) (GTermSet.elements s))) (CMap.bindings gmap));*)
  let fs = List.concat_map (make_decl formula gmap) formulas
  and ps = List.concat_map (make_decl program gmap) programs
  and m = callable gmap SMap.empty main in
  (fs, ps, m)
