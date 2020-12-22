open Printf

type callable = Ast.cname * Ast.constant list
type formula = CallF of callable | Top | Neg of formula | ListF of (Ast.foperator * formula list) | Diamond of program * formula
and program  = CallP of callable | Assign of (callable * formula) | Test of formula | ListP of (Ast.poperator * program list) | Converse of program | Kleene of program

type formula_decl = callable * formula
type program_decl = callable * program
type file = formula_decl list * program_decl list * callable

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

module SMap = Map.Make (String)

let add_strings es =
  let ints = try List.map int_of_string es with exn -> eprintf "%s\n" (Printexc.to_string exn); assert false in
  let int = Misc.sum ints in
  string_of_int int

let mult_strings es =
  let ints = try List.map int_of_string es with exn -> eprintf "%s\n" (Printexc.to_string exn); assert false in
  let int = Misc.product ints in
  string_of_int int

let subtract_strings e es =
  try
    let i1 = int_of_string e
    and i2 = int_of_string (add_strings es) in
    string_of_int (i1 - i2)
  with exn -> eprintf "%s\n" (Printexc.to_string exn); assert false

let var_decls gmap m l =
  let add_var maps (name, ground) =
    let constants = match ground with
      | Ast.CallG gname ->
         assert (SMap.mem gname gmap);
         SMap.find gname gmap
      | Ast.Set cs -> cs in
    let add_const const = List.map (SMap.add name const) maps in
    List.concat_map add_const constants in
  List.fold_left add_var [m] l

let perform_eop eop is = match eop with
  | Ast.Add -> add_strings is
  | Ast.Mult -> mult_strings is
let rec expr gmap vmap = function
  | Ast.Var n -> assert (SMap.mem n vmap); SMap.find n vmap
  | Ast.Const c -> c
  | Ast.Int i -> string_of_int i
  | Ast.ListE (eop, es) -> perform_eop eop (List.map (expr gmap vmap) es)
  | Ast.VarE (eop, vs, e) ->
     let vmaps = var_decls gmap vmap vs in
     let ints = List.map (fun m -> expr gmap m e) vmaps in
     perform_eop eop ints
  | Ast.Subtract (e, es) -> subtract_strings (expr gmap vmap e) (List.map (expr gmap vmap) es)

let callable gmap vmap (name, es) = (name, List.map (expr gmap vmap) es)

let rec formula gmap vmap = function
  | Ast.CallF c -> CallF (callable gmap vmap c)
  | Ast.Top -> Top
  | Ast.Neg f -> Neg (formula gmap vmap f)
  | Ast.ListF (fop, fs) -> ListF (fop, List.map (formula gmap vmap) fs)
  | Ast.VarF (fop, vs, f) ->
     let vmaps = var_decls gmap vmap vs in
     let fs = List.map (fun m -> formula gmap m f) vmaps in
     ListF (fop, fs)
  | Ast.Diamond (p, f) -> Diamond (program gmap vmap p, formula gmap vmap f)

and program gmap vmap = function
  | Ast.CallP c -> CallP (callable gmap vmap c)
  | Ast.Assign (c, f) -> Assign (callable gmap vmap c, formula gmap vmap f)
  | Ast.Test f -> Test (formula gmap vmap f)
  | Ast.ListP (pop, ps) -> ListP (pop, List.map (program gmap vmap) ps)
  | Ast.VarP (pop, vs, p) ->
     let vmaps = var_decls gmap vmap vs in
     let ps = List.map (fun m -> program gmap m p) vmaps in
     ListP (pop, ps)
  | Ast.Converse p -> Converse (program gmap vmap p)
  | Ast.Kleene p -> Kleene (program gmap vmap p)

let formula_decl gmap (vs, c, f) =
  let vmaps = var_decls gmap SMap.empty vs in
  List.map (fun m -> (callable gmap m c, formula gmap m f)) vmaps
let program_decl gmap (vs, c, p) =
  let vmaps = var_decls gmap SMap.empty vs in
  List.map (fun m -> (callable gmap m c, program gmap m p)) vmaps

let add_decl map (cname, constants) =
  assert (not (SMap.mem cname map));
  SMap.add cname constants map

let file (grounds, formulas, programs, main) =
  let gmap = List.fold_left add_decl SMap.empty grounds in
  let fs = List.concat_map (formula_decl gmap) formulas
  and ps = List.concat_map (program_decl gmap) programs
  and m = callable gmap SMap.empty main in
  (fs, ps, m)
