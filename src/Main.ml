open Printf

let update reference value = reference := value
let get () =
(*  let debug = debug_false () in
  let verbose = ref 0 in
  let asp = ref false in
  let output_name = ref "" in
  let speclist =
    [("--verbose",        Arg.Set_int verbose, "Enable verbose mode.", sprintf "%d" !verbose);
     ("-o",               Arg.Set_string output_name, "Set the output path.", !output_name);
     ("--direction",      Arg.Symbol (directions, set_direction), "Set the proof direction.", sprintf "%s" (Opt.show_proof_direction !direction));
     ("--asp",            Arg.Bool (update asp), "Output ASP instead of FOL.", sprintf "%B" !asp);
     ("-",                Arg.Unit (fun () -> update input_names ("-" :: !input_names)), "Read the GDL from the standard input.", "Disabled");
    ] in*)
  let ground = ref false in
  let speclist = [("--ground", Arg.Bool (update ground), "Ground rather than Solve.")] in
(*  let speclist = [] in*)
  let usage_msg = "DL-PA Grounder. Options available:" in
  let input_names = ref [] in
  let add_name s = input_names := s :: !input_names in
  Arg.parse speclist add_name usage_msg;
  match !input_names with
  | n :: [] -> (!ground, n)
  | _ :: _ :: _ | [] -> failwith "Wrong number of arguments. Usage: dlpag file"


let convert g =
  let formula = Formula.file g in
(*  let qbf = QBF.form formula in
  let qcir = QBF.to_qcir qbf in*)
  let qcir = QBF.model_checking_empty formula in
  let qcir = QCIR.sanitize_names qcir in
  (*eprintf "%s\n" (Ast.Print.file p);*)
  (*eprintf "%s\n" (Ground.Print.file g);*)
  (*printf "%s\n" (Formula.Print.formula formula);*)
  (*printf "%s\n" (QBF.Print.formula qbf);*)
  printf "%s\n" (QCIR.Print.file qcir);
  ()

let ground g =
  let f = Formula.file g in
  let d = Desugar.formula f in
  printf "%s\n" (Desugar.Print.formula d)

module S = Solve
let solve g =
  let b = Solve.model_checking g [] in
  printf "%B\n" b

let start () =
  let gr, f = get () in
  let p = Parse.from_file () f in
  let g = Circuit.file p in
  if gr then ground g else solve g
  (*if gr then convert g else solve g*)

let _ = start ()

