open Printf

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
  let speclist = [] in
  let usage_msg = "DL-PA Grounder. Options available:" in
  let input_names = ref [] in
  let add_name s = input_names := s :: !input_names in
  Arg.parse speclist add_name usage_msg;
  match !input_names with
  | n :: [] -> n
  | _ :: _ :: _ | [] -> failwith "Wrong number of arguments. Usage: dlpag file"


let convert () =
  let f = get () in
  let p = Parse.from_file () f in
  let g = Circuit.file p in
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

module S = Solve
let solve () =
  let f = get () in
  let p = Parse.from_file () f in
  let g = Circuit.file p in
  let b = Solve.model_checking g [] in
  printf "%B\n" b

let _ = solve ()

