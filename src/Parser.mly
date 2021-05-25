%token <string> VNAME
%token <string> CNAME
%token <int> INT
%token IN FORALL BIGCONJ BIGDISJ BIGSEQ BIGNONDET BIGPLUS
%token BOT TOP NEG CONJ DISJ LANGLE RANGLE
%token ASSIGN TEST SEQ NONDET CONVERSE STAR
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DEFINE COLON COMMA DOT RANGE PLUS MINUS
%token GROUND FORMULA PROGRAM MAIN
%token EOF

%{
(*let nest_quants l phi =
  let rec aux accu = function
    | [] -> accu
    | a :: r -> aux (Ast.QuantF (a, accu)) r in
  aux phi (List.rev l)*)
%}
(*%type <(Ast.keyword, Ast.free) Ast.atomic> keyword_atomic*)
%type <Ast.file> file
%start file
%%

separated_many_list(Sep, Sub):
| a = Sub Sep r = separated_nonempty_list(Sep, Sub) { a :: r }

separated_many_slist(Sep, Sub):
| a = Sub s = Sep r = separated_nonempty_list(Sep, Sub) { (s, a :: r) }

gvalue:
| i = INT RANGE j = INT { List.map string_of_int (Misc.range i (j+1)) }
| c = CNAME { [c]  }
| i = INT { [string_of_int i]  }

set:
LBRACE cs = separated_nonempty_list(COMMA, gvalue) RBRACE { List.flatten cs }

ground:
| n = CNAME { Ast.CallG n }
| cs = set { Ast.Set cs }

ground_decl: | g = CNAME DEFINE cs = set DOT { (g, cs) }

var_decl: | vs = nonempty_list(VNAME) IN g = ground { List.map (fun v -> (v, g)) vs }
var_decls(Operator): o = Operator l = separated_nonempty_list(COMMA, var_decl) COLON { (o, List.flatten l) }

doperator: | FORALL { ()  }
eoperator: | PLUS { Ast.Add } | STAR { Ast.Mult }
foperator: | CONJ { Ast.Conj } | DISJ { Ast.Disj }
poperator: | SEQ { Ast.Seq } | NONDET { Ast.U }
beoperator: | BIGPLUS { Ast.Add }
bfoperator: | BIGCONJ { Ast.Conj } | BIGDISJ { Ast.Disj }
bpoperator: | BIGSEQ { Ast.Seq } | BIGNONDET { Ast.U }

expr:
| l = separated_many_slist(eoperator, inner_expr) { Ast.ListE l }
| l = separated_many_slist(MINUS, inner_expr) { Ast.Subtract (List.hd (snd l), List.tl (snd l)) }
| vs = var_decls(beoperator) f = expr { Ast.VarE (fst vs, snd vs, f) }
| e = inner_expr { e }

inner_expr:
| LPAREN e = expr RPAREN { e }
| n = VNAME { Ast.Var n }
| n = CNAME { Ast.Const n }
| i = INT { Ast.Int i }

callable:
| n = CNAME LPAREN l = separated_list(COMMA, expr) RPAREN { (n, l) }
| n = CNAME { (n, []) }

formula:
| f = inner_formula { f }
| l = separated_many_slist(foperator, inner_formula) { Ast.ListF l }
| vs = var_decls(bfoperator) f = formula { Ast.VarF (fst vs, snd vs, f) }
outer_formula:
| c = callable { Ast.CallF c }
| TOP { Ast.Top}
| BOT { Ast.Neg (Ast.Top) }
| NEG f = inner_formula { Ast.Neg f }
| LANGLE   p = program RANGLE   f = inner_formula { Ast.Diamond (p, f) }
| LBRACKET p = program RBRACKET f = inner_formula { Ast.Neg (Ast.Diamond (p, Ast.Neg f)) }
inner_formula:
| LPAREN l = separated_many_slist(foperator, inner_formula) RPAREN { Ast.ListF l }
| LPAREN vs = var_decls(bfoperator) f = formula RPAREN { Ast.VarF (fst vs, snd vs, f) }
| LPAREN f = outer_formula RPAREN { f }
| f = outer_formula { f }
(*inner_formula:
| f = formula { f }
formula:
| LPAREN f = formula RPAREN { f }
| c = callable { Ast.CallF c }
| TOP { Ast.Top}
| BOT { Ast.Neg (Ast.Top) }
| NEG f = formula { Ast.Neg f }
| LPAREN NEG f = formula RPAREN { Ast.Neg f }
| l = separated_many_slist(foperator, formula) { Ast.ListF l }
| vs = var_decls(bfoperator) f = formula { Ast.VarF (fst vs, snd vs, f) }
| LANGLE   p = program RANGLE f   = formula { Ast.Diamond (p, f) }
| LBRACKET p = program RBRACKET f = formula { Ast.Neg (Ast.Diamond (p, Ast.Neg f)) }*)

program:
| p = outer_program { p }
| p = inner_program { p }
outer_program:
| l = separated_many_slist(poperator, inner_program) { Ast.ListP l }
| vs = var_decls(bpoperator) p = program { Ast.VarP (fst vs, snd vs, p) }
inner_program:
| LPAREN p = outer_program RPAREN { p }
| c = callable { Ast.CallP c }
| c = callable ASSIGN f = formula { Ast.Assign (c, f) }
| f = formula TEST { Ast.Test f }
| p = inner_program CONVERSE { Ast.Converse p }
| p = inner_program STAR { Ast.Kleene p }

formula_decl:
| vs = var_decls(doperator) c = callable DEFINE f = formula DOT { (snd vs, c, f) }
| c = callable DEFINE f = formula DOT { ([], c, f) }
program_decl:
| vs = var_decls(doperator) c = callable DEFINE p = program DOT { (snd vs, c, p) }
| c = callable DEFINE p = program DOT { ([], c, p) }

main_decl:
c = callable DOT { c }

file: GROUND COLON gs = list(ground_decl) FORMULA COLON fs = list(formula_decl) PROGRAM COLON ps = list(program_decl) MAIN COLON m = main_decl EOF { (gs, fs, ps, m) }
