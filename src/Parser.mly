%token <string> VNAME
%token <string> CNAME
%token <int> INT
%token IN FORALL BIGCONJ BIGDISJ BIGSEQ BIGNONDET
%token BOT TOP NEG CONJ DISJ LANGLE RANGLE
%token ASSIGN TEST SEQ NONDET CONVERSE STAR
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DEFINE COLON COMMA DOT RANGE MID
%token PLUS MINUS (*eop*)
%token EQ NEQ LEQ GEQ
%token BIGPLUS BIGMULT MAX MIN (*beop*)
%token GROUND FORMULA PROGRAM MAIN
%token EOF

%{
(*module Ast = Ast.T*)
(*let nest_quants l phi =
  let rec aux accu = function
    | [] -> accu
    | a :: r -> aux (Ast.QuantF (a, accu)) r in
  aux phi (List.rev l)*)
%}
(*%type <(Ast.keyword, Ast.free) Ast.atomic> keyword_atomic*)
%type <Ast.T.file> file
%start file
%%

separated_many_list(Sep, Sub):
| a = Sub Sep r = separated_nonempty_list(Sep, Sub) { a :: r }

separated_many_slist(Sep, Sub):
| a = Sub s = Sep r = separated_nonempty_list(Sep, Sub) { (s, a :: r) }

doperator: | FORALL { ()  }
eoperator: | PLUS { Ast.T.Add } | STAR { Ast.T.Mult }
foperator: | CONJ { Ast.T.Conj } | DISJ { Ast.T.Disj }
poperator: | SEQ { Ast.T.Seq } | NONDET { Ast.T.U }
roperator: | EQ { Ast.T.Eq } | NEQ { Ast.T.Neq } | LANGLE { Ast.T.Lt } | RANGLE { Ast.T.Gt } | LEQ { Ast.T.Leq } | GEQ { Ast.T.Geq }
beoperator: | BIGPLUS { Ast.T.Add } | BIGMULT { Ast.T.Mult } | MAX { Ast.T.Max } | MIN { Ast.T.Min }
bfoperator: | BIGCONJ { Ast.T.Conj } | BIGDISJ { Ast.T.Disj }
bpoperator: | BIGSEQ { Ast.T.Seq } | BIGNONDET { Ast.T.U }

set:
| LBRACE cs = separated_nonempty_list(COMMA, tuple) RBRACE { Ast.T.Set (cs, []) }
| LBRACE cs = separated_nonempty_list(COMMA, tuple) MID vs = vdecls RBRACE { Ast.T.Set (cs, vs) }
| c = callable { Ast.T.Name c }

vdecl:
| LPAREN vs = separated_nonempty_list(COMMA, VNAME) RPAREN IN s = set { Ast.T.FromSet (vs, s) }
| v = VNAME IN s = set { Ast.T.FromSet ([v], s) }
| LPAREN e1 = expr r = roperator e2 = expr RPAREN { Ast.T.Relation (r, e1, e2) }

vdecls:
| l = separated_nonempty_list(COMMA, vdecl) { l }

tuple:
| LPAREN e = expr COMMA es = separated_nonempty_list(COMMA, expr) RPAREN { Ast.T.Tuple (e :: es)  }
| e = expr { Ast.T.Tuple [e] }
| e1 = expr RANGE e2 = expr { Ast.T.Range (e1, e2) }

expr:
| l = separated_many_slist(eoperator, inner_expr) { Ast.T.ListE l }
| l = separated_many_slist(MINUS, inner_expr) { Ast.T.Subtract (List.hd (snd l), List.tl (snd l)) }
| o = beoperator vs = vdecls COLON f = expr { Ast.T.VarE (o, vs, f) }
| e = inner_expr { e }

inner_expr:
| LPAREN e = expr RPAREN { e }
| n = VNAME { Ast.T.Var n }
| n = CNAME { Ast.T.Const n }
| i = INT { Ast.T.Int i }
| MINUS i = INT { Ast.T.Int (-i) }

callable:
| n = CNAME LPAREN l = separated_list(COMMA, expr) RPAREN { (n, l) }
| n = CNAME { (n, []) }

decl(A):
| doperator vs = vdecls COLON c = callable DEFINE a = A DOT { (vs, c, a) }
| c = callable DEFINE a = A DOT { ([], c, a) }

formula:
| f = inner_formula { f }
outer_formula:
| c = callable { Ast.T.CallF c }
| TOP { Ast.T.Top}
| BOT { Ast.T.Neg (Ast.T.Top) }
| NEG f = outer_formula { Ast.T.Neg f }
| LANGLE   p = program RANGLE   f = outer_formula { Ast.T.Diamond (p, f) }
| LBRACKET p = program RBRACKET f = outer_formula { Ast.T.Neg (Ast.T.Diamond (p, Ast.T.Neg f)) }
| LPAREN f = inner_formula RPAREN { f }
inner_formula:
| l = separated_many_slist(foperator, outer_formula) { Ast.T.ListF l }
| o = bfoperator vs = vdecls COLON f = inner_formula { Ast.T.VarF (o, vs, f) }
| f = outer_formula { f }

program:
| p = inner_program { p }
outer_program:
| c = callable { Ast.T.CallP c }
| c = callable ASSIGN f = formula { Ast.T.Assign (c, f) }
| TEST f = inner_formula TEST { Ast.T.Test f }
| p = outer_program CONVERSE { Ast.T.Converse p }
| p = outer_program STAR { Ast.T.Kleene p }
| LPAREN p = inner_program RPAREN { p }
inner_program:
| l = separated_many_slist(poperator, outer_program) { Ast.T.ListP l }
| o = bpoperator vs = vdecls COLON p = inner_program { Ast.T.VarP (o, vs, p) }
| p = outer_program { p }

main_decl:
c = callable DOT { c }

file: GROUND COLON gs = list(decl(set)) FORMULA COLON fs = list(decl(formula)) PROGRAM COLON ps = list(decl(program)) MAIN COLON m = main_decl EOF { (gs, fs, ps, m) }
