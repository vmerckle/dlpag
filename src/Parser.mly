%token <string> VNAME
%token <string> CNAME
%token <int> INT
%token IN NOTIN
%token FORALL BIGCONJ BIGDISJ BIGSEQ BIGNONDET
%token BOT TOP NEG CONJ DISJ LANGLE RANGLE
%token ASSIGN TEST SEQ NONDET CONVERSE STAR
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DEFINE COLON COMMA DOT RANGE MID WHERE
%token PLUS MULT MINUS (*eop*)
%token EQ NEQ LEQ GEQ
%token BIGPLUS BIGMULT MAX MIN (*beop*)
%token UNION INTERSECT SETMINUS (* sop *)
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

%inline soption(X):
|        { "" } /* nothing */
| x = X  { x }

separated_many_slist(Sep, Sub):
| a = Sub s = Sep r = separated_nonempty_list(Sep, Sub) { (s, a, r) }

tuple_list(X):
| LPAREN l = separated_nonempty_list(COMMA, X) RPAREN { l }
otuple_list(X):
| { [] }
| l = tuple_list(X) { l }
ttuple_list(X):
| LPAREN x = X COMMA l = separated_nonempty_list(COMMA, X) RPAREN { x :: l }

doperator: | FORALL { ()  }
eoperator: | PLUS { Ast.T.Add } | MULT { Ast.T.Mult }
foperator: | CONJ { Ast.T.Conj } | DISJ { Ast.T.Disj }
poperator: | SEQ { Ast.T.Seq } | NONDET { Ast.T.U }
roperator: | EQ { Ast.T.Eq } | NEQ { Ast.T.Neq } | LANGLE { Ast.T.Lt } | RANGLE { Ast.T.Gt } | LEQ { Ast.T.Leq } | GEQ { Ast.T.Geq }
soperator: | UNION { Ast.T.Union } | INTERSECT { Ast.T.Intersect } | SETMINUS { Ast.T.Setminus }
beoperator: | BIGPLUS { Ast.T.Add } | BIGMULT { Ast.T.Mult } | MAX { Ast.T.Max } | MIN { Ast.T.Min }
bfoperator: | BIGCONJ { Ast.T.Conj } | BIGDISJ { Ast.T.Disj }
bpoperator: | BIGSEQ { Ast.T.Seq } | BIGNONDET { Ast.T.U }

pure_term:
| name = CNAME vs = otuple_list(pure_term) { Ast.T.Fun (name, vs) }
| vs = ttuple_list(pure_term) { Ast.T.Fun ("", vs) }
| v = VNAME { Ast.T.Var v }

term:
| name = CNAME ts = otuple_list(term) { Ast.T.Fun (name, ts)  }
| ts = ttuple_list(term) { Ast.T.Fun ("", ts)  }
| v = VNAME { Ast.T.Var v }
| e = cexpr { Ast.T.Int e }

set:
| s = inner_set { s }

outer_set:
| LBRACE cs = separated_nonempty_list(COMMA, tuple) vs = loption(MID vs = vdecls { vs }) RBRACE { Ast.T.Set (cs, vs) }
| c = callable { Ast.T.Name c }
| RPAREN s = inner_set RPAREN { s }

inner_set:
| l = separated_many_slist(soperator, outer_set) { Ast.T.List l }
| s = outer_set { s }

constraints:
| t1 = term r = roperator t2 = term { Ast.T.Relation (r, t1, t2) }
| t = term NOTIN s = set { Ast.T.Notin (t, s)}

vdecl:
| vs = pure_term IN s = set { Ast.T.FromSet (vs, s) }
| WHERE c = constraints { Ast.T.Constraint c }

vdecls:
| l = separated_nonempty_list(COMMA, vdecl) { l }

tuple:
| t = term { Ast.T.Term t  }
| e1 = expr RANGE e2 = expr { Ast.T.Range (e1, e2) }

cexpr:
| l = separated_many_slist(eoperator, inner_expr) { Ast.T.ListE l }
| l = separated_many_slist(MINUS, inner_expr) { let ((), h, r) = l in Ast.T.Subtract (h, r) }
| o = beoperator vs = vdecls COLON f = expr { Ast.T.VarE (o, vs, f) }
| LPAREN e = expr RPAREN { e }
| i = INT { Ast.T.Int i }
| MINUS i = INT { Ast.T.Int (-i) }
expr:
| l = separated_many_slist(eoperator, inner_expr) { Ast.T.ListE l }
| l = separated_many_slist(MINUS, inner_expr) { let ((), h, r) = l in Ast.T.Subtract (h, r) }
| o = beoperator vs = vdecls COLON f = expr { Ast.T.VarE (o, vs, f) }
| e = inner_expr { e }

inner_expr:
| LPAREN e = expr RPAREN { e }
| n = VNAME { Ast.T.Var n }
| i = INT { Ast.T.Int i }
| MINUS i = INT { Ast.T.Int (-i) }

callable:
| n = CNAME ts = otuple_list(term) { (n, ts) }

decl(A):
| vs = loption(doperator vs = vdecls COLON { vs }) c = callable DEFINE a = A DOT { (vs, c, a) }

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
