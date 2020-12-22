{
  module P = Parser
  exception Error of string

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum }

  let print_error lexer =
    let position = Lexing.lexeme_start_p lexer in
    let line = position.Lexing.pos_lnum
    and char = position.Lexing.pos_cnum - position.Lexing.pos_bol in
    Printf.sprintf "Lexer : at line %d, column %d: unexpected character.\n%!" line char

  let string_of_char c = Printf.sprintf "%c" c
}

let digit = ['0' - '9']
let lcase = ['a' - 'z']
let ucase = ['A' - 'Z']
(*let letter = lcase | ucase | digit | ['_'] | ['-'] | ['+'] | ['<'] | ['>'] | ['*'] | ['/'] | ['='] | ['@'] | ['#']*)
let letter = lcase | ucase | digit | ['_']
let vname = ucase letter*
let cname = lcase letter*
let integer = digit+
let linefeed = "\r\n" | ['\n' '\r']

rule line_comment = parse
  | ([^'\n']* '\n') { incr_linenum lexbuf; token lexbuf }
  | ([^'\n']* eof) { P.EOF }
  | _   { raise (Error (print_error lexbuf)) }

and token = parse
  | '(' { P.LPAREN   } | ')' { P.RPAREN   } | '[' { P.LBRACKET } | ']' { P.RBRACKET } | '{' { P.LBRACE   } | '}' { P.RBRACE   } | '<' { P.LANGLE   } | '>' { P.RANGLE   }
  | ".." { P.RANGE }
  | '+' { P.PLUS } | '-' { P.MINUS }
  | "\\neg" { P.NEG }
  | "<-" { P.ASSIGN } | "?" { P.TEST }  | "^" { P.CONVERSE } | "*" { P.STAR }
  | ',' { P.COMMA } | ":=" { P.DEFINE } | ':' { P.COLON } | '.' { P.DOT }
  | "grounding" { P.GROUND } | "formula" { P.FORMULA } | "program" { P.PROGRAM } | "main" { P.MAIN }
  | [' ' '\t'] { token lexbuf }
  | linefeed   { incr_linenum lexbuf; token lexbuf }
  | vname as n { P.VNAME n }
  | cname as n { P.CNAME n }
  | "\\in" { P.IN }
  | "\\forall" { P.FORALL }
  | "\\bigor" { P.BIGDISJ } | "\\bigand" { P.BIGCONJ } | "\\bigseq" { P.BIGSEQ } | "\\bigcup" { P.BIGNONDET }
  | "\\or" { P.DISJ } | "\\and" { P.CONJ } | "\\seq" { P.SEQ } | "\\cup" { P.NONDET }
  | "\\top" { P.TOP }
(*  | "\\|" { P.BIGDISJ } | "\\&" { P.BIGCONJ } | "\\;" { P.BIGSEQ } | "\\!" { P.BIGNONDET }
  | "|" { P.DISJ } | "&" { P.CONJ } | ";" { P.SEQ } | "!" { P.NONDET }*)
  | integer as i { P.INT (int_of_string i) }
  | eof  { P.EOF }
  | "(*" { comment lexbuf }
  | "%"  { line_comment lexbuf }
  | _    { raise (Error (print_error lexbuf)) }
  (*| variable as v { let v' = Scanf.sscanf v "?%s" (fun s -> s) in P.VARIABLE v' }*)

and comment = parse
  | "*)"     { token lexbuf }
  | linefeed { incr_linenum lexbuf; comment lexbuf }
  | _        { comment lexbuf }
