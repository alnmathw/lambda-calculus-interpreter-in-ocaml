open Sedlexing.Utf8
open Parser

exception Invalid_token

let whitespace = [%sedlex.regexp? Plus (' ' | '\n' | '\t')]

let lower_alpha = [%sedlex.regexp? 'a' .. 'z']

let number = [%sedlex.regexp? '0' .. '9']

let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_')]

let int = [%sedlex.regexp? Plus number]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | ident -> IDENT (lexeme buf)
  | int -> INT (lexeme buf |> int_of_string)
  | ':' -> COLON
  | '.' -> DOT
  | "->" -> ARROW
  | '(' -> LPARENS
  | ')' -> RPARENS
  | any -> if lexeme buf = "λ" then LAMBDA else raise Invalid_token
  | eof -> EOF
  | _ -> assert false

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string f string =
  provider (from_string string)
  |> MenhirLib.Convert.Simplified.traditional2revised f
 56  
13-parser-for-kids-in-ocaml-with-menhir/code/parser.mly
@@ -0,0 +1,56 @@
%{ open Typ
   open Expr %}
%token <string> IDENT
%token <int> INT
%token LAMBDA
%token COLON
%token DOT
%token ARROW

%token LPARENS
%token RPARENS
%token EOF

%start <Expr.expr option> expr_opt
%start <Typ.typ option> typ_opt

%%

let typ_opt :=
  | EOF; { None }
  | t = typ; EOF; { Some t }

let sub_typ ==
  | i = IDENT;
    { match i with | "int" -> TInt | _ -> failwith "invalid type" }
  | LPARENS; t = typ; RPARENS; { t }

let typ :=
  | sub_typ
  | t1 = sub_typ; ARROW; t2 = typ;
    { TArrow { param_typ = t1; body_typ = t2 } }

let expr_opt :=
  | EOF; { None }
  | e = expr; EOF; { Some e }

let terminal ==
  | i = INT; { Int i }
  | i = IDENT; { Variable i }

let abstraction ==
  | LAMBDA; p = IDENT; COLON; t = typ; DOT; e = expr;
    { Abstraction { param = p; param_typ = t; body = e } }

let sub_expr :=
  | terminal
  | LPARENS; e = expr; RPARENS; { e }

let application :=
  | sub_expr
  | e1 = application; e2 = sub_expr;
    { Application { funct = e1; argument = e2 } }

let expr :=
  | abstraction
  | application 