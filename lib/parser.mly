
%token <string> IDENTIFIER
%token <Lang.attrib_tp> TP
%token <bool> BCONSTANT
%token <int> INTCONSTANT
%token <string> STRINGCONSTANT
%token BLAND BLOR
%token EQ GE GT LE LT NE
%token ADD SUB MUL DIV MOD
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN 
%token DOT COMMA COLON
%token CREATE DELETE MATCH RETURN SET WHERE
%token ARROW
%token EOF

%start<Lang.prog> main

%left BLOR
%left BLAND
%left EQ GE GT LE LT NE
%left ADD SUB
%left MUL DIV MOD

%{ open Lang %}

%%

main: prog EOF { $1 }

prog: td = list(tpDecl);  q = query 
     { let (nts, rts) = List.partition_map Fun.id td in Prog (DBG(nts, rts), q) }

tpDecl:
| n = nodeTpDecl { Either.Left n }
| r = relTpDecl { Either.Right r }


query: cls = list(clause) { Query cls }

/* TODO: to be completed */
clause: 
| CREATE; pts = separated_list(COMMA, pattern) { Create pts }
| DELETE; pts = delete_pattern { Delete pts}
| MATCH; pts = separated_list(COMMA, pattern) { Match pts }
| RETURN; v = separated_list(COMMA, IDENTIFIER) { Return v }
| SET; attr = separated_list(COMMA, attrib_assign_pattern) { Set attr }
| WHERE; cond = expr { Where cond }

/* TODO: to be completed */
pattern: 
| np = npattern { SimpPattern np }
| np = npattern; SUB; LBRACKET; COLON; lab = IDENTIFIER; RBRACKET; ARROW; p = pattern { CompPattern(np, lab, p) }


npattern: 

| LPAREN; v = IDENTIFIER; COLON; t = IDENTIFIER; RPAREN { DeclPattern(v, t) }
| LPAREN; v = IDENTIFIER; RPAREN { VarRefPattern(v) }


delete_pattern:

| w = separated_list(COMMA, IDENTIFIER) {DeleteNodes w}
(*| x = separated_list(COMMA, rpattern) {DeleteRels x}*)

attrib_assign_pattern:

z = IDENTIFIER; DOT; attr = IDENTIFIER; EQ; exp = expr { (z, attr, exp) }



/* Expressions */

primary_expr:
| vn = IDENTIFIER; DOT; fn = IDENTIFIER 
     { AttribAcc(vn, fn) }
| c = BCONSTANT
     { Const(BoolV(c)) }
| c = INTCONSTANT
     { Const(IntV(c)) }
| c = STRINGCONSTANT
     { Const(StringV(c)) }
| LPAREN e = expr RPAREN
     { e }

/* TODO: to be completed */
expr:
| a = primary_expr { a }
| expr1 = expr; ADD; expr2=expr {BinOp(BArith BAadd, expr1, expr2) }
| expr1 = expr; SUB; expr2=expr {BinOp(BArith BAsub, expr1, expr2) }
| expr1 = expr; MUL; expr2=expr {BinOp(BArith BAmul, expr1, expr2) }
| expr1 = expr; DIV; expr2=expr {BinOp(BArith BAdiv, expr1, expr2) }
| expr1 = expr; MOD; expr2=expr {BinOp(BArith BAmod, expr1, expr2) }
| expr1 = expr; EQ; expr2=expr {BinOp(BCompar BCeq, expr1, expr2) }
| expr1 = expr; GE; expr2=expr {BinOp(BCompar BCge, expr1, expr2) }
| expr1 = expr; GT; expr2=expr {BinOp(BCompar BCgt, expr1, expr2) }
| expr1 = expr; LT; expr2=expr {BinOp(BCompar BClt, expr1, expr2) }
| expr1 = expr; LE; expr2=expr {BinOp(BCompar BCle, expr1, expr2) }
| expr1 = expr; NE; expr2=expr {BinOp(BCompar BCne, expr1, expr2) }
| expr1 = expr; BLAND; expr2=expr {BinOp(BLogic BLand, expr1, expr2) }
| expr1 = expr; BLOR; expr2=expr {BinOp(BLogic BLor, expr1, expr2) }


/* Types */
nodeTpDecl: LPAREN; COLON; i = IDENTIFIER; a = attrib_declList; RPAREN  { DBN (i, a) }

attrib_decl: i = IDENTIFIER; t = TP { (i, t) }
attrib_declList: 
| LBRACE; ads = separated_list(COMMA, attrib_decl); RBRACE { ads }


/* Relational type declarations of the form (:nt1) -[:rt]-> (:nt2)
 */
nodeTpRef: LPAREN; COLON; si = IDENTIFIER; RPAREN { si }
relTpDecl: si = nodeTpRef;
           SUB; LBRACKET; COLON; rlab = IDENTIFIER; RBRACKET; ARROW; 
           ti = nodeTpRef
           { Graphstruct.DBR (si, rlab, ti) }

%%
