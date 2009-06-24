%{
    
(** 
    expression parsing adapted from ECMA-262 
    http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-262.pdf 
*)

open Ast
open Symbol_table

let parse_error s =
    let pos = Parsing.symbol_start_pos() in
    print_string ("in file " ^ (Filename.basename pos.Lexing.pos_fname) ^ ": "^ s^" at line ");
    print_int pos.Lexing.pos_lnum;
    print_string " at columns ";
    print_int (Parsing.symbol_start() - pos.Lexing.pos_bol);
    print_string("-");
    print_int (Parsing.symbol_end() - pos.Lexing.pos_bol);
    print_string "\n";
    flush stdout
        
let get_env ()= 
    let pos=Parsing.symbol_start_pos() in
    (pos.Lexing.pos_fname,pos.Lexing.pos_lnum)

        
let resolve_import (filename, library, (inp_file, _))=
    let fullname=Filename_util.resolve_filename (Filename.dirname inp_file) filename
    in
       (fullname, {loaded=false})
        
let extract_stmt_list=function
    | StatementBlock(lst) -> lst
    | _ -> raise ( RuntimeError.InternalError "expected statement block" )
%}

%token<string> ID
%token <int> INT
%token <string> STRING
%token <float> REAL
%token <bool> BOOLEAN
%token <string> TEXT
%token <Ast.comparator> COMPOP
%token <bool> IMPORT

%token FOREACH WHILE IF  FOR ELSE TEMPLATE INSTRUCTIONS FUNCTION CONTINUE BREAK
%token RETURN IN ONCE WHEN VAR EOF LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMICOLON COLON DOTDOTDOT DOT EQUALS NOT QUESTION PLUS MINUS TIMES
%token DIVIDE MODULO AND OR VOID NAN SWITCH CASE DEFAULT PLUSEQUALS MINUSEQUALS
%token TIMESEQUALS DIVEQUALS MODEQUALS PLUSPLUS MINUSMINUS AT TRY CATCH THROW
%token FINALLY PROTOTYPE


%start program
%type <Ast.statement> program

/* resolve shift/reduce conflict for ELSE */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* resolve shift/reduce conflict for {} (map) and {} (empty block) */
%nonassoc MAP
%nonassoc EMPTYBLOCK

%right PLUSEQUALS MINUSEQUALS 
%right TIMESEQUALS DIVEQUALS MODEQUALS
%right EQUALS
%right COLON
%right QUESTION
%left OR
%left AND 
%left COMPOP
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT
%right UMINUS
%right PREFIX_INCDEC  
%left POSTFIX_INCDEC
%left ARR_INDEX

%%
program:
    | opt_statements EOF { Program($1) }
;
statements:                           
    | statement   { [$1] }
    | statement statements  { $1::$2 }
;
opt_statements:
    | statements  { $1 }
    | /*nothing*/ { [] }
;
statement_block:                      
    | LBRACE statements RBRACE  { StatementBlock($2) }
    | empty_statement_block     { $1 } 
;
empty_statement_block:
    | LBRACE RBRACE  { StatementBlock([]) }
; 
else_clause:                         
    | ELSE statement { $2 }
    | %prec LOWER_THAN_ELSE { Noop }
;
statement:                                    
    | IF LPAREN expression RPAREN statement else_clause { If($3,$5,$6,get_env()) }
    | expression SEMICOLON { ExpressionStatement($1, get_env()) }
    | SEMICOLON  { Noop }
    | statement_block { $1 }
    | FOREACH LPAREN ID IN expression RPAREN statement { ForEach($3,$5,$7,get_env()) }
    | WHILE LPAREN expression RPAREN statement { For(Value(Void),$3,Value(Void),$5,get_env()) }
    | CONTINUE SEMICOLON                      { Continue(get_env())}
    | BREAK SEMICOLON                         { Break(get_env())}
    | RETURN opt_expression SEMICOLON         { Return($2,get_env()) }
    | IMPORT STRING SEMICOLON                 { Import(resolve_import($2,$1,get_env()),get_env()) }
    | TEMPLATE ID LBRACE template_specs RBRACE { TemplateDef($2, $4,get_env()) }
    | INSTRUCTIONS FOR ID LPAREN arglist RPAREN LBRACE instruction_specs RBRACE
                                              { Instructions($3,$5,$8,get_env()) }
    | SWITCH LPAREN expression RPAREN LBRACE switch_statements RBRACE
                                              { Switch($3,$6, get_env()) }      
    | FOR LPAREN opt_expression SEMICOLON 
                 opt_expression SEMICOLON 
                 opt_expression RPAREN statement { For($3,$5,$7,$9,get_env()) }     
    | TRY statement_block CATCH LPAREN ID RPAREN statement_block { TryCatch($2,$5,$7, get_env()) }
    | TRY statement_block FINALLY statement_block { TryFinally($2,$4,get_env()) }                       
    | THROW expression SEMICOLON              { Throw($2, get_env()) }
;
switch_statement:
    | CASE expression COLON                   { Case(Some $2,get_env()) }
    | DEFAULT COLON                           { Case(None,get_env()) }
    | statement                               { $1 }
;
switch_statements:
    | switch_statement                        { [$1] }
    | switch_statement switch_statements      { $1::$2 }
;
opt_expression:
    | expression                              { $1 }
    | empty_expression                        { Value(Void) }
;
empty_expression:
    | /*nothing */                            { Value(Void) }
;
atom_expr:
    | INT                                     { Value(IntegerValue($1)) }
    | REAL                                    { Value(FloatValue($1)) }
    | STRING                                  { Value(StringValue($1)) }
    | BOOLEAN                                 { Value(BooleanValue($1)) }
    | VOID                                    { Value(Void) }
    | NAN                                     { Value(NaN) }
    | LBRACKET expr_list RBRACKET             { ArrayExpr($2) }
    | LBRACE prop_list RBRACE                 { MapExpr($2) }
    | ID                                      { Id($1) }
    | LPAREN expression RPAREN                { $2 }
;
member_expr:
    | atom_expr                               {$1}
    | FUNCTION LPAREN arglist RPAREN statement_block { Value(FunctionValue($3,extract_stmt_list($5))) }
    | member_expr  LBRACKET expression RBRACKET   { MemberExpr($1,IndexExpr($3)) }
    | member_expr DOT ID                      { MemberExpr($1,Id($3)) }
;
call_expr:
    | member_expr LPAREN fexpr_list RPAREN    { FunctionCall($1,$3) }
    | call_expr LPAREN fexpr_list RPAREN      { FunctionCall($1,$3) }
    | call_expr LBRACKET expression RBRACKET  { MemberExpr($1,IndexExpr($3)) }
    | call_expr DOT ID                        { MemberExpr($1,Id($3)) }
; 
lhs_expr:
    | member_expr                             {$1}
    | call_expr                               {$1}
;
unary_expr:
    | lhs_expr                                { $1 }
    | %prec PREFIX_INCDEC PLUSPLUS lhs_expr   { Assignment($2,BinaryOp($2,Plus,Value(IntegerValue(1)))) }
    | %prec PREFIX_INCDEC MINUSMINUS lhs_expr { Assignment($2,BinaryOp($2,Minus,Value(IntegerValue(1)))) }
    | %prec POSTFIX_INCDEC lhs_expr PLUSPLUS  { PostFixSum($1,1) }
    | %prec POSTFIX_INCDEC lhs_expr MINUSMINUS { PostFixSum($1,-1) }
;
op_expr:
    | unary_expr                              {$1}
    | op_expr PLUS op_expr              { BinaryOp($1,Plus,$3) }
    | op_expr MINUS op_expr             { BinaryOp($1,Minus,$3) }
    | op_expr TIMES op_expr             { BinaryOp($1,Times,$3) }
    | op_expr DIVIDE op_expr            { BinaryOp($1,Divide,$3) }
    | op_expr MODULO op_expr            { BinaryOp($1,Modulo,$3) }
    | op_expr COMPOP op_expr            { CompOp($1,$2,$3) }
    | NOT call_expr                     { Not($2) }
    | op_expr  AND op_expr              { BinaryOp($1,And,$3) }
    | op_expr OR op_expr                { BinaryOp($1,Or,$3) }
    | %prec UMINUS MINUS op_expr        { BinaryOp(Value(IntegerValue(0)),Minus,$2) }
;
cond_expr:
    | op_expr {$1}
    | expression QUESTION expression COLON expression
                                        {FunctionCall(Value(FunctionValue([],[
                                           If($1,Return($3,get_env()),Return($5,get_env()),get_env()) ])),[]) }                                                                                                   
;
expression:
    | cond_expr                               {$1}
    | lhs_expr EQUALS expression              { Assignment($1,$3) } 
    | lhs_expr EQUALS empty_statement_block   { Assignment($1,MapExpr([])) }
    | VAR lhs_expr EQUALS expression          { Declaration($2,$4) }
    | VAR lhs_expr EQUALS empty_statement_block { Declaration($2,MapExpr([])) }
    | lhs_expr TIMESEQUALS expression         { Assignment($1,(BinaryOp($1,Times,$3))) }
    | lhs_expr MODEQUALS expression           { Assignment($1,(BinaryOp($1,Modulo,$3))) } 
    | lhs_expr DIVEQUALS expression           { Assignment($1,(BinaryOp($1,Divide,$3))) } 
    | lhs_expr PLUSEQUALS expression          { Assignment($1,(BinaryOp($1,Plus,$3))) } 
    | lhs_expr MINUSEQUALS expression         { Assignment($1,(BinaryOp($1,Minus,$3))) } 
;
arglist:                              
    | ID                                      { [$1] }
    | ID DOTDOTDOT                            { ["["^$1] }
    | ID COMMA arglist                        { $1::$3 }
    | /* nothing */                           { [] }
;
expr_list:
    | expression                              { [$1] }
    | empty_statement_block                   { [MapExpr([])] }
    | expression COMMA expr_list              { $1::$3 }
    | /*nothing*/                             { [] }
;
fexpr:
    | expression                              { $1 }
    | empty_statement_block                   { MapExpr([]) }
    | AT ID                                   { UnboundVar($2) }
    | AT ID DOTDOTDOT                         { UnboundVar("["^$2) }
;
fexpr_list:
    | fexpr                                   { [$1] }
    | fexpr COMMA fexpr_list                  { $1::$3 }
    | /*nothing*/                             { [] }
;
property:                             
    | ID COLON expression                     { ($1,$3) }
    | ID COLON empty_statement_block          { ($1,MapExpr([])) }
;
prop_list:                            
    | property                                { [$1] }
    | property COMMA prop_list                { $1::$3 }
;
template_spec:                        
    | label TEXT                              { (Some $1,$2) }
    | TEXT                                    { (None, $1) }
;
template_specs:                       
    | template_spec                           { [$1] }
    | template_spec template_specs            { $1::$2 }
;   
instruction_spec:                     
    | label repl_condition COLON replacement_list SEMICOLON { ($1,$2,$4) }
    | label repl_condition SEMICOLON          { ($1,$2,[]) }
;
repl_condition:                       
    | ONCE                                    { Once }
    | WHEN LPAREN expression RPAREN           { When($3) }
    | FOREACH LPAREN ID IN expression RPAREN  { Loop($3,$5) }
    | FOREACH LPAREN ID IN expression RPAREN WHEN LPAREN expression RPAREN
                                              { CondLoop($9,$3,$5) }  
;
replacement:                          
    | ID EQUALS expression                    { ($1,$3) }
;
replacement_list:                     
    | replacement                             { [$1] }
    | replacement COMMA replacement_list      { $1::$3 }
;
instruction_specs:                    
    | instruction_spec                        { [$1] }
    | instruction_spec instruction_specs      { $1::$2 }
; 
label:
    | ID                                      { $1 }
    | INT                                     { string_of_int($1) }
;
%%
