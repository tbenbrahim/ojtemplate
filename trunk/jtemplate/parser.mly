%{
	
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
	| [] -> []
	| [StatementBlock(lst)] -> lst
	| lst -> lst
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
%token DIVIDE MODULO AND OR VOID NAN SWITCH CASE DEFAULT



%start program
%type <Ast.statement> program

%left AND OR
%left COMPOP
%left PLUS MINUS
%left TIMES DIVIDE MODULO


%%
program:                              statements EOF                          { StatementBlock($1) }
                                    | EOF                                     { StatementBlock([]) }
;
statements:                           statement                               { [$1] }
                                    | statement statements                    { $1::$2 }
                                    | statement_block                         { $1 } 
;
statement_block:                      LBRACE statements RBRACE                { [StatementBlock($2)] } 
                                    | LBRACE RBRACE                           { [] }
;
/* for_target_statements can appear inside a for() statements */
for_target_statement:                 variable EQUALS expression              { Assignment($1,$3,get_env()) }
                                    | VAR variable EQUALS expression          { Declaration($2,$4,get_env()) }
                                    | expression                              { ExpressionStatement($1,get_env()) }
                                    | /*nothing*/                             { Noop }
;
statement:                            for_target_statement SEMICOLON          { $1 }          
                                    | CASE expression COLON                   { Case(Some $2,get_env()) }
                                    | DEFAULT COLON                           { Case(None,get_env()) }
                                    | CONTINUE SEMICOLON                      { Continue(get_env())}
                                    | BREAK SEMICOLON                         { Break(get_env())}
                                    | RETURN expression SEMICOLON             { Return($2,get_env()) }
																		| RETURN SEMICOLON                        { Return(Value(Void),get_env()) }
																		| SWITCH LPAREN expression RPAREN statement_block
																		                                          { Switch($3,extract_stmt_list($5),get_env()) }
                                    | FOREACH LPAREN ID IN expression RPAREN statement_block
                                                                              { ForEach(Name($3),$5,extract_stmt_list($7),get_env()) }
                                    | WHILE LPAREN expression RPAREN statement_block 
                                                                              { For(Noop,$3,Noop,extract_stmt_list($5),get_env()) }
                                    | FOR LPAREN for_target_statement SEMICOLON 
                                                 expression SEMICOLON 
                                                 for_target_statement RPAREN statement_block
                                                                              { For($3,$5,$7,extract_stmt_list($9),get_env()) }
                                    | IF LPAREN expression RPAREN statement_block ELSE statement_block
                                                                              { If($3,extract_stmt_list($5),extract_stmt_list($7),get_env()) }
                                    | IF LPAREN expression RPAREN statement_block
                                                                              { If($3,extract_stmt_list($5),[],get_env()) }
                                    | TEMPLATE ID LBRACE template_specs RBRACE
                                                                              { TemplateDef(Name($2), $4,get_env()) }
                                    | INSTRUCTIONS FOR ID LPAREN arglist RPAREN LBRACE instruction_specs RBRACE
                                                                              { Instructions(Name($3),$5,$8,get_env()) }
																		| IMPORT STRING                           { Import(resolve_import($2,$1,get_env()),get_env()) }
;
expression:                           value                                   { $1 }
																		| function_call                           { $1 }
																		| unbound_variable                        { $1 }
																		| LPAREN expression RPAREN                { $2 }
																		| expression PLUS expression              { BinaryOp($1,Plus,$3) }
																		| expression MINUS expression             { BinaryOp($1,Minus,$3) }
																		| expression TIMES expression             { BinaryOp($1,Times,$3) }
																		| expression DIVIDE expression            { BinaryOp($1,Divide,$3) }
																		| expression MODULO expression            { BinaryOp($1,Modulo,$3) }
                                    | expression AND expression               { BinaryOp($1,And,$3) }
                                    | expression OR expression                { BinaryOp($1,Or,$3) }
																		| expression COMPOP expression            { CompOp($1,$2,$3) }
;
function_call:                        variable LPAREN expr_list RPAREN        { FunctionCall($1,$3) }
                                    | function_def LPAREN expr_list RPAREN    { DirectFunctionCall($1,$3) }
;
variable:                             ids                                     {
																																								 match $1 with
																																									  Name(id) :: [] -> Name(id)
																																									| _ -> EvaluatedName($1)
																																							}
;
unbound_variable:                     ID QUESTION                             { UnboundVar(Name($1)) }
;
array_index:                          ID LBRACKET expression RBRACKET         { ArrayIndex($1,$3) }
;
ids:                                  ID                                      { [Name($1)] (*TODO array ref*)}
                                    | array_index                             { [$1] }
                                    | ID DOT ids                              { Name($1)::$3 }
;
function_def:
                                      FUNCTION LPAREN arglist RPAREN statement_block 
                                                                              { Value(FunctionValue($3,$5)) }
;																																							
value:                                INT                                     { Value(IntegerValue($1)) }
																		| REAL                                    { Value(FloatValue($1)) }
																		| STRING                                  { Value(StringValue($1)) }
																		| BOOLEAN                                 { Value(BooleanValue($1)) }
																		| VOID                                    { Value(Void) }
																		| NAN                                     { Value(NaN) }
																		| function_def                            { $1 }
																		| LBRACKET expr_list RBRACKET             { ArrayExpr($2) }
																		| LBRACE RBRACE                           { MapExpr([]) }
																		| LBRACE prop_list RBRACE                 { MapExpr($2) }
																		| variable                                { VariableExpr($1) }
;
arglist:                              ID                                      { [Name($1)] }
                                    | ID DOTDOTDOT                            { [Name("["^$1)] }
																		| ID COMMA arglist                        { Name($1)::$3 }
																		| /* nothing */                           { [] }
;
expr_list:
                                      expression                              { [$1] }
																	  | expression COMMA expr_list              { $1::$3 }
																		| /*nothing*/                             { [] }
;
label:
                                      ID                                      { Label($1) }
																		| INT                                     { Label(string_of_int($1)) }
;
template_spec:                        label TEXT                              { (Some $1,$2) }
                                    | TEXT                                    { (None, $1) }
;
template_specs:                       template_spec                           { [$1] }
                                    | template_spec template_specs            { $1::$2 }
;   
instruction_spec:                     label repl_condition COLON replacement_list SEMICOLON 
                     																													{ ($1,$2,$4) }
																		| label repl_condition SEMICOLON          { ($1,$2,[]) }
;
repl_condition:                       ONCE                                    { Once }
                          					| WHEN LPAREN expression RPAREN           { When($3) }
																		| FOREACH LPAREN ID IN expression RPAREN  { Loop(Name($3),$5) }
																		| FOREACH LPAREN ID IN expression RPAREN WHEN LPAREN expression RPAREN
																		                                          { CondLoop($9,Name($3),$5) }  
;
replacement:                          ID EQUALS expression                    { ($1,$3) }
;
replacement_list:                     replacement                             { [$1] }
                                    | replacement COMMA replacement_list      { $1::$3 }
;
instruction_specs:                    instruction_spec                        { [$1] }
                                    | instruction_spec instruction_specs      { $1::$2 }
; 
property:                             ID COLON expression                     { ($1,$3) }
;
prop_list:                            property                                { [$1] }
                                    | property COMMA prop_list                { $1::$3 }
;																																							

%%
