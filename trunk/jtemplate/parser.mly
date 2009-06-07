%{
	
open Ast

exception ParseException of string

let  parse_error s =
	let pos=Parsing.symbol_start_pos() in
	print_string ("in file " ^ pos.Lexing.pos_fname ^ ": "^ s^" at line ");
	print_int pos.Lexing.pos_lnum;
	print_string " at columns ";
	print_int (Parsing.symbol_start()-pos.Lexing.pos_bol);
	print_string("-");
	print_int (Parsing.symbol_end()-pos.Lexing.pos_bol);
	print_string "\n";
  flush stdout

let get_env = function
	() -> let pos=Parsing.symbol_start_pos() in
		(pos.Lexing.pos_fname,pos.Lexing.pos_lnum)
		
%}

%token<string> ID
%token <int> INT
%token <string> STRING
%token <float> REAL
%token <bool> BOOLEAN
%token <string> TEXT

%token FOREACH
%token WHILE
%token IF
%token FOR
%token ELSE
%token TEMPLATE
%token INSTRUCTIONS
%token FUNCTION
%token CONTINUE
%token BREAK
%token RETURN
%token IN
%token ONCE
%token WHEN
%token VAR

%token EOF
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token SEMICOLON
%token COLON
%token DOT

%token EQUALS
%token NOT
%token QUESTION

%token <Ast.comparator> COMPOP
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MODULO

%token AND
%token OR

%start program
%type <Ast.statement list> program

%left COMPOP
%left PLUS MINUS
%left TIMES DIVIDE MODULO


%%
program:                              statements EOF                          { $1 }
                                    | EOF                                     { [] }
;
statements:                           statement                               { [$1] }
                                    | statement statements                    { $1::$2 }
                                    | statement_block                         { $1 } 
;
statement_block:                      LBRACE statements RBRACE                { $2 } 
                                    | LBRACE RBRACE                           { [] }
;
/* for_target_statements can appear inside a for() statements */
for_target_statement:                 variable EQUALS expression              { Assignment($1,$3) }
                                    | VAR variable EQUALS expression          { Declaration($2,$4) }
                                    | expression                              { ExpressionStatement($1) }
                                    | /*nothing*/                             { Noop }
;
statement:                            for_target_statement SEMICOLON          { $1 }                            
                                    | CONTINUE SEMICOLON                      { Continue }
                                    | BREAK SEMICOLON                         { Break }
                                    | RETURN expression SEMICOLON             { Return($2) }
																		| RETURN SEMICOLON                        { Return(Value(Void)) }
                                    | FOREACH LPAREN ID IN expression RPAREN statement_block
                                                                              { ForEach(Name($3),$5,$7) }
                                    | WHILE LPAREN statement RPAREN statement_block 
                                                                              { For(Noop,$3,Noop,$5) }
                                    | FOR LPAREN for_target_statement SEMICOLON 
                                                 for_target_statement SEMICOLON 
                                                 for_target_statement RPAREN statement_block
                                                                              { For($3,$5,$7,$9) }
                                    | IF LPAREN expression RPAREN statement_block ELSE statement_block
                                                                              { If($3,$5,$7) }
                                    | IF LPAREN expression RPAREN statement_block
                                                                              { If($3,$5,[]) }
                                    | TEMPLATE ID LBRACE template_specs RBRACE
                                                                              { TemplateDef(Name($2), $4) }
                                    | INSTRUCTIONS FOR ID LPAREN arglist RPAREN LBRACE instruction_specs RBRACE
                                                                              { Instructions(Name($3),$5,$8) }
;
expression:                           value                                   { $1 }
																		| function_call                           { $1 }
																		| LPAREN expression RPAREN                { $2 }
																		| expression PLUS expression              { BinaryOp($1,Plus,$3) }
																		| expression MINUS expression             { BinaryOp($1,Minus,$3) }
																		| expression TIMES expression             { BinaryOp($1,Times,$3) }
																		| expression DIVIDE expression            { BinaryOp($1,Divide,$3) }
																		| expression MODULO expression            { BinaryOp($1,Modulo,$3) }
																		| expression COMPOP expression            { CompOp($1,$2,$3) }
;
function_call:                        variable LPAREN expr_list RPAREN        { FunctionCall($1,$3) }
;
variable:                             ids                                     {
																																								 match $1 with
																																									  id :: [] -> Name(id)
																																									| _ -> CompoundName($1)
																																							}
; 
ids:                                  ID                                      { [$1] (*TODO array ref*)}
                                    | ID DOT ids                              { $1::$3 }
;
value:                                INT                                     { Value(IntegerValue($1)) }
																		| REAL                                    { Value(FloatValue($1)) }
																		| STRING                                  { Value(StringValue($1)) }
																		| BOOLEAN                                 { Value(BooleanValue($1)) }
																		| FUNCTION LPAREN arglist RPAREN statement_block 
																		                               						{ Value(FunctionValue($3,$5,None)) }
																		| FUNCTION LPAREN RPAREN statement_block	{ Value(FunctionValue([],$4,None)) }
																	  | LBRACKET RBRACKET                       { ArrayExpr([]) }
																		| LBRACKET expr_list RBRACKET             { ArrayExpr($2) }
																		| LBRACE RBRACE                           { MapExpr([]) }
																		| LBRACE prop_list RBRACE                 { MapExpr($2) }
																		| variable                                { VariableExpr($1) }
;
arglist:                              ID                                      { [Name($1)] }
																		| ID COMMA arglist                        { Name($1)::$3 }
;
expr_list:
                                      expression                              { [$1] }
																	  | expression COMMA expr_list              { $1::$3 }
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